# Castaway Image URLs

## Overview

Castaway thumbnail images are fetched dynamically at runtime from the
[Survivor Fandom wiki](https://survivor.fandom.com) using its MediaWiki API.
No images are downloaded or committed to the repo — the app references CDN
URLs directly.

## The MediaWiki imageinfo API

Any MediaWiki-powered wiki exposes a read API at `<wiki>/api.php`. The
`imageinfo` action returns metadata about uploaded files, including the CDN
URL of the actual image.

**Request format:**

```
GET https://survivor.fandom.com/api.php
  ?action=query
  &prop=imageinfo
  &iiprop=url
  &titles=File:S44_carolyn_t.png|File:S44_yam_yam_t.png|...
  &format=json
```

Up to 50 pipe-separated file titles can be sent in one request. The response
is a JSON object whose `query.pages` map contains one entry per file. Files
that exist have an `imageinfo` array with the CDN URL; files that don't exist
have a `"missing"` flag and no `imageinfo`.

**The HTTP request is made by `jsonlite::fromJSON(url)`** — `jsonlite`
accepts a URL directly, fetching and parsing the JSON response in one call
with no explicit `httr` or `curl` needed.

## Filename convention

Wiki filenames follow the pattern `S{N}_{name}_t.png`, where `{name}` is the
castaway's show nickname in lowercase. Examples:

| Castaway | Filename |
|---|---|
| Carolyn Wiger (S44) | `S44_carolyn_t.png` |
| Yam Yam Arocho (S44) | `S44_yam_yam_t.png` |
| Rob Mariano (S40) | `S40_rob_t.png` |
| J'Tia Taylor (S28) | `S28_jtia_t.png` |

Naming rules the wiki applies when deriving the slug from a nickname:
- Lowercase everything
- Remove periods and apostrophes
- Remove hyphens
- Spaces → underscores (e.g. `yam_yam`); sometimes spaces removed entirely
  (e.g. `caoboi` for "Cao Boi")

## How the code works

### Entry points

- **`get_castaway_image_urls(szn)`** — public, cached. Returns a named list
  of `castaway_name → CDN URL` for a season. Checks a package-level
  environment cache (`.castaway_cache`) first; calls
  `fetch_castaway_image_urls()` on a cache miss and stores the result.
- **`fetch_castaway_image_urls(szn)`** — does the actual work described below.
- **`.fandom_imageinfo(api_base, file_titles)`** — shared helper used by both
  castaway images and season logos. Builds the API URL, calls
  `jsonlite::fromJSON`, and returns the parsed response (or `NULL` on error).

### Step-by-step

**1. Get castaway names**

Pull all unique `castaway_name` values for the season from `season_picks`.
Non-breaking spaces (`\u00A0`) that can come through from Google Sheets are
normalized per-name during candidate generation (not at the vector level) so
that the original name string is preserved as the URL map key — callers look
up URLs using the same raw name from `season_picks`.

**2. Generate filename candidates**

For each castaway, candidates are generated in priority order:

**Priority 0: Known nickname mapping (YAML configuration)**
- Checks `inst/extdata/castaway_nicknames.yaml` for a predefined nickname
- Supports both default nicknames and season-specific overrides
- Example: "Oscar Lusth" → `ozzy` (all seasons), "Rob Mariano" in S4 → `rob`, in S8+ → `boston_rob`
- Also tries compound nicknames without underscores (e.g., `boston_rob` → `bostonrob`)
- **If a nickname is found, ALL other candidate generation is skipped** - the YAML mapping is authoritative
- This prevents conflicts (e.g., both "James Clement" and "James Thomas Jr." trying to claim `S20_james_t.png`)
- See the [Nickname Mapping System](#nickname-mapping-system) section below for details

**Priority 1: Quoted nickname**
- If the name contains double-quoted text (e.g. `Quintavius "Q" Burdette`), the quoted portion is extracted and lowercased (`q`), producing `File:S47_q_t.png`

**Priority 2: First-N-words combinations (N = 1, 2, 3)**
- The cleaned name is split into words, then the first N words are joined with underscores and also concatenated without spaces:
  - "Yam Yam Arocho" → `yam`, `yam_yam`, `yamyam`, `yam_yam_arocho`, …
  - "Cao Boi Bui" → `cao`, `cao_boi`, `caoboi`, …

**Priority 3: Each individual word**
- Every word of the cleaned name is tried as a standalone slug. This catches cases where the wiki uses the real first name instead of a show nickname (e.g. "Boston Rob Mariano" → `rob`)

**Priority 4: First name + last initial**
- Tries combinations like `kim_j` (Kim Johnson) vs `kim_p` (Kim Powers)
- Also tries without underscore: `kimj`, `kimp`
- Also tries last name only

Candidate cleaning mirrors the wiki's slug rules: lowercase, remove `.'`,
remove `-`, replace remaining non-alphanumeric characters with a space.

**3. Build a reverse lookup**

Every candidate filename (normalized to underscores) is stored in `name_map`
pointing back to the original `castaway_name`. This is used later to
re-associate API results with the right castaway.

**4. Batch API call**

All candidate filenames for the season are sent to the `imageinfo` API in one
request (chunked at 40 titles if needed to stay under the 50-title limit).

**5. First valid match wins**

The response is iterated. For each file that exists (has `imageinfo`):
- The returned title (MediaWiki normalizes underscores → spaces) is converted
  back to underscores.
- `name_map` maps the filename back to the original `castaway_name`.
- If no URL has been stored yet for that castaway, the URL is saved.

Candidates are added in priority order (quoted nickname first, then
first-N-words, then individual words), so the preferred filename wins
naturally when multiple candidates match one castaway.

**6. Result**

A named list of `castaway_name → CDN URL` is returned and cached. Castaways
with no matching file on the wiki are simply absent from the list, and the
app renders no image for them.

## Where images are displayed

- **Standings table (single season view)** — each row's `Castaway` cell contains an `<img>` tag
  (35px tall, with `loading="lazy"` for performance) to the left of the castaway's name. Color styling (green for
  sole survivor, red for eliminated) is embedded inline in the HTML since
  formattable escapes content when wrapping values in span tags.
- **Sole survivor infoBox** — the winner's headshot appears to the left of
  their name in the box value via `shiny::tagList()`.

### Performance Consideration: All Seasons View

**Images are NOT displayed when "All Seasons" is selected** for performance reasons:

- Fetching image URLs for all 50 seasons requires ~60 seconds of API calls
- The page would be unresponsive during this time, creating a poor user experience
- Users can select a specific season to view images for that season

The code explicitly skips image fetching when `szn == all_seasons_label()`:

```r
img_urls <- if (szn != all_seasons_label()) {
    get_castaway_image_urls(force_season_number(szn))
} else {
    list()  # Empty list - no images for All Seasons view
}
```

### Lazy Loading (Single Season View)

Images in single-season views use the native browser `loading="lazy"` attribute. This ensures:
- The table renders immediately without waiting for all images
- Images load progressively as they enter the viewport
- Better performance for seasons with many castaways

## Nickname Mapping System

### Overview

Many castaways are known by nicknames on the show, and the Fandom wiki uses these nicknames in image filenames. The mapping between database names (full legal names) and wiki nicknames is stored in a YAML configuration file.

**Configuration file**: `inst/extdata/castaway_nicknames.yaml`

**Functions**:
- `.load_nickname_mappings()` — loads YAML file once per session and caches result
- `.get_castaway_nickname(name, season)` — retrieves nickname with optional season-specific override

### YAML Structure

The YAML file supports two types of mappings:

**1. Default nickname (applies to all seasons)**
```yaml
Oscar Lusth:
  default: ozzy

Benjamin Wade:
  default: coach
```

**2. Season-specific overrides (for returning players)**
```yaml
Rob Mariano:
  4: rob           # Season 4: Marquesas
  8: boston_rob    # Season 8: All-Stars
  20: boston_rob   # Season 20: Heroes vs Villains
  22: boston_rob   # Season 22: Redemption Island
  40: boston_rob   # Season 40: Winners at War

James Clement:
  15: james        # Season 15: China
  16: jamesm       # Season 16: Micronesia (different image file!)
  20: james        # Season 20: Heroes vs Villains
```

### Why Season-Specific Mappings?

Returning players sometimes have different image files across seasons:
- **Rob Mariano**: In his first appearance (S4), images use `rob`. In later seasons, images use `boston_rob`
- **James Clement**: Uses `james` in S15 and S20, but `jamesm` in S16 (likely to differentiate from another James)
- **Jenna Lewis**: Uses `jenna` in S1, but `jenna_l` in S8 (to differentiate from Jenna Morasca)

### Adding New Nicknames

To add a new nickname mapping:

1. Edit `inst/extdata/castaway_nicknames.yaml`
2. Add the castaway's full name as it appears in the database
3. Specify either a `default` nickname or season-specific nicknames (or both)
4. Restart the app to load the new mappings

**Example for a simple case:**
```yaml
New Castaway Name:
  default: nickname
```

**Example for a returning player:**
```yaml
Returning Player Name:
  default: default_nickname  # Used if no season-specific match
  13: nickname_s13          # Override for season 13
  16: nickname_s16          # Override for season 16
```

### Benefits

- **Maintainable**: Non-developers can edit YAML without touching R code
- **Version controlled**: Track changes to nickname mappings over time
- **Self-documenting**: Comments in YAML explain each mapping
- **Performance**: Loaded once per session and cached
- **Accurate**: Season-specific overrides ensure correct images for returning players

### Current Coverage

As of the latest update:
- **Total castaways**: 917
- **Images found**: 913 (99.6%)
- **Images missing**: 4 (0.4%)

The 4 missing images are edge cases where the Fandom wiki doesn't have standard thumbnail images or uses non-standard naming that couldn't be determined programmatically.
