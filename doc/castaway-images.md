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

No external package data is needed. For each castaway, candidates are
generated in priority order:

1. **Quoted nickname** — if the name contains double-quoted text (e.g.
   `Quintavius "Q" Burdette`), the quoted portion is extracted and lowercased
   (`q`), producing `File:S47_q_t.png`. This takes highest priority.
2. **First-N-words combinations** (N = 1, 2, 3) — the cleaned name is split
   into words, then the first N words are joined with underscores and also
   concatenated without spaces:
   - "Yam Yam Arocho" → `yam`, `yam_yam`, `yamyam`, `yam_yam_arocho`, …
   - "Cao Boi Bui" → `cao`, `cao_boi`, `caoboi`, …
3. **Each individual word** — every word of the cleaned name is tried as a
   standalone slug. This catches cases where the wiki uses the real first name
   instead of a show nickname (e.g. "Boston Rob Mariano" → `rob`).

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

- **Standings table** — each row's `Castaway` cell contains an `<img>` tag
  (35px tall) to the left of the castaway's name. Color styling (green for
  sole survivor, red for eliminated) is embedded inline in the HTML since
  formattable escapes content when wrapping values in span tags.
- **Sole survivor infoBox** — the winner's headshot appears to the left of
  their name in the box value via `shiny::tagList()`.
