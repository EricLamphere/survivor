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

Naming rules applied when cleaning the nickname:
- Lowercase everything
- Remove periods and apostrophes
- Remove hyphens
- Spaces → underscores (primary); spaces removed (fallback, e.g. `caoboi`)

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
Normalize any non-breaking spaces (`\u00A0` → ` `) that can come through from
Google Sheets.

**2. Derive the show nickname**

`castaway_name` is the full name (e.g. `"Yam Yam Arocho"`). The wiki uses
the show nickname (`"yam_yam"`). The nickname is resolved in priority order:

1. **Quoted nickname in the name** — e.g. `'Quintavius "Q" Burdette'`
   extracts `"Q"` directly.
2. **survivoR package lookup** — `survivoR::castaways` is joined by
   `(version == "US", season, full_name)` to get the `castaway` field
   (e.g. `"Yam Yam Arocho"` → `"Yam Yam"`). The map also indexes
   stripped versions of names that contain a quoted nickname in survivoR.
3. **First word fallback** — if no match is found, uses the first word of
   the full name.

**3. Generate filename candidates**

For each castaway, up to four candidates are generated:
- Nickname, spaces → underscores: `S44_yam_yam_t.png`
- Nickname, spaces removed: `S44_yamyam_t.png`
- If the real first name differs from the nickname, also:
  - Real first name, spaces → underscores: `S40_rob_t.png`
  - Real first name, spaces removed (if applicable)

This handles cases like `"Boston Rob"` (nickname) → wiki uses `S40_rob_t.png`
(real first name).

**4. Build a reverse lookup**

Every candidate filename (normalized to underscores) is stored in `name_map`
pointing back to the original `castaway_name`. This is used later to
re-associate API results with the right castaway.

**5. Batch API call**

All candidate filenames for the season are sent to the API in one request
(chunked at 40 titles if needed to stay under the 50-title limit).

**6. First valid match wins**

The response is iterated. For each file that exists (has `imageinfo`):
- The returned title (MediaWiki normalizes underscores → spaces) is converted
  back to underscores.
- `name_map` maps the filename back to the `castaway_name`.
- If no URL has been stored yet for that castaway, the URL is saved.

Candidates are added in priority order (nickname first, real first name
second), so the preferred filename wins naturally when multiple candidates
exist for one castaway.

**7. Result**

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
