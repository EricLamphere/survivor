# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A Shiny web application (structured as an R package) for managing a Survivor TV show betting pool. It reads data from Google Sheets and displays standings, winnings, and historical records. Live app: https://ericlamphere.shinyapps.io/survivor/

## Commands

```r
# Run locally (in R console)
survivor::survivor_app()
# OR: Rscript app.R

# Restore dependencies
renv::restore()

# Regenerate documentation
devtools::document()

# Reload package during development
pkgload::load_all()
```

**Deployment** is via GitHub Actions (`.github/workflows/`):
- Push to `main` → auto-deploys to dev (`survivor_dev` on shinyapps.io)
- Manual trigger → production (`survivor` on shinyapps.io)

## Architecture

### Data Flow

1. `app.R` → authenticates with Google Sheets via `gs_auth()`, fetches all data, then calls `survivor_app()`
2. `R/data_init.R` → core data pipeline: fetches 4 Google Sheet tabs, joins and transforms them into `season_picks` and `season_participants` data frames used globally
3. `R/server.R` → reactive bindings that filter/display the global data frames based on sidebar inputs
4. `R/ui.R` → dashboard layout with 3 tabs: Welcome, Standings, Winners Club

### Key Files

- `R/data_init.R` — Google Sheets auth, data fetching, all scoring/winnings calculation logic
- `R/server.R` — Shiny server (reactive outputs, refresh button handler)
- `R/ui.R` — UI layout (shinydashboard with bslib "darkly" theme)
- `R/seasons.R` — Season utility functions
- `R/winners.R` — Winners Club display logic
- `R/formatters.R` — DT table rendering with conditional formatting

### Scoring Logic

- **Winner** = participant whose castaway survived the most days
- **Payment** = `cost_per_day * (max_castaway_day - your_castaway_day)` — losers pay winners
- **Tie-breaker** = picking order (lower = better, i.e., drafted first)
- **Sole survivor bonus** = `+$5 per loser` if your pick wins the whole season
- Picking order for new seasons is auto-calculated (reverse of prior season standings)

### Google Sheets Auth

`gs_auth()` tries three methods in order:
1. `.secrets/gcp-service-account.json` (local file, gitignored)
2. Cached OAuth token
3. `GCP_SERVICE_ACCOUNT_KEY_BASE64` environment variable (used in CI/CD)

### Global State

Three variables are initialized in `app.R` and used throughout:
- `season_picks` — main data frame: all picks with rankings and payment amounts
- `season_participants` — participant-level data by season
- `counter` — reactive value tracking refresh button clicks

### Historical Data

Pre-pool seasons are stored as an internal R dataset (`data/historical_castaways.rda`) and merged in via `add_historical_data()` in `data_init.R`. Raw processing script is in `data-raw/`.
