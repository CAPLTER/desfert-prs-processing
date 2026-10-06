## desfert-prs-processing

R-based, non-interactive workflow for processing PRS probe data from WesternAg
and inserting those data into the CAP LTER DesFert database
(`urbancndep.prs_analysis`).

### usage

```
./prs_load.sh -i "Nutrient Supply Rate Data_ Project 2593_ May Quincy Stewart.xlsx" --user USER --dry-run
./prs_load.sh -i FILE --user USER [--host localhost] [--dbname caplter] [--port 5432]
```

| option          | description                                                                       | default     |
|-----------------|-----------------------------------------------------------------------------------|-------------|
| `-i, --input`   | WesternAg Excel file (required)                                                   |             |
| `-n, --dry-run` | run the full workflow, including the insert, then roll back so nothing is written | off         |
| `--host`        | database host                                                                     | `localhost` |
| `--dbname`      | database                                                                          | `caplter`   |
| `--user`        | database user (required)                                                          |             |
| `--port`        | database port                                                                     | `5432`      |

The schema (`urbancndep`) is fixed. The password is never passed as an argument;
set `PGPASSWORD` or use `~/.pgpass`. Progress and a summary are written to
stdout; errors are written to stderr and the exit status is non-zero, so
failures are visible in the terminal and detectable by calling scripts. Always
do a `--dry-run` first.

Requires R (>= 4.1) with `readxl`, `dplyr`, `tidyr`, `lubridate`, `tibble`,
`DBI`, and `RPostgres` (and `testthat` and `withr` for tests).

### how it works

- `prs_load.sh`: bash wrapper that parses arguments and calls `prs_cli.R`.
- `prs_cli.R`: R entry point; sources the helpers and calls `run_prs_load()`.
- `helper_read_data.R`: locates the header row (the row with `WAL #`), checks
  the fixed columns (`WAL #`, `Sample ID`, `Burial Date`, `Retrieval Date`, `#
  Anion`, `# Cation`, `Notes`), treats all following columns as analytes (the
  suite of analytes measured differs among campaigns by design), and separates
  the `Method Detection Limits` row.
- `helper_format_data.R`: converts to the long structure of `prs_analysis`.
  Renames `Total N` to `Total-N`; flags values <= the detection limit reported
  in the file; derives plot and location from `Sample ID` (A = under plant, B =
  between plant; plots > 75 are BLANKs). Stops on: unrecognized analytes (known
  list plus any analyte already in the database), malformed or duplicated sample
  or WAL IDs, unparseable dates, deployments not between 0 and 180 days, or
  samples lost in formatting.
- `helper_db.R`: connection, plot-ID check against `urbancndep.plots`, and a
  single-transaction insert. Dates and integers are cast in R, so no temporary
  table is needed (previously `dbWriteTable` created time-zone-aware dates that
  had to be altered to `date`, and WAL IDs to integer).
- Re-loading data already in the database is refused by the unique constraint on
  `(wal_id, analyte)`; the tool reports the offending WAL IDs and writes
  nothing.

Tests: `Rscript -e 'testthat::test_dir("tests/testthat")'`. The format tests use
any `*.xlsx` examples in the repo root (not tracked in git); the database test
re-inserts each example file within a transaction that is always rolled back.

If WesternAg changes column names or layout in a future campaign, the reader
will stop with a message identifying the problem; adjust `helper_read_data.R`
(and, for a new analyte, `known_analytes` in `helper_format_data.R`)
accordingly.

### processing notes (if relevant)

#### fall 2022

- fixed error in data formatting step that was labeling blank values as having
  the location under plant (also fixed in the database errors from previous
  uploads)
- moved workflow from .Rmd to .R 
- added additional checks to ensure data integrity:
  + ensure that all WesternAg Wal ID numbers in the input are included in the
    output (i.e., that data were not inappropriately filtered or otherwise
    excluded)
  + location field includes only values: "under plant", "between plant", "BLANK"

#### fall 2021

Added a check to ensure reasonable range of deployment dates.

#### fall 2021 (should be an earlier campaign)

PRS data from fall 2021. These data include the full-suite of cation and anion
analyses (i.e., are not limited to nitrogen species). Lab blanks were provided
as 81A, 82A, and 83A; these values were edited in the original data file to 76,
77, and 78.

#### summer 2020

PRS data from summer 2020. These data include the full-suite of cation and anion
analyses (i.e., are not limited to nitrogen species).

#### 2026 (and workflow change)

- the workflow is now run from the command line (`prs_load.sh`); the interactive
  scripts and the Rmd template were retired (the Rmd is preserved in `archive/`)
- detection limits are read from the input file rather than hardcoded
- the five example files from 2023-2026 all have the same layout and analytes
  and reproduce the data already in the database
