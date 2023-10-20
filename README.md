## desfert-prs-processing

This repository contains the R-based workflow for processing PRS probe raw data, and inserting those processed data into the CAP LTER DesFert database. The workflow is outlined in `prs_processing.R`, which calls the supporting tools and functions.


### processing notes (if relevant)

#### fall 2022

- fixed error in data formatting step that was labeling blank values as having the location under plant (also fixed in the database errors from previous uploads)
- moved workflow from .Rmd to .R 
- added additional checks to ensure data integrity:
  + ensure that all WesternAg Wal ID numbers in the input are included in the output (i.e., that data were not inappropriately filtered or otherwise excluded)
  + location field includes only values: "under plant", "between plant", "BLANK"

#### fall 2021

Added a check to ensure reasonable range of deployment dates.

#### fall 2021 (should be an earlier campaign)

PRS data from fall 2021. These data include the full-suite of cation and anion analyses (i.e., are not limited to nitrogen species). Lab blanks were provided as 81A, 82A, and 83A; these values were edited in the original data file to 76, 77, and 78.

#### summer 2020

PRS data from summer 2020. These data include the full-suite of cation and anion analyses (i.e., are not limited to nitrogen species).
