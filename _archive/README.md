# Original data files and instructions

This folder (ignored when building the R package), contains older versions of data used for V1 of the app, as well as the original .py 

- `README_orig.md` contains the original instructions
- `data-prep.R` contains the R script for processing the `Activities_pressures_cross_check_forContractor (003).xlsx` file into `act2Pres.rds`
- `parse-visio.py` contains the script used to process the visio diagrams (`AmalgamatedPoE_23-Jan-2024_Update.vsdx`)
- `collect-text.R` contains the script used to create the original French-English dictionary (`en-fr_table.csv`)
- `create_customizable_data.R` contains the script used to create the current Excel data files from the original `act2Pres.rds` and `poe.json` files (this should no longer be required)

For V1 of the Pathways of Effect shiny app, see [commit 0494405
](https://github.com/marleyaikens/CWS-Pathway-of-Effects/commit/049440567c6e3664656f75326953e1f3ff68256d)