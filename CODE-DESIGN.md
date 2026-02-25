This file details code design choices of the second iteration of this tool (v.0.2.0)

## General Notes

- This project is not an R package, but we use a DESCRIPTION file and a R folder
  similar to in R packages to organize functions and track dependencies and versions.

## Run in terminal
- This runs in terminal while standardizing the port so you can use the browser refresh as you go
-  `R -q -e "shiny::runApp(port = 8080)"`

## Locations
- Functions to run this app can be found in the `R` folder.
- The top-level `app.R` file contains configuration instructions and launches the Shiny App.

## visNetwork quirks
- You can turn vis.js events into Shiny inputs using JS (this is how custom mitigations are added)
  - Events - https://visjs.github.io/vis-network/docs/network/#Events) 
  - Example - https://datastorm-open.github.io/visNetwork/shiny.html#build-your-own-input
- In order to revert the edge formating when adding/removing mitigations, 
  edge labels must start as " ". For what ever reason, `NA` and "" aren't updated when removing a mitigation edge label
    - possibly a bug in vis.js? https://github.com/visjs/vis-network/issues/1450

## Translations

Translations need to be filled out in the `translations.xlsx` file. This file
contains text that needs to be translated. It serves as both the dictionary used
by the app as well as a list of text potentially requiring translation (pre-filled with 'FR' as a placeholder).
This file is updated (without removing prior translations) automatically everytime 
the app runs. It pulls text from the data (i.e., `mititgations.xlsx`, `components.xlsx`, and `pathways.xlsx`) as well as from `ui_labels.xlsx`.

To ensure that translations are functioning, there are three files which need to be manually updated.

1. `ui_labels.xlsx` - This is where all non-data translatable text needs to be listed. This includes lables on the Shiny App UI as well as UI text from the reports.

2. `report_template.qmd` - Small UI labels from the reports should be listed in `ui_labels.xlsx`, but large blocks of text are best translated in the report itself.
These sections are identified by `if(lang == "en")... else ...` blocks of text.

3. `translations.xlsx` - This is the dictionary used by the app to translate text.
As the app detects any new terms which do not already have a translation, 
they will be added to this dictionary and a translation will need to be supplied (there are placeholder translations like "FR" or "french-001" in the meanwhile).

**NOTE:** Some french translations have been provided for testing purposes,
but these were not created by a francophone and should therefore be confirmed as accurate.