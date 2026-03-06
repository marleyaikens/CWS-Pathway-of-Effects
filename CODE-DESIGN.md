# Developer Code Design Notes

This file contains a loose collection of code design choices of the second iteration of this tool (v.0.2.0).

This is primarily meant for future *developers* of this package.

## Data Files
- Currently, the tool builds upon the files: 
  - sectors.xlsx
  - components.xlsx
  - mitigations.xlsx
  - pathways.xlsx
  - translations.xlsx
  - ui_labels.xlsx
- In future, all could be bundled with the package in `inst/extdata`, but currently, to allow easier testing of changes, all but `ui_labels.xlsx` will be provided to users and the `read_sheets()` function will first look in `extdata` and will *then* look in the working directory.
- Developers working on this package may find it easier to store these files all
  in the inst/extdata, but to add exclusions to .git/info/exclude to prevent them 
  from being pushed to GitHub (just don't forget to remove these exclusions when 
  ready to publish the data in the package!)


## visNetwork quirks
- You can turn vis.js events into Shiny inputs using JS (this is how custom mitigations are added)
  - Events - https://visjs.github.io/vis-network/docs/network/#Events) 
  - Example - https://datastorm-open.github.io/visNetwork/shiny.html#build-your-own-input
- In order to revert the edge formating when adding/removing mitigations, 
  edge labels must start as " ". For what ever reason, `NA` and "" aren't updated when removing a mitigation edge label
    - possibly a bug in vis.js? https://github.com/visjs/vis-network/issues/1450

## Colour and things
- The nodes and mitigation pathway colours are defined in `R/utils` in `node_colours()` and `mitigated_colours()`

# Other diagrams
- The UI only shows the interactive diagrams but code to create the flowchart and orthogonal views are still present
- However, the orthogonal views need some work to ensure they work with more recent 
  changes to the nodes structure
- Further, either may depend on using data.table, a package which has been removed as a dependency (so to renable these diagrams, the code would need to be modified to not use data.table, OR data.table would need to be added as a dependency to the DESCRIPTION)

## Translations

Translations need to be filled out in the `translations.xlsx` file. This file
contains text that needs to be translated. It serves as both the dictionary used
by the UI as well as a list of text potentially requiring translation (pre-filled with 'FR' as a placeholder).

This file can be updated by running `dictionary_update()` which pulls text from the data (i.e., `mititgations.xlsx`, `components.xlsx`, and `pathways.xlsx`) as well as from `ui_labels.xlsx`.

When the Shiny UI is launched the dictionary option (`poe.dict`) is set to be the translations data frame.
To ensure that translations are functioning, there are three files which need to be manually updated.

1. `ui_labels.xlsx` - This is where all non-data translatable text needs to be listed. This includes lables on the Shiny UI as well as UI text from the reports.

2. `report_template.qmd` - Small UI labels from the reports should be listed in `ui_labels.xlsx`, but large blocks of text are best translated in the report itself.
These sections are identified by `if(lang == "en")... else ...` blocks of text.

3. `translations.xlsx` - This is the dictionary used to translate text.
As the package detects any new terms which do not already have a translation, 
they will be added to this dictionary and a translation will need to be supplied (there are placeholder translations like "FR" or "french-001" in the meanwhile).

**NOTE:** Some french translations have been provided for testing purposes,
but these were not created by a francophone and should therefore be confirmed as accurate.