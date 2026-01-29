This file details code design choices of the second iteration of this tool (v.0.2.0)

-  `R -q -e "shiny::runApp(port = 8080)"`

## General Notes

- This project is not an R package, but we use a DESCRIPTION file and a R folder
  similar to in R packages to organize functions and track dependencies and versions.

## Locations
- Functions to run this app can be found in the `R` folder.
- The `global.R` file contains configuration instructions.

## visNetwork quirks
- In order to revert the edge formating when adding/removing mitigations, 
  edge labels must start as " ". For what ever reason, NA and "" don't get updated when removing a mitigation edge label
    - possibly a bug in vis.js? https://github.com/visjs/vis-network/issues/1450