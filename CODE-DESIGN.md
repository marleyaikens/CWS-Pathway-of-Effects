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