# Run the Application

The following guide provides instructions on installing the required software 
and running the application locally.

## Requirements

### Software Installations

Install RStudio and R from the software center or from source:

* [RStudio](https://posit.co/download/rstudio-desktop/)
* [R](https://cran.r-project.org/bin/windows/base/)

The R version must be >= 4.1.

### Install R Packages

Open Rstudio or an R terminal (interactive R command prompt) and run the 
following in the console to install required packages.

```r
install.packages(c("shiny", "bslib", "jsonlite", "data.table", "visNetwork", "DiagrammeR", "readxl", "htmlwidgets"))
```

This was developed with the following package versions:

* shiny 1.10.0
* bslib 0.8.0
* data.table 1.14.8
* jsonlite 1.8.7
* visNetwork 2.1.2
* DiagrammeR 1.0.11
* htmlwidgets 1.6.2

The latest versions should be sufficient. 
If errors occur when running the application (you will see these
errors when closing the application in RStudio and looking at the Console), 
install the packages from a snapshot in time:

```r
install.packages(c("shiny", "bslib", "jsonlite", "data.table", "visNetwork", "DiagrammeR", "readxl", "htmlwidgets"), repos="https://packagemanager.posit.co/cran/2025-02-05/")
```

## Run the app

### RStudio

Navigate to the CWS-Pathway-of-Effects folder and double-click `cws-poe.Rproj` or open the 
project in Rstudio (top right corner).

Open the `app/global.R` file in the editor. Click the run app button that is at 
the top right of the open file.

Note: Click the "Open in Browser" button in the top right pop-up window in 
Rstudio to open the app in a web browser.

To close the application, navigate back to RStudio and click the stop button in 
the R Console window.

### Command Line (Command Prompt/Powershell/Shell etc.)

Navigate to the CWS-Pathway-of-Effects folder and open an R terminal.

Run the following:

```
shiny::runApp("app")
```

### Configure Open in Browser (Optional)

Click the down arrow next to Run App button in the top right and check the 
"Run External" option.

# File Descriptions

  * app/global.R
    + Contains code/functions/shiny modules for running the application
  * app/ui.R
    + "Main" user interface file
  * app/server.R
    + "Main" server file that is the backend for the application
  * app/svg-pan-zoom.min.js
    + Javascript file for adding pan & zoom to diagrams
  * data/poe.json
    + JSON file of nodes/edges that is pulled from Visio diagrams
  * data/act2Pres.rds
    + R data file for mapping Activities to Pressures
  * data-raw/data-prep.R
    + R file to prepare data-raw/act2Pres.rds file from the excel file in data folder
  * parse-visio.py
    + Python file to prepare data/poe.json file from the vsdx file in data
  * README.md
    + Guide to installing software and setting up the app
  * cws-poe.Rproj
    + RStudio project file that when double-clicked opens up the project
  * .gitignore
    + Ignore this file, unless using git version control
  * data/en-fr-table.csv
    + Two column table of English and French text for translation.
  * data-raw/collect-text.R
    + Used to collect all text for translation. Does not need to be re-run.

# Update Datasets (Advanced Users)

This section is only for users that need to update the underlying data in the 
application.

## Requirements

### Software Installation

Install Python from the software center or from source:

* [Python](https://www.python.org/downloads/)

The Python version must be >= 3.10

### Add Python to the User PATH

Run `python --version` in the RStudio terminal, Command Prompt, or 
Powershell and if it returns a python version number skip to next 
section.

There is an option to add to path during installation or you can add later:

1. Locate the python installation folder and note "*FULL FILE PATH*/Python/" and 
"*FULL FILE PATH*/Python/Scripts/". Right-clicking on the python shortcut and 
selecting "Open File Location" can help locate the python install folder. 
2. Run "Edit environment variables for your account" from Settings or Windows 
Search
3. Click the "PATH" variable and add new for the two file paths above.
4. Restart the command line or open a new one
5. Run `python` and if it opens a "Python prompt" then it is successful.

### Install Python Modules

Open the command prompt/command line terminal and run the following to install 
the required modules.

OR

Open the "Terminal" tab next to the "Console" tab in Rstudio. 

Note: The Terminal in Rstudio uses CTRL+SHIFT+C/CTRL+SHIFT+V for 
copy and paste respectively.

```
pip install polars vsdx
```

This was developed with the following module versions:

* polars 1.17.1
* vsdx 0.5.19

The latest versions should be sufficient but if not run the following:

```
pip install polars==1.17.1 vsdx==0.5.19
```

## Refresh App Datasets

This only needs to be re-run when the Visio diagrams and/or the Excel crosswalk 
tables are modified.

### Update Activities to Stressors

1. Change the first line of the `data-raw/data-prep.R` script to point to the new file 
(if different). 
2. In Rstudio, open the `data-raw/data-prep.R` script and click the "Source" button. 
Alternatively, you can run line-by-line. OR
From the command line:

```r
source("data-raw/data-prep.R")
```

### Update Diagram Data

From the command line (you can also use terminal in Rstudio), navigate to the 
"cws-poe" folder. Replace the first argument with the name of the new vsdx file.

Run the following:

```
python .\parse-visio.py "data-raw/AmalgamatedPoE_23-Jan-2024_Update.vsdx" -o "data/poe.json"
```

### Update Translations

The en-fr-table.csv file contains columns of all text this is on the 
user interface, visio diagrams, and excel crosswalk file. If any of those 
are changed, a translation record will need to be added/modified.

# Troubleshooting

## Shiny app fails to run

* Check the package versions. If any packages versions are less than the 
requirements, then update the packages. 

* The raw data format changed so that the data prep/transformation was broken. 
See next section.

## Shiny app functionality is missing

* Use a chromium compatible browser such as Edge or Chrome

## Data editing

* The Visio diagram needs to be edited in a way to preserve the direction of 
arrows. The python script assumes a top-down direction of arrows so if nodes 
are missing or links are directionally incorrect, position the "from" node 
above the "to" node on the Y-axis.

* Links that are recursive are not allowed. Nodes cannot have a link that 
points back to itself.

* Ensure that links are connected to shapes. Links can sometimes visually 
look connected but may not actually be connected in the data.

* Naming conventions between the Excel and Visio documents needs to be precise. 
They are also case-sensitive. 
