# poe: Pathways of Effects Explorer

<!-- badges: start -->
<!-- badges: end -->

## Overview

The `poe` package provides an interactive Shiny User Interface for exploring Pathways of Effects (PoE) diagram models developed by the Canadian Wildlife Service (CWS).

## Installation

You can install poe from GitHub.

``` r
# install.packages("pak")
pak::pak("marleyaikens/CWS-Pathway-of-Effects")
```

**On Windows**

On Windows you may need to install RTools. If you try the above code but get a message to this
effect download and install [RTools](https://cran.r-project.org/bin/windows/Rtools/) for your R version. You can find your R version by looking at the first line in the R Console when you start RStudio. 

Close RStudio and restart. 

Try installing again: 

``` r
# install.packages("pak")
pak::pak("marleyaikens/CWS-Pathway-of-Effects")
```

## Requirements

- R >= 4.1.0
- Quarto (for report generation, installed by default with RStudio)

## Getting Started

Create an RStudio project and place the custom data files in this folder.

Then launch the Shiny UI with `poe_tool()`:

``` r
library(poe)
poe_tool()
```

### Basic Workflow

1. **Select a Valued Component** from the dropdown (e.g., "Marine Birds", "Wetlands")

2. (Optional) **Choose a Sector** to pre-select activities associated with specific sectors.

3. Further **Select Activities** relevant to your project

4. **View the Pathway** showing how activities lead to stressors, effects, and outcomes

5. **Apply Mitigations** to see how they alter pathway outcomes
    - Option to create and apply custom mitigations

6. **Generate a Report** documenting your pathway

## Learning More

- To explore customizing the data, see [CUSTOM-DATA.md](CUSTOM-DATA.md)
- To learn more about the coding and design choices, see [CODE-DESIGN.md](CODE-DESIGN.md)
- To make a new release of this R package, follow instructions in [RELEASE.R](RELEASE.R)
- To learn more about R package development and workflows, see: 
    - [R Packages](https://r-pkgs.org/) - Excellent guide, standard used by many people
    - [rOpenSci's developer Guide](https://devguide.ropensci.org/) - Good but detailed and meant for packages going to be submitted for Peer-Review
- How to setup and use the R-Universe - https://docs.r-universe.dev/publish/set-up.html

## License

MIT + file LICENSE
