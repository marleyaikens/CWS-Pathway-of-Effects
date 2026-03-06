# poe: Pathways of Effects Explorer

<!-- badges: start -->
<!-- badges: end -->

## Overview

The `poe` package provides an interactive Shiny User Interface for exploring Pathways of Effects (PoE) diagram models developed by the Canadian Wildlife Service (CWS).

## Installation

You can install the development version of poe from GitHub:

``` r
# install.packages("pak")
pak::pak("marleyaikens/CWS-Pathway-of-Effects@dev-mitigations")
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

## Customizing Data

To explore customizing the data, see [custom_data.md](custom_data.md)

## License

MIT + file LICENSE
