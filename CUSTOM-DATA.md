# Customizing Background Data

The `poe` package provides a Shiny Web Interface for exploring Pathways of Effects based
on data held in several Excel files. This document explains how to modify these
files to customize pathways, activities, stressors, and mitigations.

## Setup for users
Once the data files are ready to be bundled into the package, they need to be
added to the `inst/extdata` folder. 
This can be done via Git (see [Happy Git and GitHub for the useR](https://happygitwithr.com/) to get started), 
or via the GitHub webpage: 

- Go to the [inst/extdata](https://github.com/marleyaikens/CWS-Pathway-of-Effects/tree/main/inst/extdata) folder
- Click on "Add file" > "Upload Files" in the upper right corner
- Choose/Drag files to the box at the top
- Add a description to the "Commit" (i.e. a message explaining the change)
- Click "Commit Changes" (it's oky to commit directly to the main branch here)

That's it!


## Setup for testing
During the testing phase, the data files are expected to be in the local working directory.

**NOTE**: if the data files have been moved to the `inst/extdata` folder (above), then the package will ignore local files in the working directory!

This is the location of the RStudio Project.

Therefore:

1. Create a New RStudio project 
2. Save the 5 data files to this folder
3. Open the project
4. Install the poe R package
    - First install the 'pak' package. Type `install.packages('pak')` in the console and hit enter. Wait until the installation is complete.
    - Second install the testing version of the 'poe' package. Type `pak::pkg_install("marleyaikens/CWS-Pathway-of-Effects@dev-mitigations")` in the console and hit enter. Wait until the installation is complete. If you are asked to update any packages, or to install Rtools, please do so.
4. Run the tool by typing `poe::poe_tool()` in the console and hit enter.

To test creating custom pathways, read the following instructions, modify the files,
run the tool (`poe::poe_tool()`) and see if things have changed as you expected.


## File Overview

The tool uses four main data files:

- **`pathways.xlsx`** - Defines pathway diagrams for each valued component
- **`components.xlsx`** - Links valued components to activities and stressors
- **`sectors.xlsx`** - Lists activities associated with specific sector
- **`mitigations.xlsx`** - Defines mitigation measures which can interrupt pathways
- **`translations.xlsx`** - French translations for all text (generally this file is not modified directly, except to add French translations).

While testing the modification of these files, they will be kept in an RStudio Project folder so the package can locate them. Later, when the pathways and UI are finalized, these files will be included in the package directly so they are installed automatically.

## File Structure

Every Excel file starts with a `Notes` worksheet with details about the 
expected formats and how this file works.


### `pathways.xlsx`

Defines the nodes and edges for pathway diagrams. Each sheet represents one
valued component (except the first, "Notes" sheet).

**Required columns:**
- `valued_component` - Name of the valued component
- `node_id` - Unique identifier for the node within the component
- `label` - Text label displayed in the diagram
- `type` - Node type: "component", "stressor", "effect", "contravention",
  "habitat", "quality", or "population", used to define node colour
- `level` - Hierarchical level (highest number = top, decreases downward)
- `leads_to` - Comma-separated list of child node IDs (blank for terminal nodes)
- `sort` - (Optional) Sort order within a level

**To add a new valued component pathway:**
1. Create a new sheet in `pathways.xlsx` with the valued component name
2. Define nodes starting with the valued component at a high level (e.g., 10)
3. Add stressor nodes, effects, etc. at decreasing levels (each level corresponds to a row in the diagram).
4. Use `leads_to` to create paths between nodes
5. Set `type` for proper color coding
6. Within a `level`, arrange nodes from left to right using the sort column

**To edit an existing pathway:**
1. Modify node labels, types, or levels as needed
2. Update `leads_to` to add or remove connections
3. Ensure all referenced node IDs exist

> See the Notes worksheet for an overview and the 'Test' sheet for a basic example.

### `components.xlsx`

This file links valued components to activities and their associated stressors. This determines which activities are listed in the UI and which stressors are included in the diagram when a particular activity is selected.

**Required columns:**
- `valued_component` - Name of the valued component
- `activities` - Activity name
- `stressors` - Stressor label (must have a corresponding node in the pathway)

**To add a new activity-stressor relationship:**
1. Add a row with the valued component, activity, and stressor
2. Ensure the stressor label exactly matches the node label in `pathways.xlsx`

> See the Notes worksheet for an overview

### `sectors.xlsx`

Links Sectors to specific activities to be used by the Sector selection menu in 
the tool.

**Required columns:**
- `sector` - Sector name (e.g., "Transportation", "Energy")
- `activities` - Activity name (must match activities in `components.xlsx`)

**To add a new sector grouping:**
1. Add row with the sector and associated activity
2. Ensure the activity is also referenced in `components.xlsx`

> See the Notes worksheet for an overview

### `mitigations.xlsx`

Defines mitigation measures that can be applied to pathway edges.

**Required columns:**
- `valued_component` - Name of the valued component
- `start_node` - Starting node ID of the edge to mitigate
- `end_node` - Ending node ID of the edge to mitigate
- `short` - Short description displayed in the UI
- `long` - Detailed description displayed in the report

**To add a new mitigation:**
1. Identify the edge (start and end nodes) where the mitigation applies
  - In the UI, if you hover the mouse over an edge/pathway, the start end nodes
    will appear in a tooltip, e.g., '1-2' (start node 1, end node 2)
2. Add a row with the valued component, nodes, and descriptions
3. Ensure node IDs match those in `pathways.xlsx`
  - If you use the UI to get the nodes, this should be the case

## Workflow for Adding New Data

### Adding a New Valued Component

1. **Create pathway diagram** (`pathways.xlsx`) **REQUIRED**
   - Add new sheet with valued component name
   - Define all nodes and connections
   
2. **Link to activities** (`components.xlsx`) **REQUIRED**
   - Add new sheet with valued component name
   - Add rows linking activities to stressors for this component
   
3. **Add mitigations** (`mitigations.xlsx`) **OPTIONAL**
   - Add new sheet with valued component name
   - Define any mitigation measures for relevant edges

4. **Update translations** (semi-automatic)
   - Run `poe::dictionary_update()` to extract new English text
   - Add French translations to `translations.xlsx`
   - (Skipping this step will result in 'NA's in the French language version
     of the UI, rather than 'FR' placeholders, so it's safe to skip while testing).

### Adding a New Activity or Stressor

1. **Update components** (`components.xlsx`) **REQUIRED**
   - Add rows linking the activity to stressors for each relevant valued component

2. **Associate with sectors** (`sectors.xlsx`) **OPTIONAL**
   - Assign sectors to the activity

3. **Update pathways** (`pathways.xlsx`) **REQUIRED**
   - If adding a new stressor, add it as a node in relevant pathway sheets
   - Update `leads_to` to connect it properly to the rest of the diagram

4. **Add mitigations** (`mitigations.xlsx`) **OPTIONAL**
   - Define any new mitigations which may apply to these new pathway connections

### Editing Existing Pathways

1. **Modify nodes** (`pathways.xlsx`)
   - Change labels, types, or levels as needed
   - Update or add connections via `leads_to`

2. **Update stressors/activities** (`components.xlsx`)
   - Ensure any new stressors in pathways are added to components and associated with an activity

3. **Verify mitigations** (`mitigations.xlsx`)
   - Remove mitigations which correspond removed edges
   - Add any new mitigations for new edges

### Adding/Modifying Activities Associated with Sectors

1. **Modify Sectors/Activities** (`sectors.xlsx`)
   - Add row with the sector and associated activity
   - Ensure the activity is also referenced in `components.xlsx`

## Data Validation

After modifying data files if you run the tool `poe::poe_tool()`, several validation checks will run and let you know if there are mismatches among the files/components/etc. and where these occur.

## Tips

- **Node IDs** should be unique within a valued component but can follow patterns
  (e.g., S1-S5 for stressors, E1-E3 for effects)
  
- **Labels** should be concise for display but will wrap in the diagram if they are long

- **Levels** determine vertical positioning - use consistent increments to get a balanced diagram

- **Edge identification** - Mitigations reference edges by start and end nodes,
  so changing node IDs requires updating mitigations (try not to change node ids if you don't have to)
