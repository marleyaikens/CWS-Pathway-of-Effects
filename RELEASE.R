# These are the steps to take to make a new release

# Developers should install and use the `devtools` R package

# Update Translations --------------------------------------------------------
# - Add any new UI/report labels (i.e. anything not in a data file) to
#   `inst/extdata/ui_lables.xlsx`
# - Update `inst/extdata/translations.xlsx` by running the following:
dictionary_update()
# - Ensure that any new terms in `translations.xlsx` are updated

# Including data in the package -----------------------------------------------
#
# Once the data sets have been finalized, the data can be included directly in
# the package so it is installed along with the code.
#
# To do this: Put `sectors.xlsx`, `components.xlsx`, `mitigations.xlsx`,
# `pathways.xlsx` and `translations.xlsx` in the `inst/extdata/` folder.

# Bump version (i.e. increase the version number)
file.edit("DESCRIPTION")

# Add changes to NEWS
file.edit("NEWS.md")

# Check the package -----------------------------------------------------------

# First build the package
# (examples making reports will need access to the built version)
devtools::build()

# Check tests & examples
devtools::test()
devtools::run_examples()

# Note, the final example will launch the Shiny UI, so you'll have to exit out
# before proceeding.

# Check that the package builds and installs
devtools::check()

# Release a new version -------------------------------------------------------
# - Merge with the main branch (if not working on main)
# - Make a GitHub Release
