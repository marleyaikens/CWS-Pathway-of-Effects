# update this file name
filePath <- "data/Activities_pressures_cross_check_forContractor (003).xlsx"
get_vc_names <- function(filePath) {
  sheets <- readxl::excel_sheets(filePath)
  vapply(X = sheets, FUN = function(sheet) {
    readxl::read_excel(path = filePath, sheet = sheet, range = "A1", 
      col_names = "valued_component") |> 
      getElement("valued_component")
    
  }, FUN.VALUE = character(1))
}
fullNames <- get_vc_names(filePath = filePath)
# Look at spreadsheet and manually assign ranges for data without column names
ranges <- setNames(c("A4:Z16", "A4:Z17", "A4:Z12", "A4:Z13", "A4:Z14"), 
  c("Terrestrial SAR", "Mig Birds", "Marine Birds", "Plant SAR", "Wetlands"))
# collect "checks" for each range and combine data into "long" dataframe
act2Pres <- lapply(seq_along(ranges), function(i, ranges, filePath) {
  # need temp names to match with later given length of # of chars in names
  # exceed limits and contain special characters
  tempColNames <- c("stressors", sprintf("c%d", 2:26))
  # read column names as data
  colNames <- readxl::read_excel(
    path = filePath, 
    sheet = names(ranges)[i], range = "A2:Z2", col_names = tempColNames) |> 
    unlist()
  # read "checks" data and pivot long
  checks <- readxl::read_excel(
    path = filePath, 
    sheet = names(ranges)[i], range = ranges[i], col_names = tempColNames, 
    col_types = "text") |> 
    data.table::data.table() |> 
    data.table::melt(id.vars = "stressors", variable.factor = FALSE, 
      variable.name = "activities", value.name = "check")
  # replace temp col names with real labels
  checks[ , activities := colNames[activities]]
  # drop NA as only interested in "TRUE" case
  checks[!is.na(check), -c("check")]
}, ranges = ranges, filePath = filePath) |> 
  stats::setNames(names(ranges)) |> 
  data.table::rbindlist(idcol = "valued_component")
# replace full name for valued component
act2Pres[ , valued_component := fullNames[valued_component]]
# Remove leading/trailing whitespace
act2Pres <- act2Pres[ , lapply(.SD, trimws, whitespace = "[\\h\\v]")]
# test
stopifnot(!anyNA(act2Pres))
# save into app for use
saveRDS(act2Pres, file = "app/act2Pres.rds")
