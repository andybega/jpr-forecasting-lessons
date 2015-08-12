# Fix some ugly country names from countrycode package
prettyc <- function(country.names) {
  cn <- country.names
  dict <- matrix(ncol=2, byrow=TRUE, c(
    "Burkina Faso (Upper Volta)", "Burkina Faso",
    "Syrian Arab Republic", "Syria",
    "Congo, the Democratic Republic of the", "DR Congo",
    "Yemen Arab Republic", "Yemen",
    "Russian Federation", "Russia",
    "Bolivia, Plurinational State of", "Bolivia",
    "Venezuela, Bolivarian Republic of", "Venezuela",
    "Central African Republic", "CAR",
    "Macedonia, the former Yugoslav Republic of", "Macedonia",
    "Iran, Islamic Republic of", "Iran",
    "Moldova, Republic of", "Moldova")
  )
  for (i in 1:nrow(dict)) {
    cn <- gsub(dict[i, 1], dict[i, 2], cn)
  }
  cn
}