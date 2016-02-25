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
    "Moldova, Republic of", "Moldova",
    "Korea, Democratic People's Republic of", "Korea, North",
    "Korea, Republic of", "Korea, South",
    "Federal Republic of Germany", "Germany",
    "Lao People's Democratic Republic", "Laos",
    "Tanzania, United Republic of", "Tanzania",
    "Taiwan, Province of China", "Taiwan",
    "Yemen (Arab Republic of Yemen)", "Yemen",
    "Burkina Faso (Upper Volta)", "Burkina Faso")
  )
  for (i in 1:nrow(dict)) {
    cn <- gsub(dict[i, 1], dict[i, 2], cn)
  }
  cn
}