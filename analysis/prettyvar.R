# Fix some ugly variable names
prettyvar <- function(variable.names) {
  vn <- variable.names
  # in dict, first column is used as regular expression, so have to escape 
  # special characters. 2nd column is only printed, but possibly to latex.
  dict <- matrix(ncol=2, byrow=TRUE, c(
    "\\(Dur. Intercept\\)", "Duration eq. intercept",
    "\\(Risk Intercept\\)", "Risk eq. intercept",
    "log\\(alpha\\)", "Hazard shape",
    "log10\\(i\\_matl\\_conf\\_DIStGOV\\_l1 \\+ 1\\)", "Dissident to gov't material conflict\\\\textsuperscript{a}",
    "log10\\(i\\_matl\\_coop\\_GOVtGOV\\_l1 \\+ 1\\)", "Intra-government material cooperation\\\\textsuperscript{a}",
    "ldr_age", "Leader age",
    "log10\\(events\\_by\\_mth\\_l1)", "Global event volume\\\\textsuperscript{b}",
    "ldr_irregular", "Leader entered irregularly",
    "ldr_foreign", "Leader imposed by foreign power",
    "log10\\(mths\\_in\\_power \\+ 1\\)", "Leader months in power\\\\textsuperscript{a}",
    "log10\\(i\\_verb\\_coop\\_GOVtGOV\\_l1 \\+ 1\\)", "Intra-government verbal cooperation\\\\textsuperscript{a}",
    "log10\\(i\\_verb\\_conf\\_GOVtDIS\\_l1 \\+ 1\\)", "Gov't to dissident verbal conflict\\\\textsuperscript{a}",
    "log10\\(i\\_verb\\_conf\\_DIStGOV\\_l1 \\+ 1\\)", "Dissident to gov't verbal conflict\\\\textsuperscript{a}",
    "log10\\(i\\_protest\\_tGOV\\_l1 \\+ 1\\)", "Anti-government protests\\\\textsuperscript{a}",
    "log\\(i\\_protest\\_tGOV\\_l1 \\+ 1\\)", "Anti-government protests\\\\textsuperscript{a}",
    "^IT\\.NET\\.USER\\.P2\\.l1", "Internet users",
    "^IT\\.CEL\\.SETS\\.P2\\.l1", "Mobile cellular users",
    "log\\(IT\\.CEL\\.SETS\\.P2\\.l1\\)", "Mobile cellular users\\\\textsuperscript{c}",
    "log10\\(exclpop\\.l1 \\+ 1\\)", "Fraction excluded from power\\\\textsuperscript{a}",
    "^AUTOC\\.l1", "Autocracy score",
    "W\\.knn4\\.std\\.eth\\.rel\\.h\\.count\\.l1", "Ethno-rel. conflict, nearest 4 neighbors\\\\textsuperscript{s}",
    "W\\.knn4\\.std\\.cw\\.h\\.count\\.both\\.l1", "Civil war violence, nearest 4 neighbors\\\\textsuperscript{s}",
    "gold\\_regime\\_l1FullDem", "Full democracy",
    "gold\\_regime\\_l1Other", "Other regime/in transition",
    "gold\\_regime\\_l1PartAut", "Partial autocracy",
    "gold\\_regime\\_l1PartDem$", "Partial democracy",
    "gold\\_regime\\_l1PartDemFact", "Partial democracy with factionalism",
    "SH\\.DYN\\.MORT\\.l1norml10", "Infant mortality rate\\\\textsuperscript{*}",
    "eth\\.rel\\.l\\.count\\.l1", "Ethno-religious conflict",
    "^reb\\.l\\.count\\.both\\.l1", "Rebellion",
    "protest\\.tALL\\.l1", "Protests, all",
    "W\\.gower\\.pol\\.reb\\.l\\.count\\.both\\.l1", "Rebellion in pol. similar countries\\\\textsuperscript{s}",
    "dom\\.cris\\.i\\.count\\.l1", "Domestic crisis events",
    "log10\\(MS\\.MIL\\.XPND\\.GD\\.ZS\\.l1\\)", "Military expenditure rate\\\\textsuperscript{b}",
    "log10\\(W\\.centdist\\.std\\.opp\\_resistance\\.l1 \\+ 1\\)", "Distance-weighted opposition resistance\\\\textsuperscript{a, s}",
    "log10\\(W\\.centdist\\.std\\.repression\\.l1 \\+ 1\\)", "Distance-weighted repression\\\\textsuperscript{a, s}",
    "log10\\(SP\\.POP\\.TOTL\\.l1\\)", "Population\\\\textsuperscript{b}",
    "log10\\(NY\\.GDP\\.MKTP\\.KD\\.l1\\)", "GDP\\\\textsuperscript{b}",
    "log10\\(opp_resistance\\.l1 \\+ 1\\)", "Opposition resistance\\\\textsuperscript{b}",
    "log\\(intratension\\.l1 \\* i_protest_tGOV_l1 \\* IT\\.CEL\\.SETS\\.P2\\.l1 \\+ 1\\)", "domestic crisis \\\\(\\\\times\\\\) anti-gov't protest \\\\(\\\\times\\\\) \\\\\\\\ mobile cellular users, \\\\textsuperscript{c}",
    "log\\(intratension\\.l1 \\+ 1\\)", "Domestic crisis events\\\\textsuperscript{c}",
    "log\\(IT\\.CEL\\.SETS\\.P2\\.l1 \\+ 1\\)", "Mobile cellular users\\\\textsuperscript{c}",
    "log10\\(NY\\.GDP\\.PCAP\\.KD\\.l1\\)", "GDP per capita\\\\textsuperscript{b}",
    "FP\\.CPI\\.TOTL\\.ZG\\.l1 > 5TRUE", "Inflation > 5\\\\%",
    "food\\_price\\_idx\\_l1", "Food price index",
    "eu\\_brent\\_oil\\_d1\\_l1", "Oil price"
    )
  )
  for (i in 1:nrow(dict)) {
    vn <- gsub(dict[i, 1], dict[i, 2], vn)
  }
  vn
}


