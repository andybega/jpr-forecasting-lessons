# Which cases in the test set are we doing well/bad on?

load("data/pr_calib.rda")
load("data/pr_test6.rda")

# Predictions for all y=1 in calibration period
pr_calib %>%
  arrange(ebma) %>%
  filter(y==1) %>%
  mutate(country = countrycode(gwcode, "cown", "country.name")) %>%
  mutate(country = prettyc(country)) %>%
  select(country, gwcode, date, ebma, i1:i7) %>%
  write.csv(file="theme-search/calib-y1.csv", row.names=FALSE)

# Top false positives (y=0) in calibration period
pr_calib %>%
  arrange(desc(ebma)) %>%
  filter(y==0) %>%
  mutate(country = countrycode(gwcode, "cown", "country.name")) %>%
  mutate(country = prettyc(country)) %>%
  select(country, gwcode, date, ebma, i1:i7) %>%
  slice(1:20) %>%
  write.csv(file="theme-search/calib-y0.csv", row.names=FALSE)

# Predictions for all y=1 in test period
pr_test %>%
  arrange(ebma) %>%
  filter(y==1) %>%
  mutate(country = countrycode(gwcode, "cown", "country.name")) %>%
  mutate(country = prettyc(country)) %>%
  select(country, gwcode, date, ebma, i1:i7) %>%
  write.csv(file="theme-search/test-y1.csv", row.names=FALSE)

# Top false positives (y=0) in test period
pr_test %>%
  arrange(desc(ebma)) %>%
  filter(y==0) %>%
  mutate(country = countrycode(gwcode, "cown", "country.name")) %>%
  mutate(country = prettyc(country)) %>%
  select(country, gwcode, date, ebma, i1:i7) %>%
  slice(1:20) %>%
  write.csv(file="theme-search/test-y0.csv", row.names=FALSE)

# Individual model evaluation; just change arrange order
pr_calib %>%
  arrange(desc(i1)) %>%
  filter(y==0) %>%
  mutate(country = countrycode(gwcode, "cown", "country.name")) %>%
  mutate(country = prettyc(country)) %>%
  select(country, gwcode, date, ebma, i1:i7) %>%
  slice(1:20)

pr_test %>%
  arrange(desc(i1)) %>%
  filter(y==0) %>%
  mutate(country = countrycode(gwcode, "cown", "country.name")) %>%
  mutate(country = prettyc(country)) %>%
  select(country, gwcode, date, ebma, i1:i7) %>%
  slice(1:20)