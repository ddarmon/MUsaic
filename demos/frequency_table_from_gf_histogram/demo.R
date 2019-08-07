library(mosaic)
library(MUsaic)

load('ta01-02.rda')

gf_histogram(~ Ratio, data = `ta01-02`, binwidth = 1, boundary = 0) %>%
  gf_rugx(~ Ratio)

frequency_table_from_gf_histogram(gf_histogram(~ Ratio, data = `ta01-02`, binwidth = 1, boundary = 0))
