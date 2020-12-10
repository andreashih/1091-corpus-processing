library(readr)
library(dplyr)
library(irr)

yongfu <- read_delim("yongfu.tsv", "\t", 
                     escape_double = FALSE, trim_ws = TRUE) 
yongfu <- yongfu %>%
  arrange(id)

andrea <- read_delim("andrea.tsv", "\t", 
                     escape_double = FALSE, trim_ws = TRUE)
andrea <- andrea %>%
  arrange(id) %>%
  top_n(-length(yongfu$id), id)

rater1 <- andrea$sentiment
rater2 <- yongfu$sentiment
annotation <- cbind(rater1,rater2)

kappa2(annotation)
