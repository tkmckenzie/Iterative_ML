library(dplyr)
library(foreign)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

data.folder = "data_2017/"
zip.folder = paste0(data.folder, "zips/")
dta.files = grep(".dta$", list.files(zip.folder), value = TRUE)

dta.file = dta.files[1]
for (dta.file in dta.files){
  df = read.dta(paste0(zip.folder, dta.file), convert.factors = FALSE, warn.missing.labels = FALSE)
  write.csv(df, paste0(data.folder, "raw_csv/", gsub("-[0-9a-zA-z -]+.dta$", "", dta.file), ".csv"), row.names = FALSE)
}
