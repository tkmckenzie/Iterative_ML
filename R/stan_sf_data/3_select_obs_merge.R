library(dplyr)
library(foreign)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

data.folder = "data_2017/"
csv.folder = paste0(data.folder, "clean_csv/")
csv.files = grep(".csv$", list.files(csv.folder), value = TRUE)

filter.csv = function(csv.file){
  raw.df = read.csv(paste0(csv.folder, csv.file), stringsAsFactors = FALSE)
  raw.df[raw.df == -9] = NA
  
  df = raw.df %>%
    filter(Sector == 1) %>%
    select(Sector.Detail,
           Sector,
           Sales.Nominal,
           Capacity.Utilization,
           Capital.Purchases.Bool,
           Capital.Purchases.Amount,
           Upgraded.Capital.Bool,
           Upgraded.Capital.Percent,
           Land.Buildings.Amount,
           Full.Time.Employees,
           Full.Time.Employees.Production,
           Full.Time.Employees.Non.Production,
           Full.Time.Employees.Production.High.Skill,
           Full.Time.Employees.Production.Mid.Skill,
           Full.Time.Employees.Production.Low.Skill,
           Full.Time.Employees.Temporary,
           Full.Time.Employees.Temporary.Duration,
           Full.Time.Employees.Secondary.School.Percent,
           Labor.Cost,
           Raw.Material.Cost,
           Electricity.Cost,
           Total.Cost,
           Capital.Value,
           Country) %>%
    mutate(Capital.Purchases.Amount = ifelse(Capital.Purchases.Bool == 2, 0, Capital.Purchases.Amount),
           Upgraded.Capital.Percent = ifelse(is.na(Upgraded.Capital.Bool) | Upgraded.Capital.Bool == 2, 0, Upgraded.Capital.Percent),
           Labor.Quantity.Calculated = Full.Time.Employees + (Full.Time.Employees.Temporary.Duration / 12) * Full.Time.Employees.Temporary) %>%
    select(Sector.Detail,
           Sector,
           Sales.Nominal,
           Labor.Cost,
           Raw.Material.Cost,
           Electricity.Cost,
           Capital.Value,
           Country)
  
  df = df[complete.cases(df),]
  
  return(df)
}

df = filter.csv(csv.files[1])
for (csv.file in csv.files[-1]){
  df = rbind(df, filter.csv(csv.file))
}

write.csv(df, paste0(data.folder, "data.csv"))
