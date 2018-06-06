library(dplyr)
library(foreign)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

data.folder = "data_2017/"
csv.folder = paste0(data.folder, "raw_csv/")
csv.files = grep(".csv$", list.files(csv.folder), value = TRUE)

csv.files = csv.files[!grepl("Liberia|Niger|Sierra Leone", csv.files)]

# asdf
# csv.file = csv.files[1]
for (csv.file in csv.files){
  raw.df = read.csv(paste0(csv.folder, csv.file))
  df = raw.df %>%
    select(Sector.Detail = a4b,
           Sector = a0,
           Size = a6b,
           Part.Of.Larger.Firm = a7,
           Num.Establishments = a7a,
           Headquarters = a7b,
           Separate.Financial.Statements = a11,
           Firm.Status = b1,
           Establishment.Begin.Year = b5,
           Electrical.Application = c3,
           Electrical.Wait = c4,
           Power.Outage = c6,
           Num.Power.Outages = c7,
           Power.Outage.Duration.Hours = c8a,
           Power.Outage.Duration.Minutes = c8b,
           Generator = c10,
           Generator.Use = c11,
           Water.Application = c12,
           Water.Wait = c13,
           Water.Outage = c15,
           Num.Water.Outages = c16,
           Water.Outage.Duration = c17,
           Email = c22a,
           Website = c22b,
           Main.Activity = d1a1a,
           Sales.Nominal = d2,
           Theft.Loss = d6,
           Breakage.Loss = d7,
           Capacity.Utilization = f1,
           Operation.Hours = f2,
           Capital.Purchases.Bool = k4,
           Capital.Purchases.Amount = n5a,
           Upgraded.Capital.Bool = ASCn6,
           Upgraded.Capital.Percent = ASCn7,
           Land.Buildings.Amount = n5b,
           Full.Time.Employees = l1,
           Full.Time.Employees.Production = l3a,
           Full.Time.Employees.Non.Production = l3b,
           Full.Time.Employees.Production.High.Skill = l4a1,
           Full.Time.Employees.Production.Mid.Skill = l4a2,
           Full.Time.Employees.Production.Low.Skill = l4b,
           Full.Time.Employees.Temporary = l6,
           Full.Time.Employees.Temporary.Duration = l8,
           Full.Time.Employees.Secondary.School.Percent = l9b,
           Labor.Cost = n2a,
           Raw.Material.Cost = n2e,
           Electricity.Cost = n2b,
           Total.Cost = n2p,
           Capital.Value = n7a) %>%
    mutate(Country = gsub(".csv$", "", csv.file))
  write.csv(df, paste0(data.folder, "clean_csv/", csv.file), row.names = FALSE)
}
