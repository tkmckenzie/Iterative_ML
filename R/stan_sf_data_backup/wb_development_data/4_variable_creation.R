library(dplyr)
library(frontier)

setwd("~/git/Iterative_ML/R/stan_sf_data/wb_development_data")

rm(list = ls())

#Filter years
df = read.csv("clean_csv/worldbank_ag_clean.csv", stringsAsFactors = FALSE)
df = df %>% filter(Year == 2012)

#Select variables
y = log(df$AG.PRD.CREL.MT)
X = with(df, log(cbind(AG.LND.CREL.HA, #Cereal production land area
                       AG.CON.FERT.ZS * AG.LND.ARBL.HA, #Total fertilizer consumption = (Fertilizer per Hectare) * Hectares
                       ER.H2O.FWAG.ZS * ER.H2O.FWTL.K3, #Agricultural freshwater withdrawals
                       AG.LND.PRCP.MM, #Average rainfall
                       (SP.POP.1564.TO + SP.POP.65UP.TO) * SL.EMP.TOTL.SP.ZS * SL.AGR.EMPL.ZS #Agricultural labor
)))

#Subset complete observations
complete.obs = complete.cases(cbind(X, y))
sum(complete.obs)

y = y[complete.obs]
X = X[complete.obs,]

#Set/get names
colnames(X) = c("Land.Area", "Fertilizer", "Irrigation", "Rainfall", "Labor")
countries = df$Country.Name[complete.obs]

#Run MLE model
summary(lm(y ~ X))
summary(sfa(y ~ X))

#Save
save(X, y, countries, file = "../data.RData")