library(dplyr)
library(frontier)

setwd("~/git/Iterative_ML/R/stan_sf_data")

rm(list = ls())

df = read.csv("data_2017/data.csv", stringsAsFactors = FALSE)
df = df %>%
  # filter(Country == "Argentina")
  # filter(Country == "Bolivia")
  filter(Country == "Ecuador")
  # filter(Country == "Paraguay")
  # filter(Country == "Peru")
  # filter(Country == "Uruguay")

X = log(as.matrix(df %>%
                    select(Labor.Cost,
                           # Raw.Material.Cost,
                           # Electricity.Cost,
                           Capital.Value)))
y = log(df$Sales.Nominal)

#Drop rows with infinite values
infinite.rows = sapply(1:nrow(X), function(i) any(is.infinite(X[i,])))
X = X[!infinite.rows,]
y = y[!infinite.rows]

#Simple lm
summary(lm(y ~ X))
summary(sfa(y ~ X))

#Save data
save(X, y, file = "data.RData")
