setwd("~/code/R/marginal_likelihood/probit_logit_chib")

df = read.table("raw_data.txt", sep = " ")
df = df[-nrow(df),-1]

m = as.matrix(df)

df = data.frame(rbind(m[,1:7], m[,-(1:7)]))
df = df[order(df[,1]),]

names(df) = c("Case", "y", paste0("X.", 1:5))

write.csv(df, "data.csv", row.names = FALSE)
