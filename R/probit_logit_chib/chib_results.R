library(dplyr)
library(xtable)

setwd("~/docs/Iterative_ML_paper/R/probit_logit_chib")

#Read in results from each model
files = grep("^chib_model_[0-9]+_results.RData$", list.files(), value = TRUE)

load(files[1])
df = log.marginal.results

for (file in files[-1]){
  load(file)
  df = rbind(df, log.marginal.results)
}

df = df[complete.cases(df),]

#Input Chib results and join
chib.results = data.frame(model = as.factor(c("C", "C + X.1", "C + log(X.2)", "C + X.3", "C + X.4", "C + X.5", "C + log(X.2) + X.4", "C + log(X.2) + X.3 + X.4", "C + log(X.2) + X.3 + X.4 + X.5")),
                          chib.ml = c(-38.503, -43.175, -37.916, -35.323, -37.234, -39.075, -36.140, -34.553, -36.233),
                          chib.ml.se = c(0.005, 0.007, 0.007, 0.009, 0.009, 0.007, 0.013, 0.020, 0.024))
df = df %>%
  left_join(chib.results, by = "model")

#Statistics
results = df %>%
  group_by(model) %>%
  summarize(ml.logit = mean(logit), ml.probit = mean(probit),
            ml.logit.se = sd(logit), ml.probit.se = sd(probit),
            p.value.probit.chib = t.test(probit, mu = unique(chib.ml))$p.value) %>%
  ungroup() %>%
  mutate(logit.rank = rank(-ml.logit),
         probit.rank = rank(-ml.probit))
data.frame(results)

#Find overall model probabilities
model.df = results %>%
  select(model, ml.logit, ml.probit)
model.df = data.frame(model.df)
model.df = reshape(model.df,
                   direction = "long",
                   varying = c("ml.logit", "ml.probit"),
                   timevar = "link")

total.ml = sum(exp(model.df$ml))
model.df$model.prob = round(exp(model.df$ml) / total.ml, 5)

model.df %>%
  arrange(desc(model.prob))
