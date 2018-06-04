library(ggplot2)

setwd("~/docs/Iterative_ML_paper/R/stan_probit_logit")

rm(list = ls())

load("logitData.RData")

df = data.frame(X.1 = X[,1], X.2 = X[,2], y = y)

m.logit = glm(y ~ 0 + X.1 + X.2, data = df, family = binomial(link = "logit"))
m.probit = glm(y ~ 0 + X.1 + X.2, data = df, family = binomial(link = "probit"))

X.fit = data.frame(X.1 = 1, X.2 = seq(min(X[,2]), max(X[,2]), length.out = 1000))
fitted.logit = predict(m.logit, newdata = X.fit, type = "response")
fitted.probit = predict(m.probit, newdata = X.fit, type = "response")

plot.data = data.frame(X = rep(X.fit[,2], times = 2),
                       y = c(fitted.logit, fitted.probit),
                       Model = rep(c("Logit", "Probit"), each = nrow(X.fit)))

ggplot(plot.data, aes(X, y)) + geom_line(aes(color = Model)) +
  theme_bw() +
  theme(legend.position = "top") +
  # ggtitle("Plot of Logit and Probit Curves") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Probit-Logit-Fitted.pdf", width = 4, height = 4)