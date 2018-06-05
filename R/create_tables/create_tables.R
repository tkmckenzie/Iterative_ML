library(xtable)

setwd("~/git/Iterative_ML/R/create_tables")

rm(list = ls())

#Number of digits to round results to
round.digits = 3

#P-value function
p.value = function(x, y){
  n.x = length(x)
  n.y = length(y)
  
  z = -abs((mean(x) - mean(y)) / sqrt(var(x) / n.x + var(y) / n.y))
  
  return(2 * pnorm(z))
}

##############################
#Table 1: Comparison of Gibbs, Iterative KDE for MVN and probit
table.matrix = matrix(NA, nrow = 8, ncol = 5)
table.matrix[1,] = c("", "", "", "", "Mean Test")
table.matrix[2,] = c("Model", "\\# Trials", "Gibbs/Chib", "Iterative KDE", "$p$-value")

#MVN
num.mc.iter = 500

#Read results
load("../gibbs_stan_lm/gibbs_mc_results.RData")
gibbs.results = log.marginal.results[,1:num.mc.iter]

load("../gibbs_stan_lm/stan_mc_results.RData")
stan.results = log.marginal.results[,1:num.mc.iter]

rm(log.marginal.results)

#Create MVN rows
table.matrix[3,] = c("\\multirow{3}{*}{Multivariate Linear}",
                     paste0("\\multirow{3}{*}{", num.mc.iter, "}"),
                     round(mean(gibbs.results[1,]), round.digits),
                     round(mean(stan.results[1,]), round.digits),
                     paste0("\\multirow{3}{*}{", round(p.value(gibbs.results[1,], stan.results[1,]), round.digits), "}"))
table.matrix[4,] = c("", "",
                     paste0("(", round(sd(gibbs.results[1,]), round.digits), ")"),
                     paste0("(", round(sd(stan.results[1,]), round.digits), ")"),
                     "")
table.matrix[5,] = c("", "", "Iter = 5,000", "Iter = 5,000", "")

#Probit
num.mc.iter = 500

#Read results
load("../gibbs_stan_probit/gibbs_mc_results.RData")
gibbs.results = log.marginal.results[,1:num.mc.iter]

load("../gibbs_stan_probit/stan_mc_results.RData")
stan.results = log.marginal.results[,1:num.mc.iter]

rm(log.marginal.results)

#Create Probit rows
table.matrix[6,] = c("\\multirow{3}{*}{Probit}",
                     paste0("\\multirow{3}{*}{", num.mc.iter, "}"),
                     round(mean(gibbs.results[1,]), round.digits),
                     round(mean(stan.results[1,]), round.digits),
                     paste0("\\multirow{3}{*}{", round(p.value(gibbs.results[1,], stan.results[1,]), round.digits), "}"))
table.matrix[7,] = c("", "",
                     paste0("(", round(sd(gibbs.results[1,]), round.digits), ")"),
                     paste0("(", round(sd(stan.results[1,]), round.digits), ")"),
                     "")
table.matrix[8,] = c("", "", "Iter = 50,000", "Iter = 5,000", "")

#Write LaTeX table
table = xtable(table.matrix,
               align = c("l", "l", "|c", "|c", "|c", "|c"),
               caption = "Comparison of Gibbs and Iterative KDE",
               label = "tab:MVN-Probit")
table = print.xtable(table,
                     floating.environment = "table*",
                     table.placement = NULL,
                     hline.after = c(2, 2, 5, 8),
                     sanitize.text.function = identity,
                     include.rownames = FALSE,
                     include.colnames = FALSE)
cat(table, file = "../../tables/Table1.tex")


##############################
#Table 2: Comparison of Probit and Logit Models: An Example from \cite{Chib}
table.matrix = matrix(NA, nrow = 20, ncol = 4)
table.matrix[1,] = c("", "", "", "Chib Est.")
table.matrix[2,] = c("Specification", "Logit", "Probit", "$p$-value")

#These specs and estimates from Chib (1995)
chib.specs = c("$C$", "$C + x_1$", "$C + \\log(x_2)$",
               "$C + x_3$", "$C + x_4$", "$C + x_5$",
               "$C + \\log(x_2) + x_4$", "$C + \\log(x_2) + x_3 + x_4$", "$C + \\log(x_2) + x_3 + x_4 + x_5$")
chib.est = c(-38.503, -43.175, -37.916,
             -35.323, -37.234, -39.075, 
             -36.140, -34.553, -36.233)

for (model.num in 1:9){
  load(paste0("../probit_logit_chib/chib_model_", model.num, "_results.RData"))
  model.spec = chib.specs[model.num]
  
  table.matrix[(model.num) * 2 + 1,] = c(paste0("\\multirow{2}{*}{", model.spec, "}"),
                                         round(mean(log.marginal.results$logit), round.digits),
                                         round(mean(log.marginal.results$probit), round.digits),
                                         paste0("\\multirow{2}{*}{", round(p.value(log.marginal.results$probit, rep(chib.est[model.num], 2)), round.digits), "}"))
  table.matrix[(model.num) * 2 + 2,] = c("",
                                        paste0("(", round(sd(log.marginal.results$logit), round.digits), ")"),
                                        paste0("(", round(sd(log.marginal.results$probit), round.digits), ")"),
                                        "")
}

#Write LaTeX table
table = xtable(table.matrix,
               align = c("l", "l", "|c", "|c", "|c"),
               caption = "Comparison of Logit and Probit Models Using an Example from \\cite{Chib}",
               label = "tab:Logit-Probit-Chib")
table = print.xtable(table,
                     floating.environment = "table*",
                     table.placement = NULL,
                     hline.after = c(2, seq(2, 20, by = 2)),
                     sanitize.text.function = identity,
                     include.rownames = FALSE,
                     include.colnames = FALSE)
cat(table, file = "../../tables/Table2.tex")


##############################
#Table 3: Monte Carlo Comparison of Logit and Probit Models
table.matrix = matrix(NA, nrow = 3, ncol = 5)
table.matrix[1,] = c("Data", 
                     "Probit Probability", 
                     "Logit Probability", 
                     "Pr(Probit $>$ Logit)",
                     "Pr(Logit $>$ Probit)")

load("../stan_probit_logit/comparison_mc_results.RData")

table.matrix[2,] = c("Probit",
                     round(mean(exp(log.marginal.results$probit.probit) / (exp(log.marginal.results$probit.probit) + exp(log.marginal.results$logit.probit))), round.digits),
                     round(mean(exp(log.marginal.results$logit.probit) / (exp(log.marginal.results$probit.probit) + exp(log.marginal.results$logit.probit))), round.digits),
                     round(mean(log.marginal.results$probit.probit > log.marginal.results$logit.probit), round.digits),
                     round(mean(log.marginal.results$probit.probit < log.marginal.results$logit.probit), round.digits))
table.matrix[3,] = c("Logit",
                     round(mean(exp(log.marginal.results$probit.logit) / (exp(log.marginal.results$probit.logit) + exp(log.marginal.results$logit.logit))), round.digits),
                     round(mean(exp(log.marginal.results$logit.logit) / (exp(log.marginal.results$probit.logit) + exp(log.marginal.results$logit.logit))), round.digits),
                     round(mean(log.marginal.results$probit.logit > log.marginal.results$logit.logit), round.digits),
                     round(mean(log.marginal.results$probit.logit < log.marginal.results$logit.logit), round.digits))

#Write LaTeX table
table = xtable(table.matrix,
               align = c("l", "l", "|c", "|c", "|c", "|c"),
               caption = "Monte Carlo Comparison of Logit and Probit Models",
               label = "tab:Probit-Logit-Sim")
table = print.xtable(table,
                     floating.environment = "table*",
                     table.placement = NULL,
                     hline.after = c(1, 1, 2, 3),
                     sanitize.text.function = identity,
                     include.rownames = FALSE,
                     include.colnames = FALSE)
cat(table, file = "../../tables/Table3.tex")
