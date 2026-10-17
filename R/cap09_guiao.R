# ==================================================================
#  Capítulo 9 — Matching (emparelhamento)
#  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
#  Cada bloco «#>> nome ... #<<» corresponde a uma caixa do livro.
#  Caso: Programa TEIP e os resultados no 3.º ciclo (agrupamentos
#  de escolas). Dados: DGEEC/Infoescolas, DGE e INE.
#  Pacotes: install.packages(c("haven", "sandwich", "lmtest",
#           "Matching", "WeightIt", "marginaleffects"))
# ==================================================================
options(width = 80)

#>> c09-abrir
library(haven)
teip <- zap_labels(read_dta("../dados/teip_agrupamentos.dta"))
X <- c("pct_ase", "conclusao_esp", "ln_alunos", "ln_densidade",
       "centro", "aml", "alentejo", "algarve")
d <- as.data.frame(subset(teip, amostra == 1))
dim(d)
table(d$teip)
#<<

#>> c09-comparar
v <- c("pds", "conclusao", "retencao", X)
round(cbind(TEIP  = colMeans(d[d$teip == 1, v]),
            Nunca = colMeans(d[d$teip == 0, v])), 2)
#<<

#>> c09-logit
f <- teip ~ pct_ase + conclusao_esp + ln_alunos + ln_densidade +
  centro + aml + alentejo + algarve
logit <- glm(f, family = binomial, data = d)
summary(logit)
1 - logit$deviance / logit$null.deviance
#<<

#>> c09-suporte
d$ps <- fitted(logit)
tapply(d$ps, d$teip, summary)
sum(d$teip == 1 & d$ps > max(d$ps[d$teip == 0]))
sum(d$teip == 0 & d$ps < min(d$ps[d$teip == 1]))
#<<

#>> c09-ingenua
library(sandwich); library(lmtest)
m0 <- lm(pds ~ teip, data = d)
coeftest(m0, vcov. = vcovHC(m0, type = "HC1"))
m1 <- lm(pds ~ teip + pct_ase + conclusao_esp + ln_alunos + ln_densidade +
           centro + aml + alentejo + algarve, data = d)
coeftest(m1, vcov. = vcovHC(m1, type = "HC1"))["teip", ]
#<<

#>> c09-vizinho
library(Matching)
mt <- Match(Y = d$pds, Tr = d$teip, X = d$ps, estimand = "ATT", M = 1,
            Var.calc = 2, distance.tolerance = 1e-10)
summary(mt)
#<<

#>> c09-caliper
calip <- 0.25 * sd(d$ps)
calip
d$fora_cal <- 0
repeat {
  s  <- d[d$fora_cal == 0, ]
  ps <- fitted(glm(f, family = binomial, data = s))
  p0 <- ps[s$teip == 0]
  dist2 <- sapply(ps[s$teip == 1], function(p) sort(abs(p0 - p))[2])
  sem_par <- s$codigo[s$teip == 1][dist2 > calip]
  if (length(sem_par) == 0) break
  d$fora_cal[d$codigo %in% sem_par] <- 1
}
table(d$teip, d$fora_cal)
dc <- d[d$fora_cal == 0, ]
dc$ps <- fitted(glm(f, family = binomial, data = dc))
mc <- Match(Y = dc$pds, Tr = dc$teip, X = dc$ps, estimand = "ATT", M = 1,
            Var.calc = 2, distance.tolerance = 1e-10)
summary(mc)
#<<

#>> c09-ipw
library(WeightIt); library(marginaleffects)
w <- weightit(f, data = d, method = "glm", estimand = "ATT")
ipw <- lm_weightit(pds ~ teip, data = d, weightit = w)
summary(ipw)
ipwra <- lm_weightit(pds ~ teip * (pct_ase + conclusao_esp + ln_alunos +
                       ln_densidade + centro + aml + alentejo + algarve),
                     data = d, weightit = w)
avg_comparisons(ipwra, variables = "teip", newdata = subset(d, teip == 1))
#<<

#>> c09-equilibrio-caliper
equilibrio <- function(dd) {
  t1 <- dd$teip == 1; t0 <- dd$teip == 0
  dp <- sapply(dd[, X], function(x) (mean(x[t1]) - mean(x[t0])) /
                 sqrt((var(x[t1]) + var(x[t0])) / 2))
  rv <- sapply(dd[, X], function(x) var(x[t1]) / var(x[t0]))
  round(cbind(dif_padr = dp, razao_var = rv), 3)
}
equilibrio(d)
emp_c <- dc[c(mc$index.treated, mc$index.control), ]
equilibrio(emp_c)
#<<

#>> c09-equilibrio-nn1
emp <- d[c(mt$index.treated, mt$index.control), ]
equilibrio(emp)
#<<
