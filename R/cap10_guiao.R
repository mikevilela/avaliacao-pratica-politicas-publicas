# ==================================================================
#  Capítulo 10 — Regressão descontínua (RDD)
#  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
#  Cada bloco «#>> nome ... #<<» corresponde a uma caixa do livro.
#  Dados SIMULADOS (ver cap10_simulacao.R). Efeito verdadeiro do
#  escalão A na retenção: -0,050.
#  Pacotes: install.packages(c("haven", "rdrobust", "rddensity",
#           "sandwich", "lmtest"))
# ==================================================================
options(width = 80)

#>> c10-abrir
library(haven)
d <- zap_labels(read_dta("../dados/ias_ase_simulado.dta"))
dim(d)
names(d)
#<<

#>> c10-variaveis
d$folga    <- -d$dist_limiar
d$elegivel <- as.integer(d$folga >= 0)
summary(d[, c("rend_ref", "folga", "elegivel", "ase_a", "retencao")])
#<<

#>> c10-escaloes
table(d$escalao_abono, d$escalao_ase)
#<<

#>> c10-ingenua
library(sandwich); library(lmtest)
m <- lm(retencao ~ ase_a, data = subset(d, escalao_abono <= 2))
coeftest(m, vcov. = vcovHC(m, type = "HC1"))
#<<

#>> c10-densidade
library(rddensity)
summary(rddensity(d$folga, c = 0))
#<<

#>> c10-primeira-fase
library(rdrobust)
summary(rdrobust(d$ase_a, d$folga, c = 0))
#<<

#>> c10-forma-reduzida
summary(rdrobust(d$retencao, d$folga, c = 0))
#<<

#>> c10-difusa
summary(rdrobust(d$retencao, d$folga, c = 0, fuzzy = d$ase_a))
#<<
