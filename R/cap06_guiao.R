# ==================================================================
#  Capítulo 6 — Causalidade e contrafactual
#  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
#  Cada bloco «#>> nome ... #<<» corresponde a uma caixa do livro.
#  Caso: agregação de freguesias (2013) e participação eleitoral.
#  Dados: Rodrigues & Tavares (2020), Cities 101, 102685.
#  Pacotes: install.packages("haven")
# ==================================================================
options(width = 80)

#>> c06-abrir
library(haven)
d <- read_dta("../dados/freguesias_participacao.dta")
d <- as.data.frame(zap_labels(d))
dim(d)
head(d, 5)
#<<

#>> c06-variaveis
names(d) <- c("freguesia", "eleitores", "participacao", "agregada",
              "ano", "urbana")
summary(d[, c("participacao", "agregada", "ano", "urbana")])
d <- d[!is.na(d$participacao), ]
table(d$ano, d$agregada)
#<<

#>> c06-medias
round(tapply(d$participacao, list(d$ano, d$agregada), mean), 2)
#<<

#>> c06-guardar
a09 <- mean(d$participacao[d$agregada == 1 & d$ano == 2009])
a13 <- mean(d$participacao[d$agregada == 1 & d$ano == 2013])
n09 <- mean(d$participacao[d$agregada == 0 & d$ano == 2009])
n13 <- mean(d$participacao[d$agregada == 0 & d$ano == 2013])
c(a09 = a09, a13 = a13, n09 = n09, n13 = n13)
#<<

#>> c06-antes-depois
a13 - a09
n13 - n09
#<<

#>> c06-com-sem
a13 - n13
a09 - n09
#<<

#>> c06-did
(a13 - a09) - (n13 - n09)
(a13 - n13) - (a09 - n09)
a09 + (n13 - n09)
#<<

#>> c06-regressao
d$pos <- as.integer(d$ano == 2013)
m <- lm(participacao ~ agregada * pos + urbana,
        data = subset(d, ano %in% c(2009, 2013)))
summary(m)
#<<
