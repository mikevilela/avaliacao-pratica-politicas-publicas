# ==================================================================
#  Capítulo 7 — Diferenças-em-diferenças
#  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
#  Cada bloco «#>> nome ... #<<» corresponde a uma caixa do livro.
#  Caso: agregação de freguesias (2013) e participação eleitoral.
#  Dados: Rodrigues & Tavares (2020), Data in Brief.
#  Pacotes: install.packages(c("haven", "fixest"))
# ==================================================================
options(width = 80)

#>> c07-abrir
library(haven)
d <- as.data.frame(zap_labels(
  read_dta("../dados/freguesias_participacao.dta")))
names(d) <- c("freguesia", "eleitores", "participacao", "agregada",
              "ano", "urbana")
dim(d)
head(d)
#<<

#>> c07-medias
d <- subset(d, !is.na(participacao))
data.frame(media = tapply(d$participacao, d$ano, mean),
           dp    = tapply(d$participacao, d$ano, sd),
           n     = tapply(d$participacao, d$ano, length))
round(tapply(d$participacao, list(d$ano, d$agregada), mean), 2)
#<<

#>> c07-2x2
library(fixest)
d$pos13 <- as.integer(d$ano == 2013)
summary(feols(participacao ~ agregada * pos13,
              data = subset(d, ano %in% c(2009, 2013)), vcov = "iid"))
#<<

#>> c07-modelo1
d$pos09 <- as.integer(d$ano == 2009)
summary(feols(participacao ~ agregada * pos09 + urbana,
              data = subset(d, ano %in% c(2001, 2009)), vcov = "iid"))
#<<

#>> c07-modelo2
summary(feols(participacao ~ agregada * pos13 + urbana,
              data = subset(d, ano %in% c(2009, 2013)), vcov = "iid"))
#<<

#>> c07-modelo3
d$pos17 <- as.integer(d$ano == 2017)
summary(feols(participacao ~ agregada * pos17 + urbana,
              data = subset(d, ano %in% c(2013, 2017)), vcov = "iid"))
#<<

#>> c07-agrupados
m2c <- feols(participacao ~ agregada * pos13 + urbana,
             data = subset(d, ano %in% c(2009, 2013)), cluster = ~freguesia)
summary(m2c)
confint(m2c)
#<<

#>> c07-continente
d$distrito <- as.integer(substr(d$freguesia, 1, 2))
with(subset(d, distrito > 18), table(ano, agregada))
cont <- subset(d, distrito <= 18)
summary(feols(participacao ~ agregada * pos13 + urbana,
              data = subset(cont, ano %in% c(2009, 2013)), vcov = "iid"))
summary(feols(participacao ~ agregada * pos17 + urbana,
              data = subset(cont, ano %in% c(2013, 2017)), vcov = "iid"))
#<<

#>> c07-efeitos-fixos
d$tratamento <- d$agregada * (d$ano >= 2013)
fe <- feols(participacao ~ tratamento + i(ano, ref = 2001) | freguesia,
            data = d, cluster = ~freguesia)
summary(fe)
#<<

#>> c07-acontecimento
es <- feols(participacao ~ i(ano, agregada, ref = 2009) +
              i(ano, ref = 2001) | freguesia,
            data = d, cluster = ~freguesia)
summary(es)
#<<
