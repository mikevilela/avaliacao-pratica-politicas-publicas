# ==================================================================
#  Capítulo 8 — Séries temporais interrompidas
#  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
#  Cada bloco «#>> nome ... #<<» corresponde a uma caixa do livro.
#  Caso: Lei da Paridade (Lei Orgânica n.º 3/2006) e mulheres eleitas
#  nos executivos municipais, eleições autárquicas de 1979 a 2017.
#  Pacotes: install.packages(c("haven", "sandwich", "lmtest",
#           "prais", "fixest"))
# ==================================================================
options(width = 80)

#>> c08-abrir
library(haven)
s <- zap_labels(read_dta("../dados/paridade_serie.dta"))
s <- as.data.frame(s)
s
#<<

#>> c08-serie
names(s) <- c("year", "mulheres", "time", "paridade", "quotas_vol",
              "eleitos", "prop_mulheres")
s <- s[order(s$time), ]
s$ln_mulheres <- log(s$mulheres)
s$dif    <- c(NA, diff(s$mulheres))
s$dif_ln <- c(NA, diff(s$ln_mulheres))
print(s[, c("year", "time", "mulheres", "dif", "ln_mulheres", "dif_ln")],
      row.names = FALSE, digits = 4)
#<<

#>> c08-variaveis
s$pos_tempo <- s$paridade * (s$time - 8)
print(s[, c("year", "time", "paridade", "pos_tempo")], row.names = FALSE)
#<<

#>> c08-mqo
library(sandwich); library(lmtest)
m1 <- lm(ln_mulheres ~ time + paridade + pos_tempo, data = s)
V1 <- vcovHC(m1, type = "HC1")
coeftest(m1, vcov. = V1)
summary(m1)$r.squared
waldtest(m1, vcov = V1, test = "F")
#<<

#>> c08-combinacoes
combinar <- function(w) {
  est <- sum(w * coef(m1))
  ep  <- sqrt(drop(t(w) %*% V1 %*% w))
  ic  <- est + c(-1, 1) * qt(0.975, df = m1$df.residual) * ep
  round(c(estimativa = est, ep = ep, t = est / ep,
          p = 2 * pt(-abs(est / ep), df = m1$df.residual),
          ic_inf = ic[1], ic_sup = ic[2]), 4)
}
combinar(c(0, 0, 1, 1))   # paridade + pos_tempo
combinar(c(0, 1, 0, 1))   # time + pos_tempo
#<<

#>> c08-autocorrelacao
dwtest(m1)
bgtest(m1, order = 1, type = "Chisq", fill = 0)
s$e     <- residuals(m1)
s$e_lag <- c(0, head(s$e, -1))
aux <- lm(e ~ time + paridade + pos_tempo + e_lag, data = s)
t_e <- coef(summary(aux))["e_lag", "t value"]
c(chi2 = t_e^2, p = pchisq(t_e^2, df = 1, lower.tail = FALSE))
#<<

#>> c08-newey
V2 <- NeweyWest(m1, lag = 1, prewhite = FALSE, adjust = TRUE)
coeftest(m1, vcov. = V2)
waldtest(m1, vcov = V2, test = "F")
#<<

#>> c08-prais
library(prais)
m3 <- prais_winsten(ln_mulheres ~ time + paridade + pos_tempo, data = s,
                    index = "time", max_iter = 100, tol = 1e-8)
summary(m3)
#<<

#>> c08-contrafactual
b <- coef(m1)
s$prev    <- exp(fitted(m1))
s$contraf <- ifelse(s$time >= 8, exp(b["(Intercept)"] + b["time"] * s$time),
                    NA)
print(s[s$time >= 8, c("year", "mulheres", "prev", "contraf")],
      row.names = FALSE)
#<<

#>> c08-painel
library(fixest)
p <- zap_labels(read_dta("../dados/paridade_municipios.dta"))
p$mulheres  <- p$Women_in_office
p$paridade  <- p$GenderQuotas
p$pos_tempo <- p$paridade * (p$time - 8)
mp <- fepois(mulheres ~ time + paridade + pos_tempo | id, data = p,
             cluster = ~id)
summary(mp)
round(cbind(IRR = exp(coef(mp)), ep = exp(coef(mp)) * se(mp)), 4)
#<<
