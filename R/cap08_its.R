# ==================================================================
#  Capítulo 8 — Séries temporais interrompidas
#  Caso: Lei da Paridade (Lei Orgânica n.º 3/2006) e mulheres eleitas
#  nos executivos municipais, eleições autárquicas de 1979 a 2017
#  Dados: Rodrigues (Local Government Studies)
#
#  Versão em R do código Stata do capítulo. Reproduz:
#    - a série da Tabela «Mulheres eleitas para as câmaras municipais»;
#    - os três modelos da Tabela de resultados (MQO com erros-padrão
#      robustos, MQO com erros-padrão de Newey-West, Prais-Winsten);
#    - as combinações b2+b3 (efeito em 2009) e b1+b3 (declive pós-lei);
#    - os testes de Durbin-Watson, Breusch-Godfrey e Durbin alternativo;
#    - a tabela de valores previstos e contrafactuais e a figura;
#    - a análise complementar de Poisson com efeitos fixos (municípios).
#
#  Execute a partir da pasta R/ (os dados estão em ../dados/).
#  Pacotes: haven, sandwich, lmtest, prais, fixest, ggplot2
# ==================================================================

library(haven)
library(sandwich)
library(lmtest)
library(prais)
library(fixest)
library(ggplot2)

dir_dados <- "../dados"
dir_fig   <- "../figuras"
dir.create(dir_fig, showWarnings = FALSE)

# ------------------------------------------------------------------
# 1. Dados e variáveis da regressão segmentada
# ------------------------------------------------------------------
serie <- read_dta(file.path(dir_dados, "paridade_serie.dta"))
serie <- zap_labels(serie)
names(serie)[names(serie) == "TotalMulheres"] <- "mulheres"
names(serie)[names(serie) == "ParLaw"]        <- "paridade"
names(serie)[names(serie) == "propostas"]     <- "quotas_vol"
names(serie)[names(serie) == "Total"]         <- "eleitos"
names(serie)[names(serie) == "ProMulheres"]   <- "prop_mulheres"
serie <- as.data.frame(serie[order(serie$time), ])

# Variável dependente em logaritmos: coeficientes lidos como variações
# proporcionais
serie$ln_mulheres <- log(serie$mulheres)
print(serie[, c("year", "time", "mulheres", "eleitos", "prop_mulheres",
                "paridade", "quotas_vol")], row.names = FALSE)

# Tempo desde a intervenção, com referência na última eleição anterior
# à lei (0 até 2005; 1, 2, 3 em 2009, 2013, 2017). Com esta escolha, o
# coeficiente de «paridade» é o salto extrapolado para 2005 e o salto
# em 2009 é paridade + pos_tempo (ver Secção «A escolha do ponto de
# referência»).
serie$pos_tempo <- serie$paridade * (serie$time - 8)

# Função auxiliar: tabela de coeficientes com t de Student (df = n - k)
tabela <- function(b, V, df) {
  se <- sqrt(diag(V))
  t  <- b / se
  p  <- 2 * pt(abs(t), df = df, lower.tail = FALSE)
  round(cbind(coef = b, ep = se, t = t, p = p), 4)
}
# Combinação linear w'b, com erro-padrão e intervalo de confiança a 95%
combinacao <- function(b, V, w, df) {
  est <- sum(w * b)
  se  <- sqrt(as.numeric(t(w) %*% V %*% w))
  tt  <- est / se
  c(estimativa = est, ep = se, t = tt,
    p = 2 * pt(abs(tt), df, lower.tail = FALSE),
    ic_inf = est - qt(0.975, df) * se, ic_sup = est + qt(0.975, df) * se)
}

# ------------------------------------------------------------------
# 2. Os três modelos
# ------------------------------------------------------------------
f   <- ln_mulheres ~ time + paridade + pos_tempo
m1  <- lm(f, data = serie)
n   <- nobs(m1); k <- length(coef(m1)); gl <- n - k

# Modelo 1: MQO com erros-padrão robustos (HC1 = vce(robust) do Stata)
V1 <- vcovHC(m1, type = "HC1")
cat("\nModelo 1 — MQO, erros-padrão robustos\n")
print(tabela(coef(m1), V1, gl))
cat("R2 =", round(summary(m1)$r.squared, 3), "\n")
cat("F(3,7) robusto =",
    round(waldtest(m1, vcov = V1, test = "F")$F[2], 2), "\n")

# Modelo 2: erros-padrão de Newey-West com 1 desfasamento
# (núcleo de Bartlett, sem pré-branqueamento, correção n/(n-k))
V2 <- NeweyWest(m1, lag = 1, prewhite = FALSE, adjust = TRUE)
cat("\nModelo 2 — MQO, erros-padrão de Newey-West (1 desfasamento)\n")
print(tabela(coef(m1), V2, gl))
cat("F(3,7) =", round(waldtest(m1, vcov = V2, test = "F")$F[2], 2), "\n")

# Modelo 3: Prais-Winsten com erro AR(1), iterado até à convergência
m3 <- prais_winsten(f, data = serie, index = "time", twostep = FALSE,
                    max_iter = 100, tol = 1e-8)
s3 <- summary(m3)
cat("\nModelo 3 — Prais-Winsten AR(1)\n")
print(round(s3$coefficients, 4))
rho <- as.numeric(tail(m3$rho, 1))
cat("rho =", round(rho, 4), "\n")
print(s3)   # inclui R2 e estatística F do modelo transformado

# Durbin-Watson do modelo transformado (Prais-Winsten)
r3  <- residuals(m3)
e_t <- c(sqrt(1 - rho^2) * r3[1], r3[-1] - rho * r3[-length(r3)])
cat("Durbin-Watson (transformado) =",
    round(sum(diff(e_t)^2) / sum(e_t^2), 3), "\n")

# Tabela-resumo dos três modelos (coeficientes com 3 casas decimais)
resumo <- cbind(
  "(1) MQO robusto"   = coef(m1),
  "(2) Newey-West"    = coef(m1),
  "(3) Prais-Winsten" = coef(m3))
ep <- cbind(sqrt(diag(V1)), sqrt(diag(V2)), s3$coefficients[, 2])
cat("\nCoeficientes\n"); print(round(resumo, 3))
cat("Erros-padrão\n");  print(round(ep, 3))

# ------------------------------------------------------------------
# 3. Efeito em 2009 (b2 + b3) e declive pós-lei (b1 + b3), modelo 1
# ------------------------------------------------------------------
w_2009    <- c(0, 0, 1, 1)   # paridade + pos_tempo
w_declive <- c(0, 1, 0, 1)   # time + pos_tempo
cat("\nEfeito em 2009 (b2+b3), modelo 1:\n")
print(round(combinacao(coef(m1), V1, w_2009, gl), 4))
cat("Declive pós-lei (b1+b3), modelo 1:\n")
print(round(combinacao(coef(m1), V1, w_declive, gl), 4))
cat("Mesmas combinações nos modelos 2 e 3 (valor pontual):\n")
print(round(c(nw_2009 = sum(w_2009 * coef(m1)),
              pw_2009 = sum(w_2009 * coef(m3)),
              nw_decl = sum(w_declive * coef(m1)),
              pw_decl = sum(w_declive * coef(m3))), 3))

# ------------------------------------------------------------------
# 4. Diagnóstico de autocorrelação dos resíduos do modelo 1
# ------------------------------------------------------------------
cat("\nDurbin-Watson (modelo 1):",
    round(dwtest(m1)$statistic, 3), "\n")
# Breusch-Godfrey (1 desfasamento; resíduo desfasado em falta = 0)
print(bgtest(m1, order = 1, type = "Chisq", fill = 0))
# Teste alternativo de Durbin: regressão dos resíduos nas variáveis
# explicativas e no resíduo desfasado; qui-quadrado = t^2
serie$e     <- residuals(m1)
serie$e_lag <- c(0, head(serie$e, -1))
aux <- lm(e ~ time + paridade + pos_tempo + e_lag, data = serie)
t_e <- coef(summary(aux))["e_lag", "t value"]
cat("Durbin alternativo: chi2(1) =", round(t_e^2, 3),
    " p =", round(pchisq(t_e^2, 1, lower.tail = FALSE), 4), "\n")

# ------------------------------------------------------------------
# 5. Valores previstos, contrafactual e efeito (Tabela do contrafactual)
# ------------------------------------------------------------------
b <- coef(m1)
serie$prev    <- exp(fitted(m1))
serie$contraf <- ifelse(serie$time >= 8,
                        exp(b["(Intercept)"] + b["time"] * serie$time), NA)
serie$efeito_log <- ifelse(serie$time >= 9,
                           b["paridade"] + b["pos_tempo"] * (serie$time - 8), NA)
serie$efeito_pct <- 100 * (serie$prev / serie$contraf - 1)
cat("\nEfeito em 2009: ", round(b["paridade"] + b["pos_tempo"], 3),
    " pontos logarítmicos, ou ",
    round(100 * (exp(b["paridade"] + b["pos_tempo"]) - 1), 1),
    "% face ao contrafactual\n", sep = "")
print(round(serie[serie$time >= 8,
                  c("year", "mulheres", "prev", "contraf",
                    "efeito_log", "efeito_pct")], 3), row.names = FALSE)

# ------------------------------------------------------------------
# 6. Figura: observado, previsto e contrafactual
# ------------------------------------------------------------------
azul <- rgb(27, 54, 93, maxColorValue = 255)
ocre <- rgb(192, 121, 47, maxColorValue = 255)
serie$segmento <- ifelse(serie$time <= 8, "antes", "depois")
g <- ggplot(serie, aes(x = year)) +
  geom_vline(xintercept = 2007, colour = "grey60", linetype = "dotted") +
  geom_line(aes(y = prev, group = segmento, colour = "Previsto pelo modelo")) +
  geom_line(data = subset(serie, !is.na(contraf)),
            aes(y = contraf, colour = "Contrafactual (tendência pré-lei)"),
            linetype = "dashed") +
  geom_point(aes(y = mulheres, colour = "Observado"), size = 2.2) +
  scale_colour_manual(NULL,
    values = c("Observado" = azul, "Previsto pelo modelo" = azul,
               "Contrafactual (tendência pré-lei)" = ocre),
    breaks = c("Observado", "Previsto pelo modelo",
               "Contrafactual (tendência pré-lei)")) +
  scale_x_continuous(breaks = c(1979, 1985, 1993, 2001, 2009, 2017)) +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100)) +
  labs(x = "Eleição autárquica", y = "Mulheres eleitas (n.º)") +
  theme_classic(base_family = "serif") +
  theme(legend.position = "bottom")
ggsave(file.path(dir_fig, "c09_its_R.png"), g, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------
# 7. Análise complementar: Poisson com efeitos fixos de município
# ------------------------------------------------------------------
painel <- zap_labels(read_dta(file.path(dir_dados, "paridade_municipios.dta")))
names(painel)[names(painel) == "Women_in_office"] <- "mulheres"
names(painel)[names(painel) == "GenderQuotas"]    <- "paridade"
painel$pos_tempo <- painel$paridade * (painel$time - 8)
# Os municípios que nunca elegeram uma mulher não contribuem para a
# verosimilhança condicional e são excluídos automaticamente.
mp <- fepois(mulheres ~ time + paridade + pos_tempo | id, data = painel,
             cluster = ~id)
print(summary(mp))
irr <- cbind(IRR = exp(coef(mp)),
             ep  = exp(coef(mp)) * se(mp),          # método delta, como o Stata
             ic_inf = exp(confint(mp)[, 1]),
             ic_sup = exp(confint(mp)[, 2]))
cat("\nRazões de taxas de incidência (erros-padrão agrupados por município)\n")
print(round(irr, 4))
cat("Observações:", nobs(mp), " Municípios:", length(unique(painel$id)) -
      length(mp$fixef_removed$id), "\n")
cat("Efeito em 2009 (IRR paridade x pos_tempo):",
    round(exp(coef(mp)["paridade"] + coef(mp)["pos_tempo"]), 3), "\n")
cat("Ritmo pós-lei (IRR time x pos_tempo):",
    round(exp(coef(mp)["time"] + coef(mp)["pos_tempo"]), 3), "\n")
