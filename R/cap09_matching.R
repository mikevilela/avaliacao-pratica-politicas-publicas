# ==================================================================
#  Capítulo 9 — Matching (emparelhamento)
#  Caso: Territórios Educativos de Intervenção Prioritária (TEIP)
#  Unidades: agrupamentos de escolas públicas do continente
#  Dados: DGEEC/Infoescolas (3.º ciclo, 2020/21 a 2022/23); listas TEIP
#         da DGE (2021 e 2024); INE (densidade populacional, 2011)
#
#  Versão em R do código Stata do capítulo. Reproduz:
#    1. a tabela descritiva antes do matching (médias, diferenças
#       padronizadas, razões de variâncias);
#    2. a diferença simples e a regressão com covariáveis;
#    3. o propensity score (logit), a área sob a curva ROC e o suporte
#       comum, com a figura da distribuição do score;
#    4. o teste de equilíbrio por blocos de Becker e Ichino;
#    5. as estimativas do ATT (vizinho mais próximo, cinco vizinhos,
#       caliper, Mahalanobis com correção do enviesamento, kernel, IPW,
#       IPWRA, IPWRA no suporte comum, IPWRA com score flexível);
#    6. os diagnósticos de equilíbrio e a figura das diferenças
#       padronizadas antes e depois do matching com caliper;
#    7. os limites de Rosenbaum;
#    8. o teste placebo com os novos agrupamentos TEIP4;
#    9. a análise de heterogeneidade por geração do programa.
#
#  Notas sobre a comparação com o Stata:
#    - os estimadores de vizinho mais próximo usam o pacote Matching,
#      com empates incluídos (como no Stata); as estimativas pontuais
#      coincidem, mas os erros-padrão de Abadie e Imbens calculados aqui
#      não incluem a correção pela estimação do score (Abadie e Imbens,
#      2016), que o Stata aplica; podem por isso diferir um pouco;
#    - o IPW e o IPWRA são programados diretamente, com erros-padrão
#      de sanduíche (método dos momentos) que têm em conta a estimação
#      do score, como no Stata;
#    - o kernel é programado diretamente (Epanechnikov, largura 0,06,
#      suporte comum) e o bootstrap usa outra sequência aleatória, pelo
#      que o erro-padrão difere ligeiramente;
#    - os pares sem reposição dos limites de Rosenbaum dependem da ordem
#      e das regras de emparelhamento, que não são exatamente as do Stata
#      (aqui formam-se 104 pares; no Stata, 102); os limites são
#      programados diretamente (teste de postos sinalizados de Wilcoxon,
#      aproximação normal, e estimativa de Hodges-Lehmann).
#
#  Execute a partir da pasta R/ (os dados estão em ../dados/).
#  Pacotes: haven, sandwich, lmtest, Matching, numDeriv, ggplot2
# ==================================================================

library(haven)
library(sandwich)
library(lmtest)
library(Matching)
library(numDeriv)
library(ggplot2)

set.seed(20240716)
dir_dados <- "../dados"
dir_fig   <- "../figuras"
dir.create(dir_fig, showWarnings = FALSE)

# ------------------------------------------------------------------
# 0. Dados, grupos e amostras
# ------------------------------------------------------------------
# O ficheiro já contém as covariáveis derivadas (ln_alunos, ln_densidade,
# indicadores de região) e as variáveis de grupo e de amostra:
#   grupo = 0 nunca TEIP, 1 TEIP (lista de 2021), 2 novo TEIP4 (2024)
#   amostra    = agrupamentos TEIP e nunca TEIP sem valores em falta
#   amostra_pl = novos TEIP4 e nunca TEIP sem valores em falta
teip <- as.data.frame(zap_labels(read_dta(file.path(dir_dados,
                                                    "teip_agrupamentos.dta"))))
X  <- c("pct_ase", "conclusao_esp", "ln_alunos", "ln_densidade",
        "centro", "aml", "alentejo", "algarve")
Xc <- c("pct_ase", "conclusao_esp", "ln_alunos", "ln_densidade")  # contínuas
Y  <- c("conclusao", "pds", "retencao")

d <- teip[teip$amostra == 1, ]
cat("Amostra principal:", nrow(d), "agrupamentos,", sum(d$teip), "TEIP\n")
print(table(fase = d$fase[d$teip == 1]))

# Funções auxiliares ------------------------------------------------
wmean <- function(x, w) sum(w * x) / sum(w)
wvar  <- function(x, w) {                 # variância ponderada (pesos de frequência)
  m <- wmean(x, w); sum(w * (x - m)^2) / (sum(w) - 1)
}
# Diferença padronizada (%) e razão de variâncias, com pesos opcionais
equilibrio <- function(dd, trat, w = rep(1, nrow(dd)), vars = X) {
  t(sapply(vars, function(v) {
    x1 <- dd[[v]][dd[[trat]] == 1]; w1 <- w[dd[[trat]] == 1]
    x0 <- dd[[v]][dd[[trat]] == 0]; w0 <- w[dd[[trat]] == 0]
    m1 <- wmean(x1, w1); m0 <- wmean(x0, w0)
    v1 <- wvar(x1, w1);  v0 <- wvar(x0, w0)
    c(media_trat = m1, media_contr = m0, diferenca = m1 - m0,
      dif_padr = 100 * (m1 - m0) / sqrt((v1 + v0) / 2), razao_var = v1 / v0)
  }))
}
# Linha-resumo de uma estimativa
resultados <- data.frame()
regista <- function(rotulo, est, ep, n) {
  resultados <<- rbind(resultados,
    data.frame(estimativa = rotulo, coef = est, ep = ep, z = est / ep, N = n))
  cat(sprintf("RES|%-24s|%9.3f|%9.3f|%9.3f|%d\n", rotulo, est, ep, est / ep, n))
}
# Fórmula do modelo do score
f_ps <- function(trat, vars = X) reformulate(vars, response = trat)

# ------------------------------------------------------------------
# 1. Descrição antes do matching
# ------------------------------------------------------------------
cat("\nTabela descritiva (TEIP vs. nunca TEIP)\n")
print(round(equilibrio(d, "teip", vars = c(Y, X)), 2))

# ------------------------------------------------------------------
# 2. Diferença simples e regressão com covariáveis (EP robustos HC1)
# ------------------------------------------------------------------
for (y in Y) {
  m0 <- lm(reformulate("teip", y), data = d)
  regista(paste0("ingenua_", y), coef(m0)["teip"],
          sqrt(vcovHC(m0, "HC1")["teip", "teip"]), nobs(m0))
  m1 <- lm(reformulate(c("teip", X), y), data = d)
  regista(paste0("mqo_", y), coef(m1)["teip"],
          sqrt(vcovHC(m1, "HC1")["teip", "teip"]), nobs(m1))
}

# ------------------------------------------------------------------
# 3. Propensity score: modelo logit e suporte comum
# ------------------------------------------------------------------
logit <- glm(f_ps("teip"), family = binomial, data = d)
print(summary(logit))
d$ps <- fitted(logit)
pseudo_r2 <- function(m) 1 - m$deviance / m$null.deviance
cat("Pseudo-R2 =", round(pseudo_r2(logit), 4), "\n")
# Área sob a curva ROC (estatística de Mann-Whitney)
auc <- function(p, t) {
  r <- rank(p); n1 <- sum(t == 1); n0 <- sum(t == 0)
  (sum(r[t == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
cat("Área sob a curva ROC =", round(auc(d$ps, d$teip), 4), "\n")
cat("Mediana do score: TEIP =", round(median(d$ps[d$teip == 1]), 3),
    "; nunca TEIP =", round(median(d$ps[d$teip == 0]), 3), "\n")
cat("TEIP com score > 0,9:", sum(d$teip == 1 & d$ps > 0.9),
    "; nunca TEIP com score > 0,9:", sum(d$teip == 0 & d$ps > 0.9), "\n")
# Suporte comum: critério mínimo-máximo
ps_min1 <- min(d$ps[d$teip == 1]); ps_max0 <- max(d$ps[d$teip == 0])
cat(sprintf("SUP|min tratados=%.4f|max controlos=%.4f\n", ps_min1, ps_max0))
cat("Tratados acima do máximo dos controlos:", sum(d$teip == 1 & d$ps > ps_max0), "\n")
cat("Controlos abaixo do mínimo dos tratados:", sum(d$teip == 0 & d$ps < ps_min1), "\n")
d$suporte <- d$ps >= ps_min1 & d$ps <= ps_max0

# Figura: histograma espelhado do score (intervalos de 0,025)
azul <- rgb(27, 54, 93, maxColorValue = 255)
ocre <- rgb(192, 121, 47, maxColorValue = 255)
hist_d <- aggregate(list(n = d$ps),
                    list(bin = floor(d$ps / 0.025) * 0.025 + 0.0125, teip = d$teip),
                    length)
hist_d$n[hist_d$teip == 0] <- -hist_d$n[hist_d$teip == 0]
hist_d$grupo <- factor(hist_d$teip, levels = c(1, 0),
                       labels = c("TEIP (acima do eixo)", "Nunca TEIP (abaixo do eixo)"))
g1 <- ggplot(hist_d, aes(x = bin, y = n, fill = grupo)) +
  geom_col(width = 0.022) +
  geom_hline(yintercept = 0, colour = "grey50") +
  scale_fill_manual(NULL, values = c(azul, ocre)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(-100, 25, 25), labels = abs, limits = c(-100, 25)) +
  labs(x = "Propensity score", y = "Número de agrupamentos") +
  theme_classic(base_family = "serif") + theme(legend.position = "bottom")
ggsave(file.path(dir_fig, "c10_pscore_R.png"), g1, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------
# 4. Teste de equilíbrio por blocos (algoritmo de Becker e Ichino)
# ------------------------------------------------------------------
# Suporte comum: score entre o mínimo e o máximo dos tratados. Blocos
# iniciais de 0,2; um bloco divide-se ao meio enquanto o score médio
# diferir entre tratados e controlos (teste t, 1%). Depois testa-se cada
# covariável dentro de cada bloco (teste t, 1%).
bi <- d[d$ps >= ps_min1 & d$ps <= max(d$ps[d$teip == 1]), ]
cortes <- c(ps_min1, 0.2, 0.4, 0.6, 0.8, 1)
repeat {
  bi$bloco <- cut(bi$ps, cortes, include.lowest = TRUE, right = FALSE, labels = FALSE)
  novos <- cortes
  for (b in seq_len(length(cortes) - 1)) {
    s <- bi[bi$bloco == b, ]
    if (sum(s$teip == 1) < 2 || sum(s$teip == 0) < 2) next
    p <- t.test(ps ~ teip, data = s, var.equal = TRUE)$p.value
    if (p < 0.01) novos <- sort(c(novos, (cortes[b] + cortes[b + 1]) / 2))
  }
  if (length(novos) == length(cortes)) break
  cortes <- novos
}
cat("\nBecker-Ichino: número de blocos =", length(cortes) - 1, "\n")
for (b in sort(unique(bi$bloco))) {
  s <- bi[bi$bloco == b, ]
  if (sum(s$teip == 1) < 2 || sum(s$teip == 0) < 2) next
  for (v in X) {
    if (var(s[[v]]) == 0) next
    p <- tryCatch(t.test(reformulate("teip", v), data = s, var.equal = TRUE)$p.value,
                  error = function(e) NA)
    if (!is.na(p) && p < 0.01) cat("  Variável", v, "não equilibrada no bloco", b, "\n")
  }
}

# ------------------------------------------------------------------
# 5. Estimação do ATT
# ------------------------------------------------------------------
# 5.1-5.2 Vizinho mais próximo no score (com reposição, empates incluídos)
emparelha <- function(dd, y, trat = "teip", M = 1) {
  ps <- fitted(glm(f_ps(trat), family = binomial, data = dd))
  Match(Y = dd[[y]], Tr = dd[[trat]], X = ps, estimand = "ATT", M = M,
        ties = TRUE, replace = TRUE, Var.calc = 2, distance.tolerance = 1e-10)
}
for (M in c(1, 5)) for (y in Y) {
  m <- emparelha(d, y, M = M)
  regista(paste0("nn", M, "_", y), m$est, m$se, nrow(d))
}

# Pesos de frequência dos controlos num matching (para o equilíbrio)
pesos_match <- function(m, n) {
  w <- numeric(n)
  w[unique(m$index.treated)] <- 1
  agg <- tapply(m$weights, m$index.control, sum)
  w[as.integer(names(agg))] <- w[as.integer(names(agg))] + agg
  w
}
m_nn1 <- emparelha(d, "conclusao", M = 1)
cat("\nEquilíbrio depois do vizinho mais próximo 1:1\n")
print(round(equilibrio(d, "teip", pesos_match(m_nn1, nrow(d)))[, c("dif_padr", "razao_var")], 2))

# 5.3 Vizinho mais próximo com caliper (0,25 desvios-padrão do score).
#     Exclui iterativamente os tratados sem par dentro do caliper e
#     reestima o score sem eles, como no capítulo. Tal como no Stata,
#     que precisa de dois vizinhos para estimar a variância condicional,
#     um tratado só fica na amostra se tiver pelo menos dois controlos
#     dentro do caliper.
calip <- 0.25 * sd(d$ps)
cat(sprintf("CAL|caliper=%.4f\n", calip))
d$fora_cal <- 0
repeat {
  s  <- d[d$fora_cal == 0, ]
  ps <- fitted(glm(f_ps("teip"), family = binomial, data = s))
  p0 <- ps[s$teip == 0]
  dist_min <- sapply(ps[s$teip == 1], function(p) sort(abs(p0 - p))[2])
  sem_par <- s$codigo[s$teip == 1][dist_min > calip]
  if (length(sem_par) == 0) break
  d$fora_cal[d$codigo %in% sem_par] <- 1
}
cat("Tratados excluídos pelo caliper:", sum(d$fora_cal), "\n")
cat("Médias dos tratados incluídos (0) e excluídos (1):\n")
print(round(aggregate(d[d$teip == 1, c("ps", "pct_ase", "conclusao_esp", "ln_densidade")],
                      list(fora_cal = d$fora_cal[d$teip == 1]), mean), 2))
dc <- d[d$fora_cal == 0, ]
for (y in Y) {
  m <- emparelha(dc, y, M = 1)   # todos os tratados têm par dentro do caliper
  regista(paste0("cal_", y), m$est, m$se, nrow(dc))
}
m_cal <- emparelha(dc, "conclusao", M = 1)
eq_cal <- equilibrio(dc, "teip", pesos_match(m_cal, nrow(dc)))
cat("\nEquilíbrio depois do matching com caliper\n")
print(round(eq_cal[, c("dif_padr", "razao_var")], 2))

# 5.4 Matching nas covariáveis (distância de Mahalanobis) com correção
#     do enviesamento nas quatro covariáveis contínuas
for (y in Y) {
  m <- Match(Y = d[[y]], Tr = d$teip, X = as.matrix(d[, X]), Z = as.matrix(d[, Xc]),
             estimand = "ATT", M = 1, Weight = 2, BiasAdjust = TRUE,
             ties = TRUE, Var.calc = 2, distance.tolerance = 1e-10)
  regista(paste0("maha_", y), m$est, m$se, nrow(d))
}

# 5.5 IPW e IPWRA para o ATT, com erros-padrão de sanduíche que têm em
#     conta a estimação do score (equações de estimação empilhadas)
att_ponderado <- function(dd, y, trat = "teip", vars_ps = X, ra = FALSE) {
  Z  <- cbind(1, as.matrix(dd[, vars_ps]))
  W  <- cbind(1, as.matrix(dd[, X]))
  Tr <- dd[[trat]]; Yv <- dd[[y]]
  kz <- ncol(Z); kw <- ncol(W)
  g0 <- coef(glm(f_ps(trat, vars_ps), family = binomial, data = dd))
  p0 <- plogis(drop(Z %*% g0)); o0 <- p0 / (1 - p0)
  if (!ra) {
    th <- c(g0, mean(Yv[Tr == 1]), sum(o0 * Yv * (1 - Tr)) / sum(o0 * (1 - Tr)))
    momentos <- function(th) {
      p <- plogis(drop(Z %*% th[1:kz])); o <- p / (1 - p)
      cbind((Tr - p) * Z, Tr * (Yv - th[kz + 1]), (1 - Tr) * o * (Yv - th[kz + 2]))
    }
    grad <- c(rep(0, kz), 1, -1)
  } else {
    b1 <- coef(lm.fit(W[Tr == 1, ], Yv[Tr == 1]))
    b0 <- coef(lm.wfit(W[Tr == 0, ], Yv[Tr == 0], o0[Tr == 0]))
    tau <- mean(drop(W[Tr == 1, ] %*% (b1 - b0)))
    th <- c(g0, b1, b0, tau)
    momentos <- function(th) {
      p  <- plogis(drop(Z %*% th[1:kz])); o <- p / (1 - p)
      bb1 <- th[kz + 1:kw]; bb0 <- th[kz + kw + 1:kw]
      cbind((Tr - p) * Z,
            Tr * drop(Yv - W %*% bb1) * W,
            (1 - Tr) * o * drop(Yv - W %*% bb0) * W,
            Tr * (drop(W %*% (bb1 - bb0)) - th[length(th)]))
    }
    grad <- c(rep(0, length(th) - 1), 1)
  }
  n  <- nrow(dd)
  G  <- jacobian(function(t) colMeans(momentos(t)), th)
  S  <- crossprod(momentos(th)) / n
  Gi <- solve(G)
  V  <- Gi %*% S %*% t(Gi) / n
  c(est = sum(grad * th), ep = sqrt(drop(t(grad) %*% V %*% grad)))
}
for (y in Y) {
  r <- att_ponderado(d, y);            regista(paste0("ipw_", y), r[1], r[2], nrow(d))
  r <- att_ponderado(d, y, ra = TRUE); regista(paste0("ipwra_", y), r[1], r[2], nrow(d))
}
# Equilíbrio depois do IPW (controlos ponderados por p/(1-p))
w_ipw <- ifelse(d$teip == 1, 1, d$ps / (1 - d$ps))
cat("\nEquilíbrio depois do IPW\n")
print(round(equilibrio(d, "teip", w_ipw)[, c("dif_padr", "razao_var")], 2))
# IPWRA só no suporte comum (critério mínimo-máximo)
ds <- d[d$suporte, ]
for (y in Y) {
  r <- att_ponderado(ds, y, ra = TRUE); regista(paste0("ipwra_sup_", y), r[1], r[2], nrow(ds))
}

# 5.6 Kernel de Epanechnikov (largura 0,06), no suporte comum; erros-
#     padrão por bootstrap (500 replicações, reestimando o score)
kernel_att <- function(dd, y, h = 0.06, devolve_pesos = FALSE) {
  ps <- fitted(glm(f_ps("teip"), family = binomial, data = dd))
  Tr <- dd$teip
  sup <- Tr == 0 | (ps >= min(ps[Tr == 0]) & ps <= max(ps[Tr == 0]))
  i1 <- which(Tr == 1 & sup); i0 <- which(Tr == 0)
  u  <- outer(ps[i1], ps[i0], "-") / h
  K  <- ifelse(abs(u) < 1, 1 - u^2, 0)
  ok <- rowSums(K) > 0
  K  <- K[ok, , drop = FALSE] / rowSums(K[ok, , drop = FALSE])
  att <- mean(dd[[y]][i1[ok]] - drop(K %*% dd[[y]][i0]))
  if (!devolve_pesos) return(att)
  w <- numeric(nrow(dd)); w[i1[ok]] <- 1; w[i0] <- colSums(K)
  list(att = att, pesos = w, n_trat = sum(ok))
}
for (y in Y) {
  est  <- kernel_att(d, y)
  boot <- replicate(500, kernel_att(d[sample(nrow(d), replace = TRUE), ], y))
  regista(paste0("kernel_", y), est, sd(boot), nrow(d))
}
# Diagnóstico de equilíbrio do kernel: enviesamento padronizado médio
# (variâncias da amostra completa no denominador) e pseudo-R2 de um logit
# estimado na amostra ponderada
k <- kernel_att(d, "conclusao", devolve_pesos = TRUE)
cat("\nKernel: tratados no suporte comum =", k$n_trat, "\n")
vies <- function(w) {
  sapply(X, function(v) {
    m1 <- wmean(d[[v]][d$teip == 1], w[d$teip == 1])
    m0 <- wmean(d[[v]][d$teip == 0], w[d$teip == 0])
    100 * (m1 - m0) / sqrt((var(d[[v]][d$teip == 1]) + var(d[[v]][d$teip == 0])) / 2)
  })
}
cat("Enviesamento padronizado médio: antes =", round(mean(abs(vies(rep(1, nrow(d))))), 1),
    "; depois =", round(mean(abs(vies(k$pesos))), 1), "\n")
dk <- d[k$pesos > 0, ]
lk <- suppressWarnings(glm(f_ps("teip"), family = binomial, data = dk,
                           weights = k$pesos[k$pesos > 0]))
cat("Pseudo-R2: antes =", round(pseudo_r2(logit), 3),
    "; depois =", round(pseudo_r2(lk), 3), "\n")

# ------------------------------------------------------------------
# 6. Figura: diferenças padronizadas antes e depois (caliper)
# ------------------------------------------------------------------
rotulos <- c("Alunos com ASE (%)", "Conclusão esperada (%)", "Dimensão (ln alunos)",
             "Densidade (ln)", "Centro", "AM Lisboa", "Alentejo", "Algarve")
fig <- rbind(
  data.frame(var = rotulos, dif = equilibrio(d, "teip")[, "dif_padr"],
             momento = "Antes do matching"),
  data.frame(var = rotulos, dif = eq_cal[, "dif_padr"],
             momento = "Depois (vizinho mais próximo com caliper)"))
fig$var <- factor(fig$var, levels = rev(rotulos))
g2 <- ggplot(fig, aes(x = dif, y = var, colour = momento, shape = momento)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_vline(xintercept = c(-10, 10), colour = "grey75", linetype = "dashed") +
  geom_point(size = 3) +
  scale_colour_manual(NULL, values = c(ocre, azul)) +
  scale_shape_manual(NULL, values = c(16, 18)) +
  scale_x_continuous(limits = c(-120, 120), breaks = seq(-120, 120, 20)) +
  labs(x = "Diferença padronizada (%)", y = NULL) +
  theme_classic(base_family = "serif") +
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave(file.path(dir_fig, "c10_equilibrio_R.png"), g2, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------
# 7. Limites de Rosenbaum: pares 1:1 sem reposição, dentro do caliper
# ------------------------------------------------------------------
# Emparelhamento guloso, pela ordem dos dados, no score da amostra
# principal, só para tratados no suporte comum.
i1 <- which(d$teip == 1 & d$ps <= ps_max0)
disponivel <- d$teip == 0
pares <- matrix(NA_integer_, 0, 2)
for (i in i1) {
  cand <- which(disponivel)
  dist <- abs(d$ps[cand] - d$ps[i])
  if (min(dist) <= calip) {
    j <- cand[which.min(dist)]
    pares <- rbind(pares, c(i, j)); disponivel[j] <- FALSE
  }
}
cat("\nLimites de Rosenbaum: número de pares =", nrow(pares), "\n")
# Limites de Rosenbaum para o teste de postos sinalizados de Wilcoxon
# sobre as diferenças entre pares. Para cada Gamma, a probabilidade de
# a diferença de um par ser positiva varia entre 1/(1+Gamma) e
# Gamma/(1+Gamma). Os níveis de significância são unilaterais, no
# sentido da estimativa; a estimativa de Hodges-Lehmann é o desvio s
# que torna a estatística de (dif - s) igual ao seu valor esperado.
estat_w <- function(dif, s, p) {
  x <- dif - s; x <- x[x != 0]
  r <- rank(abs(x)); Tp <- sum(r[x > 0])
  (Tp - p * sum(r)) / sqrt(p * (1 - p) * sum(r^2))
}
hl <- function(dif, p) {
  uniroot(function(s) estat_w(dif, s, p), range(dif) + c(-1, 1),
          tol = 1e-8)$root
}
rosenbaum <- function(dif, gammas = seq(1, 3, 0.25)) {
  positivo <- estat_w(dif, 0, 0.5) > 0
  t(sapply(gammas, function(G) {
    pmax <- G / (1 + G); pmin <- 1 / (1 + G)
    z_mais  <- estat_w(dif, 0, pmax)   # estatística sob p máximo
    z_menos <- estat_w(dif, 0, pmin)   # estatística sob p mínimo
    if (positivo) { sig_mais <- pnorm(z_mais, lower.tail = FALSE)
                    sig_menos <- pnorm(z_menos, lower.tail = FALSE) }
    else          { sig_mais <- pnorm(z_mais)
                    sig_menos <- pnorm(z_menos) }
    c(gamma = G, sig_mais = sig_mais, sig_menos = sig_menos,
      hl_mais = hl(dif, pmax), hl_menos = hl(dif, pmin))
  }))
}
for (y in Y) {
  dif <- d[[y]][pares[, 1]] - d[[y]][pares[, 2]]
  cat("\nResultado:", y, " diferença média =", round(mean(dif), 3), "\n")
  print(round(rosenbaum(dif), 4))
}

# ------------------------------------------------------------------
# 8. Teste placebo: agrupamentos que só entraram no TEIP4 (2024/25)
# ------------------------------------------------------------------
pl <- teip[teip$amostra_pl == 1, ]
pl$novo4 <- as.integer(pl$grupo == 2)
cat("\nPlacebo:", nrow(pl), "agrupamentos,", sum(pl$novo4), "novos TEIP4\n")
for (y in Y) {
  m0 <- lm(reformulate("novo4", y), data = pl)
  regista(paste0("pl_ingenua_", y), coef(m0)["novo4"],
          sqrt(vcovHC(m0, "HC1")["novo4", "novo4"]), nrow(pl))
  m <- emparelha(pl, y, trat = "novo4", M = 1)
  regista(paste0("pl_nn1_", y), m$est, m$se, nrow(pl))
  r <- att_ponderado(pl, y, trat = "novo4", ra = TRUE)
  regista(paste0("pl_ipwra_", y), r[1], r[2], nrow(pl))
}

# ------------------------------------------------------------------
# 9. Heterogeneidade: TEIP2 (fases 1 a 3) e entradas de 2012 em diante
# ------------------------------------------------------------------
for (g in 1:2) {
  fases <- if (g == 1) 1:3 else 4:5
  dh <- d[d$teip == 0 | d$fase %in% fases, ]
  for (y in Y) {
    r <- att_ponderado(dh, y, ra = TRUE)
    regista(paste0("het", g, "_ipwra_", y), r[1], r[2], nrow(dh))
  }
}

# ------------------------------------------------------------------
# 10. Robustez: score com termos quadráticos e interação
# ------------------------------------------------------------------
d$pct_ase_2 <- d$pct_ase^2
d$esp_2     <- d$conclusao_esp^2
d$ase_esp   <- d$pct_ase * d$conclusao_esp
for (y in Y) {
  r <- att_ponderado(d, y, vars_ps = c(X, "pct_ase_2", "esp_2", "ase_esp"), ra = TRUE)
  regista(paste0("flex_ipwra_", y), r[1], r[2], nrow(d))
}

cat("\nResumo das estimativas\n")
print(transform(resultados, coef = round(coef, 3), ep = round(ep, 3), z = round(z, 3)),
      row.names = FALSE)
