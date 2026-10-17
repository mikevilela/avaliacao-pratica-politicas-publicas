# ==================================================================
#  Capítulo 10 — Regressão descontínua (RDD)
#  Caso: escalões do abono de família, Ação Social Escolar (escalão A
#        vs. escalão B) e retenção escolar — DADOS SIMULADOS
#
#  ATENÇÃO: os dados são SIMULADOS (stata/cap10_simulacao.do; versão R
#  equivalente em R/cap10_simulacao.R). O autor não tem acesso aos
#  microdados administrativos. Os parâmetros foram escolhidos para fins
#  pedagógicos e não são estimativas do efeito real.
#  Efeitos verdadeiros (escalão A face ao B): retenção -0,050; nota +0,20.
#
#  Versão em R da análise do capítulo (equivalente ao ficheiro Stata
#  stata/cap10_rdd.do), aplicada ao MESMO ficheiro dados/ias_ase_simulado.dta.
#  Reproduz: tabela descritiva e comparação ingénua, teste de densidade,
#  equilíbrio das covariáveis, primeira fase, forma reduzida e RDD difusa,
#  robustez (banda, núcleo, polinómio, covariáveis, donut, placebo), a
#  ilustração do limiar com o IAS de 2026 e as figuras do capítulo.
#
#  Variável de afetação nas estimações: folga = -(rend_ref - 3 657,50),
#  positiva no 1.º escalão do abono, para que o efeito estimado seja
#  «escalão A menos escalão B».
#
#  Pacotes: haven, rdrobust, rddensity, sandwich, lmtest, AER (2SLS),
#  ggplot2. Corra o script a partir da pasta onde se encontra; os dados
#  estão em ../dados e as figuras são gravadas na subpasta «saidas».
#  Nota: as versões do rdrobust para R e para Stata podem diferir
#  ligeiramente na terceira ou quarta casa decimal.
# ==================================================================

library(haven)
library(rdrobust)
library(rddensity)
library(sandwich)
library(lmtest)
library(AER)
library(ggplot2)

dir_dados  <- "../dados"
dir_saidas <- "saidas"
dir.create(dir_saidas, showWarnings = FALSE)
L1 <- 3657.5

virgula <- function(dig) function(x) formatC(x, format = "f", digits = dig, decimal.mark = ",", big.mark = ".")
tema <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 12, hjust = 0.5))

# ------------------------------------------------------------------
# 0. Dados
# ------------------------------------------------------------------
d <- zap_labels(read_dta(file.path(dir_dados, "ias_ase_simulado.dta")))
d <- as.data.frame(d)
d$folga    <- -d$dist_limiar
d$elegivel <- as.integer(d$folga >= 0)
d$superior <- as.integer(d$escol_pais == 3)
d$basico   <- as.integer(d$escol_pais == 1)
cat("Observações:", nrow(d), "\n")
print(table(d$escalao_abono, d$escalao_ase))
cat("Proporção com escalão A no 2.º escalão do abono:",
    round(mean(d$ase_a[d$escalao_abono == 2]), 3), "\n")

# ------------------------------------------------------------------
# Funções auxiliares
# ------------------------------------------------------------------
rdlinha <- function(y, x, rotulo, c = 0, p = 1, h = NULL, b = NULL,
                    kernel = "triangular", fuzzy = NULL, covs = NULL) {
  e <- rdrobust(y, x, c = c, p = p, h = h, b = b, kernel = kernel, fuzzy = fuzzy, covs = covs)
  r <- data.frame(rotulo = rotulo, coef = e$coef[1], ep = e$se[1],
                  ic_inf = e$ci[3, 1], ic_sup = e$ci[3, 2], p_rob = e$pv[3],
                  h = e$bws[1, 1], n_esq = e$N_h[1], n_dir = e$N_h[2])
  if (!is.null(e$tau_cl)) { r$y_esq <- e$tau_cl[1]; r$y_dir <- e$tau_cl[2] }
  r
}

# janela fixa de h euros, núcleo retangular, erros-padrão robustos (HC1);
# iv = TRUE estima a RDD difusa por 2SLS (ase_a instrumentado por elegivel)
janela <- function(yvar, rotulo, h, iv = FALSE) {
  s <- d[abs(d$folga) <= h, ]
  s$y <- s[[yvar]]
  s$Ex <- s$elegivel * s$folga
  if (!iv) {
    m <- lm(y ~ elegivel + folga + Ex, data = s); nm <- "elegivel"
  } else {
    m <- ivreg(y ~ ase_a + folga + Ex | elegivel + folga + Ex, data = s); nm <- "ase_a"
  }
  t <- coeftest(m, vcov. = vcovHC(m, type = "HC1"))
  b <- t[nm, 1]; se <- t[nm, 2]
  data.frame(rotulo = rotulo, coef = b, ep = se, ic_inf = b - qnorm(.975) * se,
             ic_sup = b + qnorm(.975) * se, p = 2 * pnorm(-abs(b / se)), n = nrow(s))
}

# ------------------------------------------------------------------
# 1. Descrição e comparação ingénua
# ------------------------------------------------------------------
vars <- c("ase_a", "retencao", "nota", "rapariga", "idade", "superior", "basico",
          "dim_agregado", "desemprego")
grupos <- list(abono1 = d$escalao_abono == 1, abono2 = d$escalao_abono == 2,
               abaixo250 = d$folga >= 0 & d$folga <= 250, acima250 = d$folga < 0 & d$folga >= -250)
desc <- sapply(grupos, function(g) c(N = sum(g), colMeans(d[g, vars])))
print(round(desc, 3))

d12 <- d[d$escalao_abono <= 2, ]
ingenua <- function(f) { m <- lm(f, data = d12); coeftest(m, vcov. = vcovHC(m, type = "HC1"))["ase_a", 1:2] }
print(rbind(
  retencao     = ingenua(retencao ~ ase_a),
  retencao_ctr = ingenua(retencao ~ ase_a + rapariga + factor(ano_esc) + factor(regiao) +
                           factor(escol_pais) + dim_agregado),
  nota         = ingenua(nota ~ ase_a),
  nota_ctr     = ingenua(nota ~ ase_a + rapariga + factor(ano_esc) + factor(regiao) +
                           factor(escol_pais) + dim_agregado)), digits = 3)

# ------------------------------------------------------------------
# 2. Densidade
# ------------------------------------------------------------------
dens <- rddensity(X = d$folga, c = 0)
cat("Densidade: T =", round(dens$test$t_jk, 3), " p =", round(dens$test$p_jk, 3), "\n")
print(binom.test(sum(d$elegivel[abs(d$folga) <= 50]), sum(abs(d$folga) <= 50), 0.5))

h1 <- ggplot(d, aes(rend_ref / 1000)) +
  geom_histogram(binwidth = 0.1, boundary = 0, fill = "grey70", colour = "grey45", linewidth = 0.1) +
  geom_vline(xintercept = c(3.6575, 7.315), linetype = "dashed") +
  scale_x_continuous(labels = virgula(0), breaks = seq(0, 12, 2)) +
  scale_y_continuous(labels = virgula(0)) +
  labs(x = "Rendimento de referência (milhares de euros)", y = "Número de alunos",
       title = "(a) Distribuição completa") + tema
h2 <- ggplot(d[abs(d$folga) <= 1000, ], aes(rend_ref / 1000)) +
  geom_histogram(binwidth = 0.05, boundary = 3.6575, fill = "grey70", colour = "grey45", linewidth = 0.1) +
  geom_vline(xintercept = 3.6575, linetype = "dashed") +
  scale_x_continuous(labels = virgula(1)) + scale_y_continuous(labels = virgula(0)) +
  labs(x = "Rendimento de referência (milhares de euros)", y = "Número de alunos",
       title = "(b) Mil euros de cada lado do limiar") + tema
ggsave(file.path(dir_saidas, "c11_densidade_a.png"), h1, width = 6, height = 4.2, dpi = 300)
ggsave(file.path(dir_saidas, "c11_densidade_b.png"), h2, width = 6, height = 4.2, dpi = 300)

# ------------------------------------------------------------------
# 3. Equilíbrio das covariáveis
# ------------------------------------------------------------------
equil <- do.call(rbind, lapply(c("rapariga", "idade", "superior", "basico", "dim_agregado", "desemprego"),
  function(v) cbind(var = v,
                    rdlinha(d[[v]], d$folga, paste0("bal_", v))[, c("coef", "ep", "ic_inf", "ic_sup", "p_rob", "h")],
                    janela1000 = janela(v, "", 1000)$coef)))
print(equil, digits = 3)

# médias por classes de 100 euros contadas a partir do limiar
w <- d[abs(d$folga) <= 2000, ]
w$classe <- (L1 + (floor(w$dist_limiar / 100) + 0.5) * 100) / 1000
w$lado <- factor(w$folga >= 0)
w$rk <- w$rend_ref / 1000
grafico_painel <- function(vs, titulos, ficheiro, largura, altura, ncol, ic = FALSE) {
  pts <- do.call(rbind, lapply(vs, function(v) {
    m <- aggregate(w[[v]], list(classe = w$classe), mean)
    data.frame(var = unname(titulos[v]), classe = m$classe, y = m$x)
  }))
  lng <- do.call(rbind, lapply(vs, function(v)
    data.frame(var = unname(titulos[v]), rk = w$rk, lado = w$lado, y = w[[v]])))
  pts$var <- factor(pts$var, levels = titulos); lng$var <- factor(lng$var, levels = titulos)
  g <- ggplot(pts, aes(classe, y)) +
    geom_smooth(data = lng, aes(rk, y, group = lado), method = "lm", se = ic,
                colour = "black", fill = "grey80", linewidth = 0.8, formula = y ~ x) +
    geom_point(colour = "grey35", size = 1.3) +
    geom_vline(xintercept = L1 / 1000, linetype = "dashed") +
    facet_wrap(~var, scales = "free_y", ncol = ncol) +
    scale_x_continuous(labels = virgula(1)) + scale_y_continuous(labels = virgula(2)) +
    labs(x = "Rendimento de referência (milhares de euros)", y = NULL) + tema
  ggsave(file.path(dir_saidas, ficheiro), g, width = largura, height = altura, dpi = 300)
}
grafico_painel(c("superior", "idade", "dim_agregado", "desemprego"),
               c(superior = "(a) Pais com ensino superior", idade = "(b) Idade (anos)",
                 dim_agregado = "(c) Dimensão do agregado", desemprego = "(d) Progenitor desempregado"),
               "c11_equilibrio.png", 9, 7, 2)

# ------------------------------------------------------------------
# 4. Primeira fase, forma reduzida e RDD difusa
# ------------------------------------------------------------------
for (v in c("ase_a", "retencao", "nota")) {
  s <- d[abs(d$folga) <= 50, ]
  t <- t.test(s[[v]][s$elegivel == 1], s[[v]][s$elegivel == 0])
  cat("Janela de 50 euros,", v, ": abaixo", round(t$estimate[1], 4), " acima", round(t$estimate[2], 4),
      " diferença", round(diff(rev(t$estimate)), 4), "\n")
}
ref <- rdrobust(d$retencao, d$folga)
h_opt <- ref$bws[1, 1]; b_opt <- ref$bws[2, 1]
covs <- cbind(d$rapariga, d$idade, d$superior, d$basico, d$dim_agregado)
principal <- rbind(
  rdlinha(d$ase_a, d$folga, "fs_tri"),
  rdlinha(d$retencao, d$folga, "fr_ret_tri"),
  rdlinha(d$nota, d$folga, "fr_nota_tri"),
  rdlinha(d$retencao, d$folga, "fz_ret_tri", fuzzy = d$ase_a),
  rdlinha(d$nota, d$folga, "fz_nota_tri", fuzzy = d$ase_a),
  rdlinha(d$retencao, d$folga, "fz_ret_covs", fuzzy = d$ase_a, covs = covs),
  rdlinha(d$retencao, d$folga, "fz_ret_uni", fuzzy = d$ase_a, kernel = "uniform"),
  rdlinha(d$retencao, d$folga, "fz_ret_p2", fuzzy = d$ase_a, p = 2))
print(principal, digits = 3)
print(rbind(janela("ase_a", "fs_j1000", 1000),
            janela("retencao", "fr_ret_j1000", 1000),
            janela("retencao", "fr_ret_j500", 500),
            janela("nota", "fr_nota_j1000", 1000),
            janela("retencao", "fz_ret_j1000", 1000, iv = TRUE),
            janela("nota", "fz_nota_j1000", 1000, iv = TRUE)), digits = 3)

grafico_painel(c("ase_a", "retencao", "nota"),
               c(ase_a = "(a) Proporção com escalão A", retencao = "(b) Taxa de retenção",
                 nota = "(c) Pontuação na prova"),
               "c11_rdplot.png", 12, 4.2, 3, ic = TRUE)

# ------------------------------------------------------------------
# 5. Robustez (forma reduzida da retenção)
# ------------------------------------------------------------------
bandas <- do.call(rbind, lapply(c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2), function(m)
  rdlinha(d$retencao, d$folga, paste0("bw_", m), h = m * h_opt, b = b_opt)))
print(bandas, digits = 3)
print(rbind(rdlinha(d$retencao, d$folga, "fr_ret_uni", kernel = "uniform"),
            rdlinha(d$retencao, d$folga, "fr_ret_p2", p = 2),
            rdlinha(d$retencao, d$folga, "fr_ret_covs", covs = covs)), digits = 3)
donut <- do.call(rbind, lapply(c(0, 25, 50, 100, 200), function(k) {
  s <- if (k == 0) d else d[abs(d$folga) > k, ]
  cbind(k = k, rdlinha(s$retencao, s$folga, paste0("donut_", k)))
}))
print(donut, digits = 3)
placebo <- rbind(
  do.call(rbind, lapply(c(500, 1000, 1500), function(cc) {
    s <- d[d$folga >= 0, ]; cbind(c = cc, rdlinha(s$retencao, s$folga, paste0("plac_", cc), c = cc)) })),
  do.call(rbind, lapply(c(-500, -1000, -1500, -2000, -2500), function(cc) {
    s <- d[d$folga < 0 & d$folga > -3657.5, ]; cbind(c = cc, rdlinha(s$retencao, s$folga, paste0("plac_", cc), c = cc)) })))
print(placebo, digits = 3)

g1 <- ggplot(bandas, aes(h, coef)) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 30, colour = "grey45") + geom_point() +
  geom_vline(xintercept = h_opt, linetype = "dashed") +
  scale_x_continuous(labels = virgula(0)) + scale_y_continuous(labels = virgula(3)) +
  labs(x = "Largura de banda h (euros)", y = "Efeito estimado", title = "(a) Largura de banda") + tema
g2 <- ggplot(donut, aes(k, coef)) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 6, colour = "grey45") + geom_point() +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
  scale_y_continuous(labels = virgula(3)) +
  labs(x = "Exclusão em torno do limiar (euros)", y = "Efeito estimado", title = "(b) Donut") + tema
verd <- cbind(c = 0, rdlinha(d$retencao, d$folga, "verdadeiro"))
pl <- rbind(placebo[, names(verd)], verd)
pl$tipo <- ifelse(pl$c == 0, "verdadeiro", "placebo")
g3 <- ggplot(pl, aes((L1 - c) / 1000, coef, shape = tipo)) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.08, colour = "grey45") +
  geom_point(size = 2) + scale_shape_manual(values = c(placebo = 16, verdadeiro = 18), guide = "none") +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
  scale_x_continuous(labels = virgula(0)) + scale_y_continuous(labels = virgula(3)) +
  labs(x = "Limiar (milhares de euros)", y = "Efeito estimado", title = "(c) Limiares placebo") + tema
ggsave(file.path(dir_saidas, "c11_robustez_a.png"), g1, width = 4.5, height = 4.2, dpi = 300)
ggsave(file.path(dir_saidas, "c11_robustez_b.png"), g2, width = 4.5, height = 4.2, dpi = 300)
ggsave(file.path(dir_saidas, "c11_robustez_c.png"), g3, width = 4.5, height = 4.2, dpi = 300)

# ------------------------------------------------------------------
# 6. Ilustração: o limiar com o IAS de 2026 (537,13 euros)
# ------------------------------------------------------------------
L2026 <- 0.5 * 537.13 * 14
cat("Limiar com o IAS de 2026:", L2026, "; alunos entre os dois limiares:",
    sum(d$rend_ref > L1 & d$rend_ref <= L2026), "\n")
