# ==================================================================
#  Capítulo 10 — Regressão descontínua (RDD)
#  SIMULAÇÃO DOS DADOS do caso prático — versão em R
#
#  ATENÇÃO: os dados produzidos por este script são SIMULADOS. O autor
#  não tem acesso aos microdados administrativos da Segurança Social nem
#  do Ministério da Educação. Os parâmetros foram escolhidos para fins
#  pedagógicos e NÃO devem ser lidos como estimativas do efeito real da
#  Ação Social Escolar.
#
#  Este script reproduz o MESMO processo gerador dos dados (PGD) do
#  ficheiro stata/cap10_simulacao.do, mas com o gerador de números
#  aleatórios do R. Produz, por isso, dados estatisticamente equivalentes,
#  NÃO idênticos. As análises do capítulo (em Stata e em R) leem o
#  ficheiro dados/ias_ase_simulado.dta, gerado pela versão Stata; este
#  script grava a sua amostra em dados/ias_ase_simulado_R.dta, para não o
#  substituir, e pode correr opcionalmente um pequeno estudo de Monte Carlo.
#
#  Política que inspira a simulação
#    - Abono de família: escalões em múltiplos de IAS x 14 (Decreto-Lei
#      n.º 176/2003, art. 14.º, na redação do Decreto-Lei n.º 56/2022).
#    - Ação Social Escolar (Despacho n.º 8452-A/2015): escalão A = 1.º
#      escalão do abono; escalão B = 2.º escalão; art. 12.º, n.º 3:
#      reposicionamento no escalão A em caso de desemprego de um progenitor.
#    - IAS de 2025 = 522,50 euros; limiar = 0,5 x 522,50 x 14 = 3 657,50.
#
#  Parâmetros verdadeiros (iguais aos da versão Stata)
#    efeito do escalão A face ao B: retenção -0,050; pontuação +0,20;
#    efeito do escalão B face a nenhum apoio: retenção -0,020.
#
#  Pacotes: haven (gravar .dta); rdrobust (só para o Monte Carlo).
# ==================================================================

library(haven)

L1 <- 0.5 * 522.50 * 14   # 3 657,50 euros
L2 <- 1.0 * 522.50 * 14   # 7 315,00 euros
L3 <- 1.7 * 522.50 * 14   # 12 435,50 euros
invlogit <- function(z) 1 / (1 + exp(-z))

gera_amostra <- function(N = 40000, n0 = 70000) {
  u  <- rnorm(n0)
  ncriancas    <- 1 + (runif(n0) < .45) + (runif(n0) < .15) + (runif(n0) < .04)
  monoparental <- as.integer(runif(n0) < .22)
  r <- runif(n0)
  regiao <- 1 + (r > .35) + (r > .57) + (r > .85) + (r > .93)
  efreg  <- ifelse(regiao == 1, -.05, ifelse(regiao == 3, .08, ifelse(regiao == 4, -.03, 0)))
  rend_ref <- round(exp(log(4300) + .40 * u - .10 * (ncriancas - 1) - .12 * monoparental +
                          efreg + .22 * rnorm(n0)), 2)
  d <- data.frame(u, ncriancas, monoparental, regiao, rend_ref)
  d <- d[d$rend_ref <= L3, ][1:N, ]
  n <- nrow(d)
  d$dim_agregado <- d$ncriancas + 2 - d$monoparental
  d$rapariga   <- as.integer(runif(n) < .49)
  d$ano_esc    <- 5 + floor(5 * runif(n))
  d$atraso     <- as.integer(runif(n) < invlogit(-2.3 - .7 * d$u))
  d$idade      <- d$ano_esc + 5 + d$atraso
  e            <- .8 * d$u + rnorm(n)
  d$escol_pais <- 1 + (e > -.3) + (e > 1.0)
  d$desemprego <- as.integer(runif(n) < invlogit(-2 - .6 * d$u))
  d$escalao_abono <- 1 + (d$rend_ref > L1) + (d$rend_ref > L2)
  d$escalao_ase <- ifelse(d$escalao_abono == 1, 1,
                   ifelse(d$escalao_abono == 2 & d$desemprego == 1, 1,
                   ifelse(d$escalao_abono == 2, 2, 3)))
  d$ase_a <- as.integer(d$escalao_ase == 1)
  d$ase_b <- as.integer(d$escalao_ase == 2)
  s <- d$rend_ref / L1
  p <- .11 - .02 * (s - 1) + .004 * (s - 1)^2 - .03 * d$u - .02 * (d$escol_pais == 3) +
    .01 * (1 - d$rapariga) + .004 * (d$ano_esc - 7) + .03 * d$atraso -
    .07 * d$ase_a - .02 * d$ase_b
  p <- pmin(pmax(p, .005), .60)
  d$retencao <- as.integer(runif(n) < p)
  d$nota <- round(.35 * d$u + .15 * (d$escol_pais == 3) - .10 * (d$escol_pais == 1) +
                    .05 * d$rapariga + .08 * (s - 1) - .01 * (s - 1)^2 +
                    .25 * d$ase_a + .05 * d$ase_b + .9 * rnorm(n), 3)
  d$dist_limiar <- d$rend_ref - L1
  d$aluno_id <- seq_len(n)
  d[, c("aluno_id", "rapariga", "ano_esc", "idade", "regiao", "escol_pais", "dim_agregado",
        "ncriancas", "monoparental", "desemprego", "rend_ref", "dist_limiar",
        "escalao_abono", "escalao_ase", "ase_a", "ase_b", "retencao", "nota")]
}

# ------------------------------------------------------------------
# 1. Uma amostra (estatisticamente equivalente à da versão Stata)
# ------------------------------------------------------------------
set.seed(20251001)
d <- gera_amostra()
print(summary(d[, c("rend_ref", "retencao", "nota", "ase_a", "desemprego")]))
print(table(d$escalao_abono, d$escalao_ase))
write_dta(d, "../dados/ias_ase_simulado_R.dta")

# ------------------------------------------------------------------
# 2. Monte Carlo opcional (mude para TRUE; demora alguns minutos)
# ------------------------------------------------------------------
correr_mc <- FALSE
if (correr_mc) {
  library(rdrobust)
  set.seed(4242)
  res <- t(replicate(200, {
    s <- gera_amostra()
    folga <- -s$dist_limiar
    e <- rdrobust(s$retencao, folga, fuzzy = s$ase_a)
    c(fz = e$coef[1], fz_l = e$ci[3, 1], fz_r = e$ci[3, 2])
  }))
  cat("Média:", mean(res[, 1]), " DP:", sd(res[, 1]),
      " Cobertura do IC robusto:", mean(res[, 2] <= -0.05 & res[, 3] >= -0.05), "\n")
}
