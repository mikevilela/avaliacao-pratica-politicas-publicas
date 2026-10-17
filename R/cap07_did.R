# ==================================================================
#  Capítulo 7 — Diferenças-em-diferenças
#  Caso: agregação de freguesias (2013) e participação eleitoral
#  Dados: Rodrigues & Tavares (2020), Cities 101, 102685
#
#  Versão em R da análise do capítulo (equivalente ao ficheiro Stata).
#  Reproduz: médias por ano e por grupo (Tabela descritiva), a dupla
#  diferença 2 x 2, os Modelos 1 a 3 do artigo, o Modelo 2 com
#  erros-padrão agrupados por freguesia, a verificação só com o
#  continente, o modelo de efeitos fixos bidirecionais (TWFE), o estudo
#  de acontecimento e as duas figuras do capítulo.
#
#  Pacotes: haven (ler .dta), fixest (regressões e erros-padrão
#  agrupados), ggplot2 (figuras).
#  Corra o script a partir da pasta onde se encontra; os dados estão em
#  ../dados e as figuras são gravadas na subpasta «saidas».
# ==================================================================

library(haven)
library(fixest)
library(ggplot2)

dir_dados  <- "../dados"
dir_saidas <- "saidas"
dir.create(dir_saidas, showWarnings = FALSE)

# ------------------------------------------------------------------
# 0. Dados: nomes das variáveis em português
# ------------------------------------------------------------------
d <- read_dta(file.path(dir_dados, "freguesias_participacao.dta"))
d <- zap_labels(d)
d <- data.frame(
  freguesia    = as.character(d$SMU),
  eleitores    = d$registered_voters,
  participacao = d$turnout,          # votantes em % dos inscritos
  agregada     = d$treated,          # 1 = freguesia agregada em 2013
  ano          = d$Year,
  urbana       = d$APU               # 1 = área predominantemente urbana
)
d_todos <- d                         # inclui as unidades sem resultado num dado ano
d <- d[!is.na(d$participacao), ]     # análise: só observações com resultado

# ------------------------------------------------------------------
# 1. Exploração: médias por ano e por grupo
# ------------------------------------------------------------------
desc <- do.call(rbind, lapply(split(d, d$ano), function(x) data.frame(
  ano           = x$ano[1],
  nao_agregadas = mean(x$participacao[x$agregada == 0]),
  agregadas     = mean(x$participacao[x$agregada == 1]),
  total         = mean(x$participacao),
  dp            = sd(x$participacao),
  n             = nrow(x))))
desc$diferenca <- desc$agregadas - desc$nao_agregadas
tot <- data.frame(ano = "Total",
                  nao_agregadas = mean(d$participacao[d$agregada == 0]),
                  agregadas     = mean(d$participacao[d$agregada == 1]),
                  total = mean(d$participacao), dp = sd(d$participacao),
                  n = nrow(d))
tot$diferenca <- tot$agregadas - tot$nao_agregadas
desc <- rbind(desc, tot)
cat("\n== Participação média por ano e por grupo (%) ==\n")
print(format(desc, digits = 4, nsmall = 2), row.names = FALSE)

# ------------------------------------------------------------------
# 2. Gráfico das tendências (pressuposto das tendências paralelas)
# ------------------------------------------------------------------
medias <- aggregate(participacao ~ ano + agregada, data = d, FUN = mean)
medias$grupo <- factor(medias$agregada, levels = c(1, 0),
                       labels = c("Freguesias agregadas", "Freguesias não agregadas"))
g1 <- ggplot(medias, aes(ano, participacao, colour = grupo, linetype = grupo,
                         shape = grupo)) +
  geom_vline(xintercept = 2011, colour = "grey60", linetype = "dotted") +
  geom_line() + geom_point(size = 2.5) +
  scale_colour_manual(values = c("#1B365D", "#C0792F")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_shape_manual(values = c(16, 15)) +
  scale_x_continuous(breaks = seq(2001, 2017, 4)) +
  scale_y_continuous(limits = c(58, 70), breaks = seq(58, 70, 2)) +
  labs(x = "Ano da eleição", y = "Participação eleitoral (%)",
       colour = NULL, linetype = NULL, shape = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(dir_saidas, "c08_tendencias.png"), g1, width = 7, height = 4.5, dpi = 300)

# ------------------------------------------------------------------
# 3. Dupla diferença «à mão» (2009 vs 2013)
# ------------------------------------------------------------------
m <- function(g, a) mean(d$participacao[d$agregada == g & d$ano == a])
cat("\n== Tabela 2 x 2 (2009-2013) ==\n")
cat(sprintf("Agregadas:     %6.2f -> %6.2f  dif = %6.2f\n",
            m(1, 2009), m(1, 2013), m(1, 2013) - m(1, 2009)))
cat(sprintf("Não agregadas: %6.2f -> %6.2f  dif = %6.2f\n",
            m(0, 2009), m(0, 2013), m(0, 2013) - m(0, 2009)))
cat(sprintf("DiD = %6.3f\n",
            (m(1, 2013) - m(1, 2009)) - (m(0, 2013) - m(0, 2009))))

# ------------------------------------------------------------------
# 4. Regressões DiD por par de eleições (modelos do artigo)
#    pos = 1 no segundo ano de cada par; erros-padrão convencionais
# ------------------------------------------------------------------
par_anos <- function(a0, a1, dados = d) {
  x <- dados[dados$ano %in% c(a0, a1), ]
  x$pos <- as.integer(x$ano == a1)
  x
}
did <- participacao ~ agregada * pos + urbana

m1 <- feols(did, data = par_anos(2001, 2009), vcov = "iid")  # placebo
m2 <- feols(did, data = par_anos(2009, 2013), vcov = "iid")  # efeito da reforma
m3 <- feols(did, data = par_anos(2013, 2017), vcov = "iid")  # persistência

# ------------------------------------------------------------------
# 5. Modelo 2 com erros-padrão agrupados por freguesia
# ------------------------------------------------------------------
m2c <- feols(did, data = par_anos(2009, 2013), cluster = ~freguesia)

estrelas <- c("***" = 0.01, "**" = 0.05, "*" = 0.10)
cat("\n== Modelos 1 a 3 e Modelo 2 com erros-padrão agrupados ==\n")
print(etable(m1, m2, m3, m2c, digits = "r3", digits.stats = "r3",
             fitstat = ~ n + r2, signif.code = estrelas))
cat("\nIC a 95% da interação (Modelo 2, agrupados):\n")
print(round(confint(m2c)["agregada:pos", ], 3))

# ------------------------------------------------------------------
# 5b. Verificação: só freguesias do continente (distritos 01 a 18)
# ------------------------------------------------------------------
d$distrito       <- as.integer(substr(d$freguesia, 1, 2))
d_todos$distrito <- as.integer(substr(d_todos$freguesia, 1, 2))
cat("\n== Unidades por distrito e grupo em 2013 (todas as linhas do ficheiro) ==\n")
tab13 <- with(d_todos[d_todos$ano == 2013, ], table(distrito, agregada))
print(tab13)
cat("Unidades com código de distrito > 18 (todas agregadas):",
    sum(tab13[as.integer(rownames(tab13)) > 18, ]), "\n")

cont <- d[d$distrito <= 18, ]
m2_cont <- feols(did, data = par_anos(2009, 2013, cont), vcov = "iid")
m3_cont <- feols(did, data = par_anos(2013, 2017, cont), vcov = "iid")
cat("\n== Replicação vs. continente ==\n")
print(etable(m2, m2_cont, m3, m3_cont, digits = "r3", digits.stats = "r3",
             fitstat = ~ n + r2, signif.code = estrelas))

# ------------------------------------------------------------------
# 6. Painel completo: efeitos fixos de freguesia e de ano (TWFE)
#    erros-padrão agrupados por freguesia
# ------------------------------------------------------------------
d$tratamento <- d$agregada * (d$ano >= 2013)
fe <- feols(participacao ~ tratamento + i(ano, ref = 2001) | freguesia,
            data = d, cluster = ~freguesia)

# ------------------------------------------------------------------
# 7. Estudo de acontecimento, referência = 2009
# ------------------------------------------------------------------
for (a in c(2001, 2005, 2013, 2017)) d[[paste0("ag_", a)]] <- d$agregada * (d$ano == a)
es <- feols(participacao ~ ag_2001 + ag_2005 + ag_2013 + ag_2017 +
              i(ano, ref = 2001) | freguesia,
            data = d, cluster = ~freguesia)

cat("\n== Efeitos fixos (TWFE) e estudo de acontecimento ==\n")
print(etable(fe, es, digits = "r3", digits.stats = "r3",
             fitstat = ~ n + wr2, signif.code = estrelas))

# Figura do estudo de acontecimento
ce <- data.frame(ano = c(2001, 2005, 2009, 2013, 2017),
                 b   = c(coef(es)[c("ag_2001", "ag_2005")], 0,
                         coef(es)[c("ag_2013", "ag_2017")]),
                 ep  = c(se(es)[c("ag_2001", "ag_2005")], 0,
                         se(es)[c("ag_2013", "ag_2017")]))
gl <- fitstat(es, "G")[[1]] - 1                      # graus de liberdade (grupos - 1)
ce$li <- ce$b - qt(0.975, gl) * ce$ep
ce$ls <- ce$b + qt(0.975, gl) * ce$ep
g2 <- ggplot(ce, aes(factor(ano), b)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 3.5, colour = "grey60", linetype = "dashed") +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0.1, colour = "#1B365D") +
  geom_point(size = 2.5, colour = "#1B365D") +
  labs(x = "Ano da eleição", y = "Diferença face a 2009 (p.p.)") +
  theme_minimal()
ggsave(file.path(dir_saidas, "c08_eventstudy.png"), g2, width = 7, height = 4.5, dpi = 300)
