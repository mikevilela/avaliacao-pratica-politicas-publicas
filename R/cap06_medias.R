# ==================================================================
#  Capítulo 6 — Causalidade e contrafactual
#  Caso: agregação de freguesias (2013) e participação eleitoral
#  Dados: Rodrigues & Tavares (2020), Cities 101, 102685
#
#  Reproduz a tabela 2 x 2 de médias (2009 e 2013, agregadas e não
#  agregadas), as três comparações discutidas no capítulo (antes-depois,
#  com-sem e diferença-em-diferenças) e o contrafactual implícito.
#  Pacote: haven (ler .dta). Corra o script a partir da pasta onde se
#  encontra; os dados estão em ../dados.
# ==================================================================

library(haven)

d <- zap_labels(read_dta("../dados/freguesias_participacao.dta"))
d <- data.frame(participacao = d$turnout,   # votantes em % dos inscritos
                agregada     = d$treated,   # 1 = freguesia agregada em 2013
                ano          = d$Year)
d <- d[!is.na(d$participacao), ]

# Médias por ano e por grupo (todas as eleições)
cat("== Participação média por ano e por grupo (%) ==\n")
print(round(tapply(d$participacao, list(ano = d$ano, agregada = d$agregada), mean), 2))

# Tabela 2 x 2: 2009 (antes) e 2013 (depois)
m <- function(g, a) mean(d$participacao[d$agregada == g & d$ano == a])
t22 <- rbind("Agregadas"     = c(m(1, 2009), m(1, 2013)),
             "Não agregadas" = c(m(0, 2009), m(0, 2013)))
colnames(t22) <- c("2009", "2013")
t22 <- cbind(t22, "Variação" = t22[, "2013"] - t22[, "2009"])
t22 <- rbind(t22, "Diferença" = t22["Agregadas", ] - t22["Não agregadas", ])
cat("\n== Tabela 2 x 2 (diferenças com valores não arredondados) ==\n")
print(round(t22, 2))

cat(sprintf("\nAntes-depois (agregadas):  %6.2f\n", m(1, 2013) - m(1, 2009)))
cat(sprintf("Com-sem em 2013:           %6.2f\n", m(1, 2013) - m(0, 2013)))
cat(sprintf("Diferença-em-diferenças:   %6.3f\n",
            (m(1, 2013) - m(1, 2009)) - (m(0, 2013) - m(0, 2009))))
cat(sprintf("Contrafactual 2013 (agregadas): %6.2f\n",
            m(1, 2009) + (m(0, 2013) - m(0, 2009))))

# Desvio-padrão da participação em 2013 e número de freguesias por grupo
# (úteis para o exercício sobre o efeito mínimo detetável)
x13 <- d[d$ano == 2013, ]
cat(sprintf("\nDesvio-padrão da participação em 2013: %5.2f (N = %d; agregadas = %d)\n",
            sd(x13$participacao), nrow(x13), sum(x13$agregada == 1)))
