# Avaliação Prática de Políticas Públicas — dados e código

Dados e guiões de comandos (Stata e R) que acompanham o livro

> Rodrigues, Miguel Ângelo Vilela (2026). *Avaliação Prática de Políticas Públicas: fundamentos, modelos e métodos de avaliação de impacto*.

Nos capítulos 6 a 10, o livro convida o leitor a refazer cada caso prático no seu computador, passo a passo. As caixas «Acompanhar no Stata e no R» mostram os comandos e o resultado que cada programa imprime. Este repositório contém tudo o que é preciso para obter exatamente esses resultados.

## Como começar

1. Descarregue o repositório (botão **Code → Download ZIP** no GitHub, ou o arquivo da versão no Zenodo) e descompacte-o, mantendo a estrutura das pastas.
2. Instale os pacotes indicados abaixo (só uma vez).
3. Abra o guião do capítulo (`stata/capNN_guiao.do` ou `R/capNN_guiao.R`), defina como pasta de trabalho a pasta onde o guião está e execute os blocos pela ordem do capítulo.

A caixa «antes de começar» do Capítulo 6 explica estes passos em pormenor.

## Conteúdo

| Pasta | Conteúdo |
|---|---|
| `dados/` | Ficheiros de dados no formato do Stata (`.dta`), que o R lê com o pacote `haven`. |
| `dados/brutos/teip/` | Ficheiros de origem do caso TEIP (Capítulo 9) e `preparar_teip.py`, que os transforma. |
| `stata/` | `capNN_guiao.do`: os comandos das caixas do Capítulo NN, em blocos `*>> nome … *<<`. `capNN_<método>.do`: o script completo que reproduz todas as tabelas e figuras do capítulo. |
| `R/` | Os mesmos guiões e scripts completos para o R (`#>> nome … #<<`). |
| `resultados/` | O resultado esperado de cada bloco dos guiões, tal como impresso no livro, para comparação (`stata/` e `r/`). |
| `figuras/` | Pasta onde os scripts completos do Stata gravam as figuras. Os scripts R gravam-nas numa subpasta `saidas/` da pasta `R/`. |

| Capítulo | Método | Caso | Dados |
|---|---|---|---|
| 6 | Causalidade e contrafactual | Agregação de freguesias | `freguesias_participacao.dta` |
| 7 | Diferenças-em-diferenças | Agregação de freguesias e participação eleitoral | `freguesias_participacao.dta` |
| 8 | Séries temporais interrompidas | Lei da Paridade e mulheres eleitas nos executivos municipais | `paridade_serie.dta`, `paridade_municipios.dta` |
| 9 | Matching | Programa TEIP e resultados no 3.º ciclo | `teip_agrupamentos.dta` |
| 10 | Regressão descontínua (RDD) | Escalão A da Ação Social Escolar e retenção (**dados simulados**) | `ias_ase_simulado.dta`, `ias_ase_montecarlo.dta` |

## Pacotes

**Stata** (testado com StataNow 19.5; os guiões declaram `version 18`):

```stata
ssc install psmatch2
ssc install coefplot
ssc install rdrobust
net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata)
net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata)
```

**R** (testado com R 4.5.1):

```r
install.packages(c("haven", "sandwich", "lmtest", "fixest", "prais",
                   "Matching", "WeightIt", "marginaleffects", "numDeriv",
                   "AER", "rdrobust", "rddensity", "ggplot2"))
```

Versões usadas para produzir os resultados do livro: R: haven 2.5.5, sandwich 3.1.3, lmtest 0.9.40, fixest 0.14.2, prais 1.2.0, Matching 4.10.15, WeightIt 2.1.0, marginaleffects 1.0.0, numDeriv 2016.8.1.1, AER 1.2.17, rdrobust 4.1.1, rddensity 3.0, ggplot2 4.0.3; Stata: rdrobust 11.1.0, rddensity 2.3, lpdensity 2.3.1, psmatch2 4.0.12, coefplot 1.8.8. Versões diferentes dos pacotes podem alterar as últimas casas decimais.

## Fontes dos dados

- **Freguesias** (Capítulos 6 e 7): Rodrigues, M. e Tavares, A. (2020). Turnout and Territorial Reform: Data from Sub Municipal Governments in Portugal. *Data in Brief*, 30, 105598. https://doi.org/10.1016/j.dib.2020.105598
- **Lei da Paridade** (Capítulo 8): dados eleitorais autárquicos, compilados para Rodrigues (2023); ver o Capítulo 8 do livro.
- **TEIP** (Capítulo 9): DGEEC/Infoescolas (dados por unidade orgânica, 2.º e 3.º ciclos); DGE (listas das unidades orgânicas TEIP, 2021 e 2024; Despacho n.º 7798/2023); INE (densidade populacional por município, 2011). As fontes e as regras de construção estão descritas em `dados/preparar_teip.py`.
- **Ação Social Escolar** (Capítulo 10): dados **simulados**, gerados por `stata/cap10_simulacao.do` (versão R equivalente em `R/cap10_simulacao.R`). Não são dados reais e os efeitos não são estimativas do efeito real da Ação Social Escolar.

## Como citar

> Rodrigues, Miguel Ângelo Vilela (2026). *Avaliação Prática de Políticas Públicas — dados e código* (versão 1.0.0) [conjunto de dados e código]. Zenodo. https://doi.org/10.5281/zenodo.23443032

## Licenças

- Código (`stata/`, `R/`, `dados/preparar_teip.py`): licença MIT (ficheiro `LICENSE`).
- Dados construídos pelo autor (`dados/*.dta`, `resultados/`): Creative Commons Atribuição 4.0 Internacional (CC BY 4.0) (ficheiro `LICENSE-dados`).
- Ficheiros de origem de terceiros (`dados/brutos/`): mantêm as condições de utilização das respetivas fontes (DGEEC, DGE, INE), que devem ser citadas.
