* ==================================================================
*  Capítulo 6 — Causalidade e contrafactual
*  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
*  Cada bloco «*>> nome ... *<<» corresponde a uma caixa do livro.
*  Caso: agregação de freguesias (2013) e participação eleitoral.
*  Dados: Rodrigues & Tavares (2020), Cities 101, 102685.
*  Não usa pacotes adicionais.
* ==================================================================
version 18
clear all
set more off
set linesize 80

*>> c06-abrir
use "../dados/freguesias_participacao.dta", clear
describe
list in 1/5
*<<

*>> c06-variaveis
rename (SMU registered_voters turnout treated Year APU) ///
       (freguesia eleitores participacao agregada ano urbana)
summarize participacao agregada ano urbana
drop if missing(participacao)
tabulate ano agregada
*<<

*>> c06-medias
table ano agregada, statistic(mean participacao) nformat(%5.2f)
*<<

*>> c06-guardar
summarize participacao if agregada == 1 & ano == 2009
scalar a09 = r(mean)
quietly summarize participacao if agregada == 1 & ano == 2013
scalar a13 = r(mean)
quietly summarize participacao if agregada == 0 & ano == 2009
scalar n09 = r(mean)
quietly summarize participacao if agregada == 0 & ano == 2013
scalar n13 = r(mean)
scalar list
*<<

*>> c06-antes-depois
display a13 - a09
display n13 - n09
*<<

*>> c06-com-sem
display a13 - n13
display a09 - n09
*<<

*>> c06-did
display (a13 - a09) - (n13 - n09)
display (a13 - n13) - (a09 - n09)
display a09 + (n13 - n09)
*<<

*>> c06-regressao
generate pos = (ano == 2013)
regress participacao i.agregada##i.pos urbana if inlist(ano, 2009, 2013)
*<<
