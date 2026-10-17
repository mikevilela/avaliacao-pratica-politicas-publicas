* ==================================================================
*  Capítulo 7 — Diferenças-em-diferenças
*  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
*  Cada bloco «*>> nome ... *<<» corresponde a uma caixa do livro.
*  Caso: agregação de freguesias (2013) e participação eleitoral.
*  Dados: Rodrigues & Tavares (2020), Data in Brief.
*  Não precisa de pacotes adicionais.
* ==================================================================
version 18
clear all
set more off
set linesize 80

*>> c07-abrir
use "../dados/freguesias_participacao.dta", clear
rename (SMU registered_voters turnout treated Year APU) ///
       (freguesia eleitores participacao agregada ano urbana)
describe
*<<

*>> c07-medias
tabstat participacao, by(ano) statistics(mean sd n)
table ano agregada, statistic(mean participacao) nformat(%5.2f)
*<<

*>> c07-2x2
generate pos13 = (ano == 2013)
regress participacao i.agregada##i.pos13 if inlist(ano, 2009, 2013)
*<<

*>> c07-modelo1
generate pos09 = (ano == 2009)
regress participacao i.agregada##i.pos09 urbana if inlist(ano, 2001, 2009)
*<<

*>> c07-modelo2
regress participacao i.agregada##i.pos13 urbana if inlist(ano, 2009, 2013)
*<<

*>> c07-modelo3
generate pos17 = (ano == 2017)
regress participacao i.agregada##i.pos17 urbana if inlist(ano, 2013, 2017)
*<<

*>> c07-agrupados
regress participacao i.agregada##i.pos13 urbana ///
    if inlist(ano, 2009, 2013), vce(cluster freguesia)
*<<

*>> c07-continente
generate distrito = real(substr(freguesia, 1, 2))
tabulate ano agregada if distrito > 18
regress participacao i.agregada##i.pos13 urbana ///
    if inlist(ano, 2009, 2013) & distrito <= 18
regress participacao i.agregada##i.pos17 urbana ///
    if inlist(ano, 2013, 2017) & distrito <= 18
*<<

*>> c07-efeitos-fixos
encode freguesia, generate(id)
xtset id ano
generate tratamento = agregada * (ano >= 2013)
xtreg participacao tratamento i.ano, fe vce(cluster id)
*<<

*>> c07-acontecimento
xtreg participacao 1.agregada#i(2001 2005 2013 2017).ano i.ano, ///
    fe vce(cluster id)
*<<
