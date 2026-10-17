* ==================================================================
*  Capítulo 10 — Regressão descontínua (RDD)
*  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
*  Cada bloco «*>> nome ... *<<» corresponde a uma caixa do livro.
*  Dados SIMULADOS (ver cap10_simulacao.do). Efeito verdadeiro do
*  escalão A na retenção: -0,050.
*  Pacotes: ssc install rdrobust ; net install rddensity, from(...)
* ==================================================================
version 18
clear all
set more off
set linesize 80

*>> c10-abrir
use "../dados/ias_ase_simulado.dta", clear
describe
*<<

*>> c10-variaveis
generate double folga = -dist_limiar
generate byte elegivel = (folga >= 0)
summarize rend_ref folga elegivel ase_a retencao
*<<

*>> c10-escaloes
tabulate escalao_abono escalao_ase
*<<

*>> c10-ingenua
regress retencao ase_a if escalao_abono <= 2, vce(robust)
*<<

*>> c10-densidade
rddensity folga, c(0)
*<<

*>> c10-primeira-fase
rdrobust ase_a folga, c(0)
*<<

*>> c10-forma-reduzida
rdrobust retencao folga, c(0)
*<<

*>> c10-difusa
rdrobust retencao folga, c(0) fuzzy(ase_a)
*<<
