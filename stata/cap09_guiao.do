* ==================================================================
*  Capítulo 9 — Matching (emparelhamento)
*  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
*  Cada bloco «*>> nome ... *<<» corresponde a uma caixa do livro.
*  Caso: Programa TEIP e os resultados no 3.º ciclo (agrupamentos
*  de escolas). Dados: DGEEC/Infoescolas, DGE e INE.
*  Os comandos usados (logit, teffects, tebalance) fazem parte do
*  Stata; não é preciso instalar pacotes.
* ==================================================================
version 18
clear all
set more off
set linesize 80

*>> c09-abrir
use "../dados/teip_agrupamentos.dta", clear
global X "pct_ase conclusao_esp ln_alunos ln_densidade centro aml alentejo algarve"
describe teip pds conclusao retencao $X
keep if amostra == 1
tabulate teip
*<<

*>> c09-comparar
tabstat pds conclusao retencao $X, by(teip) nototal format(%8.2f)
*<<

*>> c09-logit
logit teip $X
lroc, nograph
*<<

*>> c09-suporte
predict ps, pr
tabstat ps, by(teip) statistics(n min p50 max) format(%6.3f)
summarize ps if teip == 0
count if teip == 1 & ps > r(max)
summarize ps if teip == 1
count if teip == 0 & ps < r(min)
*<<

*>> c09-ingenua
regress pds teip, vce(robust)
regress pds teip $X, vce(robust)
*<<

*>> c09-vizinho
teffects psmatch (pds) (teip $X, logit), atet nneighbor(1)
*<<

*>> c09-caliper
quietly summarize ps
scalar calip = 0.25*r(sd)
display calip
generate fora_cal = 0
local k = 1
while `k' > 0 {
    capture drop os_
    capture teffects psmatch (pds) (teip $X, logit) if fora_cal == 0, ///
        atet nneighbor(1) caliper(`=calip') osample(os_)
    quietly count if os_ == 1
    local k = r(N)
    quietly replace fora_cal = 1 if os_ == 1
}
tabulate teip fora_cal
teffects psmatch (pds) (teip $X, logit) if fora_cal == 0, ///
    atet nneighbor(1) caliper(`=calip')
*<<

*>> c09-ipw
teffects ipw (pds) (teip $X, logit), atet
teffects ipwra (pds $X) (teip $X, logit), atet
*<<

*>> c09-equilibrio-caliper
quietly teffects psmatch (pds) (teip $X, logit) if fora_cal == 0, ///
    atet nneighbor(1) caliper(`=calip')
tebalance summarize
*<<

*>> c09-equilibrio-nn1
quietly teffects psmatch (pds) (teip $X, logit), atet nneighbor(1)
tebalance summarize
*<<
