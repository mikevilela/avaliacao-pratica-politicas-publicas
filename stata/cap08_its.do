* ==================================================================
*  Capítulo 8 — Séries temporais interrompidas
*  Caso: Lei da Paridade (Lei Orgânica n.º 3/2006) e mulheres eleitas
*  nos executivos municipais, eleições autárquicas de 1979 a 2017
*  Dados: Rodrigues (Local Government Studies)
* ==================================================================
version 18
clear all
set more off
graph set window fontface "Palatino"
capture log close
log using "cap08_its.log", text replace

use "../dados/paridade_serie.dta", clear
rename (TotalMulheres ParLaw propostas Total ProMulheres) ///
       (mulheres paridade quotas_vol eleitos prop_mulheres)
label variable mulheres  "Mulheres eleitas vereadoras/presidentes"
label variable paridade  "Lei da Paridade em vigor (eleições de 2009 em diante)"
generate ln_mulheres = ln(mulheres)
list year time mulheres eleitos prop_mulheres paridade, noobs sep(0)

* Variável de tempo desde a intervenção (0 até 2005; 1, 2, 3 depois)
generate pos_tempo = paridade*(time-8)
tsset time

* Modelo 1: MQO com erros-padrão robustos
regress ln_mulheres time paridade pos_tempo, vce(robust)
estimates store its1
* Modelo 2: erros-padrão de Newey-West (1 desfasamento)
newey ln_mulheres time paridade pos_tempo, lag(1)
estimates store its2
* Modelo 3: Prais-Winsten com erro AR(1)
prais ln_mulheres time paridade pos_tempo
estimates store its3
estimates table its1 its2 its3, b(%7.3f) se(%7.3f) stats(N r2)

* Efeito imediato em 2009 (b2+b3) e declive pós-lei (b1+b3), com erro-padrão
quietly regress ln_mulheres time paridade pos_tempo, vce(robust)
lincom paridade + pos_tempo
lincom time + pos_tempo
estat dwatson

* Diagnóstico de autocorrelação dos resíduos do Modelo 1
quietly regress ln_mulheres time paridade pos_tempo
estat bgodfrey, lags(1)
estat durbinalt, lags(1)

* Valores previstos e contrafactual (tendência pré-2009 prolongada)
quietly regress ln_mulheres time paridade pos_tempo, vce(robust)
predict ln_prev, xb
generate prev = exp(ln_prev)
generate contraf = exp(_b[_cons] + _b[time]*time) if time>=8
* Efeito estimado na eleição de 2009 (em número de eleitas e em %)
display "Efeito de nível 2009 (log): " %6.3f _b[paridade]+_b[pos_tempo]*1
display "Equivale a uma variação de " %6.1f 100*(exp(_b[paridade]+_b[pos_tempo])-1) "% face ao contrafactual"
list year mulheres prev contraf if time>=8, noobs

twoway (scatter mulheres year, mcolor("27 54 93") msymbol(O)) ///
       (line prev year if time<=8, lcolor("27 54 93")) ///
       (line prev year if time>=9, lcolor("27 54 93")) ///
       (line contraf year, lcolor("192 121 47") lpattern(dash)), ///
       xline(2007, lcolor(gs10) lpattern(shortdash)) xlabel(1979 1985 1993 2001 2009 2017) ///
       ylabel(0(100)700, angle(0)) ytitle("Mulheres eleitas (n.º)") xtitle("Eleição autárquica") ///
       legend(order(1 "Observado" 2 "Previsto pelo modelo" 4 "Contrafactual (tendência pré-lei)") rows(1) position(6) size(small)) ///
       graphregion(color(white)) plotregion(color(white))
graph export "../figuras/c09_its.png", replace width(2400)

* Extensão: painel de 308 municípios (11 eleições), Poisson com efeitos fixos
use "../dados/paridade_municipios.dta", clear
rename (Women_in_office GenderQuotas) (mulheres paridade)
generate pos_tempo = paridade*(time-8)
xtset id time
xtpoisson mulheres time paridade pos_tempo, fe vce(robust) irr
estimates store itsp

log close
