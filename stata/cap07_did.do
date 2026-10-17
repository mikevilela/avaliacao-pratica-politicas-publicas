* ==================================================================
*  Capítulo 7 — Diferenças-em-diferenças
*  Caso: agregação de freguesias (2013) e participação eleitoral
*  Dados: Rodrigues & Tavares (2020), Cities 101, 102685
* ==================================================================
version 18
clear all
set more off
graph set window fontface "Palatino"
capture log close
log using "cap07_did.log", text replace

use "../dados/freguesias_participacao.dta", clear
rename (SMU registered_voters turnout treated Year APU) ///
       (freguesia eleitores participacao agregada ano urbana)
label variable participacao "Participação eleitoral (%)"
label variable agregada     "Freguesia agregada em 2013 (1 = sim)"
label variable urbana       "Área predominantemente urbana (1 = sim)"

* 1. Exploração: médias por grupo e por ano ------------------------
tabstat participacao, by(ano) statistics(mean sd n)
table ano agregada, statistic(mean participacao) nformat(%5.2f)

* 2. Gráfico das tendências (pressuposto das tendências paralelas) --
preserve
collapse (mean) participacao, by(ano agregada)
twoway (connected participacao ano if agregada==1, lcolor("27 54 93") mcolor("27 54 93") msymbol(O)) ///
       (connected participacao ano if agregada==0, lcolor("192 121 47") mcolor("192 121 47") msymbol(S) lpattern(dash)), ///
       xline(2011, lcolor(gs10) lpattern(shortdash)) xlabel(2001(4)2017) ///
       ylabel(58(2)70, angle(0) format(%2.0f)) ytitle("Participação eleitoral (%)") xtitle("Ano da eleição") ///
       legend(order(1 "Freguesias agregadas" 2 "Freguesias não agregadas") rows(1) position(6)) ///
       graphregion(color(white)) plotregion(color(white))
graph export "../figuras/c08_tendencias.png", replace width(2400)
restore

* 3. DiD 2x2 «à mão» (2009 vs 2013) --------------------------------
foreach g in 0 1 {
  foreach a in 2009 2013 {
    quietly summarize participacao if agregada==`g' & ano==`a'
    scalar m`g'_`a' = r(mean)
  }
}
display "Agregadas: "     %6.2f m1_2009 " -> " %6.2f m1_2013 "  dif = " %6.2f m1_2013-m1_2009
display "Não agregadas: " %6.2f m0_2009 " -> " %6.2f m0_2013 "  dif = " %6.2f m0_2013-m0_2009
display "DiD = " %6.3f (m1_2013-m1_2009)-(m0_2013-m0_2009)

* 4. Regressões DiD por par de eleições (modelos do artigo) ---------
generate pos = .
* Modelo 1 (placebo): 2001 vs 2009
replace pos = (ano==2009) if inlist(ano,2001,2009)
regress participacao i.agregada##i.pos urbana if inlist(ano,2001,2009)
estimates store m1
* Modelo 2: 2009 vs 2013 (efeito da reforma)
replace pos = (ano==2013) if inlist(ano,2009,2013)
regress participacao i.agregada##i.pos urbana if inlist(ano,2009,2013)
estimates store m2
* Modelo 3: 2013 vs 2017 (persistência)
replace pos = (ano==2017) if inlist(ano,2013,2017)
regress participacao i.agregada##i.pos urbana if inlist(ano,2013,2017)
estimates store m3
estimates table m1 m2 m3, b(%7.3f) se(%7.3f) stats(N r2)

* 5. Erros-padrão agrupados por freguesia (inferência robusta) -------
replace pos = (ano==2013) if inlist(ano,2009,2013)
regress participacao i.agregada##i.pos urbana if inlist(ano,2009,2013), vce(cluster freguesia)
estimates store m2c

* 5b. Verificação: só freguesias do Continente (códigos de distrito 01 a 18)
generate distrito = real(substr(freguesia,1,2))
tab distrito agregada if ano==2013
foreach p in "2009,2013" "2013,2017" {
  replace pos = (ano==real(substr("`p'",6,4))) if inlist(ano,`p')
  regress participacao i.agregada##i.pos urbana if inlist(ano,`p') & distrito<=18
}

* 6. Painel completo com efeitos fixos de freguesia e de ano ---------
encode freguesia, generate(id)
generate tratamento = agregada*(ano>=2013)
xtset id ano
xtreg participacao tratamento i.ano, fe vce(cluster id)
estimates store fe

* 7. Estudo de acontecimento (event study), referência = 2009 --------
foreach a in 2001 2005 2013 2017 {
  generate ag_`a' = agregada*(ano==`a')
}
xtreg participacao ag_2001 ag_2005 ag_2013 ag_2017 i.ano, fe vce(cluster id)
estimates store es
coefplot es, keep(ag_*) vertical yline(0, lcolor(gs10)) xline(2.5, lpattern(dash) lcolor(gs10)) ///
   coeflabels(ag_2001="2001" ag_2005="2005" ag_2013="2013" ag_2017="2017") ///
   ytitle("Diferença face a 2009 (p.p.)") xtitle("Ano da eleição") mcolor("27 54 93") ciopts(lcolor("27 54 93")) ///
   graphregion(color(white)) plotregion(color(white))
graph export "../figuras/c08_eventstudy.png", replace width(2400)

log close
