* ==================================================================
*  Capítulo 9 — Matching (emparelhamento)
*  Caso: Territórios Educativos de Intervenção Prioritária (TEIP)
*  Unidades: agrupamentos de escolas e escolas não agrupadas públicas
*  Dados: DGEEC/Infoescolas (3.º ciclo, 2020/21 a 2022/23); listas TEIP
*         da DGE (2021 e 2024); INE (densidade populacional, 2011)
*  Preparação dos dados brutos: dados/preparar_teip.py
* ==================================================================
version 18
clear all
set more off
set seed 20240716
graph set window fontface "Palatino"
capture log close
log using "cap09_matching.log", text replace

* Programa auxiliar: escreve no log uma linha-resumo de cada estimativa
capture program drop res
program define res
  args rotulo
  matrix b_ = e(b)
  matrix V_ = e(V)
  display "RES|`rotulo'|" %9.3f b_[1,1] "|" %9.3f sqrt(V_[1,1]) "|" ///
          %9.3f b_[1,1]/sqrt(V_[1,1]) "|" e(N)
end

* 0. Importação, rótulos e ficheiro de trabalho ---------------------
import delimited "../dados/brutos/teip/teip_agrupamentos.csv", ///
       encoding(utf-8) varnames(1) clear
label variable codigo        "Código da unidade orgânica (DGAE ou DGEEC)"
label variable nome          "Nome da unidade orgânica (Infoescolas)"
label variable nao_agrupada  "Escola não agrupada (1 = sim)"
label variable alunos_2020   "Alunos do 3.º ciclo em 2019/20"
label variable alunos_2023   "Alunos do 3.º ciclo em 2022/23"
label variable retencao      "Taxa de retenção/desistência no 3.º ciclo, 2021/22-2022/23 (%)"
label variable pct_rapazes   "Rapazes no 3.º ciclo, 2022/23 (%)"
label variable n_coorte      "Alunos nas coortes de conclusão 2020/21-2022/23"
label variable conclusao     "Conclusão do 3.º ciclo em 3 anos, coortes 2020/21-2022/23 (%)"
label variable conclusao_esp "Conclusão esperada dado o perfil ASE e a escolaridade da mãe (%)"
label variable pct_ase       "Alunos com Ação Social Escolar nas coortes do 3.º ciclo (%)"
label variable n_pds         "Alunos nas coortes de percursos diretos 2021/22-2022/23"
label variable pds           "Percursos diretos de sucesso no 3.º ciclo, 2021/22-2022/23 (%)"
label variable pds_esp       "Percursos diretos de sucesso: média nacional comparável (%)"
label variable conclusao2    "Conclusão do 2.º ciclo em 2 anos, 2020/21-2022/23 (%)"
label variable pct_ase2      "Alunos com Ação Social Escolar no 2.º ciclo (%)"
label variable municipio     "Município"
label variable densidade     "Densidade populacional do município, 2011 (hab./km2)"
label variable nuts2         "Região NUTS II"
label variable fase          "Fase de entrada no TEIP (0 = nunca; 9 = fusão)"
label variable teip          "Unidade TEIP na lista de 2021 (1 = sim)"
label variable teip4         "Unidade na lista TEIP4 de 2024 (1 = sim)"
label variable grupo4        "Grupo TEIP4 (1 = em desenvolvimento; 2 = em transição)"
label variable fase4         "Fase indicada na lista TEIP4 (6 = nova entrada)"
label variable teip4_novo    "Entrou no TEIP só em 2024/25 (TEIP4, 1 = sim)"
label define simnao 0 "Não" 1 "Sim"
label values nao_agrupada teip teip4 teip4_novo simnao
label define fases 0 "Nunca TEIP" 1 "1.ª fase (2006/07)" 2 "2.ª fase (2009)" ///
       3 "3.ª fase (2009/10)" 4 "4.ª fase (2012/13)" 5 "5.ª fase (posterior)" 9 "Fusão"
label values fase fases

* Covariáveis derivadas
generate ln_alunos    = ln(alunos_2020)
generate ln_densidade = ln(densidade)
label variable ln_alunos    "Logaritmo dos alunos do 3.º ciclo (2019/20)"
label variable ln_densidade "Logaritmo da densidade populacional (2011)"
generate regiao = 1*(nuts2=="Norte") + 2*(nuts2=="Centro") + 3*(nuts2=="AM Lisboa") ///
         + 4*(nuts2=="Alentejo") + 5*(nuts2=="Algarve")
label define regioes 1 "Norte" 2 "Centro" 3 "AM Lisboa" 4 "Alentejo" 5 "Algarve"
label values regiao regioes
label variable regiao "Região NUTS II"
generate centro   = regiao==2
generate aml      = regiao==3
generate alentejo = regiao==4
generate algarve  = regiao==5
label variable centro   "Região Centro"
label variable aml      "Área Metropolitana de Lisboa"
label variable alentejo "Região Alentejo"
label variable algarve  "Região Algarve"

* Grupos de análise: tratados (TEIP desde 2012/13 ou antes, lista de 2021),
* controlos (nunca TEIP: nem na lista de 2021 nem na de 2024) e o grupo
* placebo (entrou só no TEIP4, em 2024/25: resultados anteriores à entrada)
generate grupo = .
replace  grupo = 1 if teip==1
replace  grupo = 0 if teip==0 & teip4==0
replace  grupo = 2 if teip4_novo==1
label define grupos 0 "Nunca TEIP" 1 "TEIP (lista 2021)" 2 "Novo TEIP4 (2024)"
label values grupo grupos
label variable grupo "Grupo de análise"
global X "pct_ase conclusao_esp ln_alunos ln_densidade centro aml alentejo algarve"
egen falta = rowmiss(conclusao pds retencao $X)
tabulate grupo falta, missing
* As escolas não agrupadas (quase todas secundárias com 3.º ciclo) ficam fora:
* têm uma estrutura diferente da dos agrupamentos e só 3 são TEIP.
tabulate grupo nao_agrupada if falta==0
generate amostra = inlist(grupo,0,1) & falta==0 & nao_agrupada==0
label variable amostra "Amostra principal (agrupamentos TEIP vs. nunca TEIP, sem valores em falta)"
generate amostra_pl = inlist(grupo,0,2) & falta==0 & nao_agrupada==0
label variable amostra_pl "Amostra placebo (novos TEIP4 vs. nunca TEIP)"
compress
save "../dados/teip_agrupamentos.dta", replace

* 1. Descrição: médias e diferenças padronizadas antes do matching
tabulate grupo if falta==0 & nao_agrupada==0
tabulate fase if amostra & teip==1
tabulate regiao teip if amostra
tabstat conclusao pds retencao $X if amostra, by(teip) statistics(mean sd) ///
        format(%8.2f) columns(statistics) longstub
foreach v of varlist conclusao pds retencao $X {
  quietly summarize `v' if amostra & teip==1
  local m1 = r(mean)
  local v1 = r(Var)
  quietly summarize `v' if amostra & teip==0
  local m0 = r(mean)
  local v0 = r(Var)
  display "DESC|" %-14s "`v'" "|" %7.2f `m1' "|" %7.2f `m0' "|" %7.2f `m1'-`m0' ///
          "|" %7.1f 100*(`m1'-`m0')/sqrt((`v1'+`v0')/2) "|" %5.2f `v1'/`v0'
}

* 2. Comparação ingénua e regressão com controlos -------------------
foreach y in conclusao pds retencao {
  regress `y' teip if amostra, vce(robust)
  res "ingenua_`y'"
  regress `y' teip $X if amostra, vce(robust)
  matrix b_ = e(b)
  matrix V_ = e(V)
  display "RES|mqo_`y'|" %9.3f b_[1,1] "|" %9.3f sqrt(V_[1,1]) "|" ///
          %9.3f b_[1,1]/sqrt(V_[1,1]) "|" e(N)
}

* 3. Propensity score: modelo logit --------------------------------
logit teip $X if amostra
estimates store ps_logit
predict ps if amostra, pr
label variable ps "Propensity score (probabilidade estimada de ser TEIP)"
lroc, nograph
summarize ps if teip==1 & amostra, detail
summarize ps if teip==0 & amostra, detail
* Suporte comum: critério mínimo-máximo
quietly summarize ps if teip==1 & amostra
scalar ps_min1 = r(min)
quietly summarize ps if teip==0 & amostra
scalar ps_max0 = r(max)
display "SUP|min tratados=" %6.4f ps_min1 "|max controlos=" %6.4f ps_max0
count if amostra & teip==1 & ps > ps_max0
count if amostra & teip==0 & ps < ps_min1
generate suporte = amostra & ps >= ps_min1 & ps <= ps_max0
label variable suporte "Dentro do suporte comum (critério mínimo-máximo)"
tabulate teip suporte if amostra
* Quantos controlos há em cada faixa do score?
generate faixa = floor(ps*10)/10 if amostra
tabulate faixa teip if amostra

* Figura: distribuição do score nos dois grupos (histograma espelhado)
preserve
keep if amostra
generate bin = floor(ps/0.025)*0.025 + 0.0125
collapse (count) n=codigo, by(bin teip)
replace n = -n if teip==0
twoway (bar n bin if teip==1, barwidth(0.022) color("27 54 93")) ///
       (bar n bin if teip==0, barwidth(0.022) color("192 121 47")), ///
       yline(0, lcolor(gs8)) xlabel(0(0.1)1, format(%3.1f)) ///
       ylabel(-100 "100" -75 "75" -50 "50" -25 "25" 0 "0" 25 "25", angle(0)) ///
       ytitle("Número de agrupamentos") xtitle("Propensity score") ///
       legend(order(1 "TEIP (acima do eixo)" 2 "Nunca TEIP (abaixo do eixo)") rows(1) position(6)) ///
       graphregion(color(white)) plotregion(color(white))
graph export "../figuras/c10_pscore.png", replace width(2400)
restore

* 4. Teste de equilíbrio por blocos (Becker e Ichino, comando pscore)
pscore teip $X if amostra, pscore(ps_bi) blockid(bloco) comsup logit

* 5. Estimação do ATT ----------------------------------------------
* 5.1 Vizinho mais próximo no propensity score (com reposição), EP de Abadie-Imbens
foreach y in conclusao pds retencao {
  teffects psmatch (`y') (teip $X, logit) if amostra, atet nneighbor(1)
  res "nn1_`y'"
}
* Equilíbrio depois do matching 1:1
teffects psmatch (conclusao) (teip $X, logit) if amostra, atet nneighbor(1)
tebalance summarize
tebalance summarize, baseline
* 5.2 Cinco vizinhos
foreach y in conclusao pds retencao {
  teffects psmatch (`y') (teip $X, logit) if amostra, atet nneighbor(5)
  res "nn5_`y'"
}
teffects psmatch (conclusao) (teip $X, logit) if amostra, atet nneighbor(5)
tebalance summarize
* 5.3 Vizinho mais próximo com caliper (0,25 desvios-padrão do score);
*     exclui iterativamente os tratados sem par dentro do caliper
quietly summarize ps if amostra
scalar calip = 0.25*r(sd)
display "CAL|caliper=" %6.4f calip
generate fora_cal = 0 if amostra
local k = 1
while `k' > 0 {
  capture drop os_
  capture teffects psmatch (conclusao) (teip $X, logit) if amostra & fora_cal==0, ///
          atet nneighbor(1) caliper(`=calip') osample(os_)
  quietly count if os_==1
  local k = r(N)
  quietly replace fora_cal = 1 if os_==1
}
label variable fora_cal "Tratado sem par dentro do caliper (excluído)"
tabulate teip fora_cal if amostra
foreach y in conclusao pds retencao {
  teffects psmatch (`y') (teip $X, logit) if amostra & fora_cal==0, ///
           atet nneighbor(1) caliper(`=calip')
  res "cal_`y'"
}
teffects psmatch (conclusao) (teip $X, logit) if amostra & fora_cal==0, ///
         atet nneighbor(1) caliper(`=calip')
tebalance summarize
* Caraterísticas dos tratados excluídos pelo caliper
tabstat ps pct_ase conclusao_esp ln_densidade if amostra & teip==1, by(fora_cal) format(%8.2f)
* 5.4 Matching nas covariáveis (Mahalanobis) com correção do enviesamento
foreach y in conclusao pds retencao {
  teffects nnmatch (`y' $X) (teip) if amostra, atet nneighbor(1) ///
           biasadj(pct_ase conclusao_esp ln_alunos ln_densidade)
  res "maha_`y'"
}
* 5.5 Ponderação pelo inverso da probabilidade (IPW) e IPW com regressão (IPWRA)
foreach y in conclusao pds retencao {
  teffects ipw (`y') (teip $X, logit) if amostra, atet
  res "ipw_`y'"
  teffects ipwra (`y' $X) (teip $X, logit) if amostra, atet
  res "ipwra_`y'"
}
teffects ipw (conclusao) (teip $X, logit) if amostra, atet
tebalance summarize
* IPWRA só no suporte comum (critério mínimo-máximo)
foreach y in conclusao pds retencao {
  teffects ipwra (`y' $X) (teip $X, logit) if suporte, atet
  res "ipwra_sup_`y'"
}
* 5.6 Kernel (psmatch2, Epanechnikov), erros-padrão por bootstrap
foreach y in conclusao pds retencao {
  psmatch2 teip $X if amostra, outcome(`y') logit kernel kerneltype(epan) bwidth(0.06) common
  bootstrap r(att), reps(500) nodots: psmatch2 teip $X if amostra, ///
            outcome(`y') logit kernel kerneltype(epan) bwidth(0.06) common
  display "RES|kernel_`y'|" %9.3f _b[_bs_1] "|" %9.3f _se[_bs_1] "|" ///
          %9.3f _b[_bs_1]/_se[_bs_1] "|" e(N)
}
* Kernel ao estilo Becker-Ichino (attk), para comparação
attk conclusao teip $X if amostra, pscore(ps_bi) comsup boot reps(200) dots logit
* Diagnóstico de equilíbrio do kernel (pstest)
quietly psmatch2 teip $X if amostra, outcome(conclusao) logit kernel kerneltype(epan) bwidth(0.06) common
pstest $X, both
tabulate teip _support if amostra

* 6. Figura: diferenças padronizadas antes (amostra completa) e depois
*    do matching 1:1 com caliper (valores de tebalance summarize)
quietly teffects psmatch (conclusao) (teip $X, logit) if amostra, atet nneighbor(1)
quietly tebalance summarize
matrix T0 = r(table)
quietly teffects psmatch (conclusao) (teip $X, logit) if amostra & fora_cal==0, ///
        atet nneighbor(1) caliper(`=calip')
quietly tebalance summarize
matrix T1 = r(table)
matrix list T0
matrix list T1
preserve
clear
local k : word count $X
set obs `k'
generate antes  = .
generate depois = .
forvalues i = 1/`k' {
  replace antes  = 100*T0[`i',1] in `i'
  replace depois = 100*T1[`i',2] in `i'
}
generate ordem = _n
label define rot 1 "Alunos com ASE (%)" 2 "Conclusão esperada (%)" 3 "Dimensão (ln alunos)" ///
       4 "Densidade (ln)" 5 "Centro" 6 "AM Lisboa" 7 "Alentejo" 8 "Algarve"
label values ordem rot
list, clean
twoway (scatter ordem antes, msymbol(O) mcolor("192 121 47") msize(large)) ///
       (scatter ordem depois, msymbol(D) mcolor("27 54 93") msize(large)), ///
       xline(0, lcolor(gs8)) xline(-10 10, lcolor(gs12) lpattern(dash)) ///
       ylabel(1(1)`k', valuelabel angle(0) labsize(medsmall) nogrid) yscale(reverse) ///
       xlabel(-120(20)120) xtitle("Diferença padronizada (%)") ytitle("") ///
       legend(order(1 "Antes do matching" 2 "Depois (vizinho mais próximo com caliper)") ///
              rows(2) position(6)) ///
       graphregion(color(white)) plotregion(color(white))
graph export "../figuras/c10_equilibrio.png", replace width(2400)
restore

* Figura: densidade do score antes e depois do matching 1:1 com caliper
*    (psmatch2 com o score já estimado; controlos ponderados pelo n.º de vezes usados)
quietly psmatch2 teip if amostra, pscore(ps) outcome(conclusao) neighbor(1) caliper(`=calip')
tabulate teip _support if amostra
twoway (kdensity ps if amostra & teip==0, lcolor("192 121 47") lwidth(medthick)) ///
       (kdensity ps if amostra & teip==1, lcolor("27 54 93") lwidth(medthick) lpattern(dash)), ///
       title("Antes", size(medium)) xtitle("Propensity score") ytitle("Densidade") ///
       xlabel(0(0.2)1, format(%3.1f)) ylabel(0(1)6, angle(0)) ///
       legend(order(1 "Nunca TEIP" 2 "TEIP") cols(1) ring(0) position(1) region(lcolor(white))) ///
       graphregion(color(white)) plotregion(color(white)) name(g_antes, replace)
twoway (kdensity ps if amostra & teip==0 & _weight<. [aweight=_weight], lcolor("192 121 47") lwidth(medthick)) ///
       (kdensity ps if amostra & teip==1 & _support==1, lcolor("27 54 93") lwidth(medthick) lpattern(dash)), ///
       title("Depois", size(medium)) xtitle("Propensity score") ytitle("") ///
       xlabel(0(0.2)1, format(%3.1f)) ylabel(0(1)6, angle(0)) legend(off) ///
       graphregion(color(white)) plotregion(color(white)) name(g_depois, replace)
graph combine g_antes g_depois, rows(1) ycommon graphregion(color(white)) xsize(10) ysize(5)
graph export "../figuras/c10_densidade.png", replace width(2400)

* 7. Sensibilidade a fatores não observados: limites de Rosenbaum ---
*    matching 1:1 sem reposição, dentro do caliper; diferenças por par
foreach y in conclusao pds retencao {
  quietly psmatch2 teip $X if amostra, outcome(`y') logit neighbor(1) ///
          noreplacement caliper(`=calip') common
  display "RES|semrep_`y'|" %9.3f r(att) "|"
  * _n1 guarda o identificador (_id) do par: ordenar por _id antes de o usar
  sort _id
  capture drop dif_par
  generate dif_par = `y' - `y'[_n1] if _treated==1 & _support==1 & _n1<.
  summarize dif_par
  rbounds dif_par, gamma(1(0.25)3)
}

* 8. Teste placebo: unidades que só entraram no TEIP4 (2024/25) ---------
*    Os resultados de 2020/21-2022/23 são anteriores à entrada: o «efeito»
*    estimado deve ser nulo se as covariáveis captarem a seleção.
generate novo4 = (grupo==2) if amostra_pl
tabulate novo4
tabstat conclusao pds retencao $X if amostra_pl, by(novo4) statistics(mean) format(%8.2f)
foreach y in conclusao pds retencao {
  regress `y' novo4 if amostra_pl, vce(robust)
  res "pl_ingenua_`y'"
  teffects psmatch (`y') (novo4 $X, logit) if amostra_pl, atet nneighbor(1)
  res "pl_nn1_`y'"
  teffects ipwra (`y' $X) (novo4 $X, logit) if amostra_pl, atet
  res "pl_ipwra_`y'"
}

* 9. Heterogeneidade: TEIP2 (fases 1 a 3) e entradas de 2012 em diante (4 e 5)
generate tg = .
replace tg = 1 if amostra & inlist(fase,1,2,3)
replace tg = 2 if amostra & inlist(fase,4,5)
tabulate tg
foreach g in 1 2 {
  foreach y in conclusao pds retencao {
    teffects ipwra (`y' $X) (teip $X, logit) if amostra & (teip==0 | tg==`g'), atet
    res "het`g'_ipwra_`y'"
  }
}

* 10. Robustez: modelo de score mais flexível (termos quadráticos e interação)
generate pct_ase_2 = pct_ase^2
generate esp_2 = conclusao_esp^2
generate ase_esp = pct_ase*conclusao_esp
foreach y in conclusao pds retencao {
  teffects ipwra (`y' $X) (teip $X pct_ase_2 esp_2 ase_esp, logit) if amostra, atet
  res "flex_ipwra_`y'"
}

log close
