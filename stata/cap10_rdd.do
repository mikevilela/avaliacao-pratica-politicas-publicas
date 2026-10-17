* ==================================================================
*  Capítulo 10 — Regressão descontínua (RDD)
*  Caso: escalões do abono de família, Ação Social Escolar (escalão A
*        vs. escalão B) e retenção escolar — DADOS SIMULADOS
*
*  ATENÇÃO: os dados são SIMULADOS (stata/cap10_simulacao.do). O autor
*  não tem acesso aos microdados administrativos. Os parâmetros foram
*  escolhidos para fins pedagógicos e não são estimativas do efeito real.
*  Efeitos verdadeiros no PGD (escalão A face ao escalão B):
*     retenção -0,050; pontuação na prova +0,20.
*
*  Variáveis principais
*    rend_ref    : rendimento de referência anual (euros, rendimentos de 2025)
*    dist_limiar : rend_ref - 3 657,50 (0,5 x IAS x 14, IAS de 2025 = 522,50)
*    folga       : -dist_limiar (positiva = 1.º escalão = elegível para o
*                  escalão A); usada nas estimações para que o efeito
*                  estimado seja «escalão A menos escalão B»
*    ase_a       : escalão A da ASE (tratamento recebido)
*    retencao, nota : resultados
*
*  Reproduz todos os números, tabelas e figuras do capítulo:
*    1. descrição dos dados e comparação ingénua
*    2. densidade da variável de afetação (figura c11_densidade)
*    3. equilíbrio das covariáveis (tabela e figura c11_equilibrio)
*    4. primeira fase, forma reduzida e RDD difusa (tabela, figura c11_rdplot)
*    5. robustez: banda, núcleo, polinómio, donut, placebo (c11_robustez)
*    6. ilustração: deslocação do limiar com o IAS de 2026
*  A figura c11_montecarlo é produzida por cap10_simulacao.do.
*
*  Pacotes: rdrobust e rddensity (Calonico, Cattaneo, Titiunik et al.)
*     ssc install rdrobust
*     net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata)
*     net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata)
*  Corra este ficheiro a partir da pasta onde se encontra; os dados estão
*  em ../dados e as figuras são gravadas em ../figuras.
* ==================================================================
version 18
clear all
set more off
set linesize 120
capture log close
log using "cap10_rdd.log", text replace

* ------------------------------------------------------------------
* Programas auxiliares
* ------------------------------------------------------------------
* rdlinha: corre o rdrobust e escreve uma linha «RES|» com coeficiente
*   convencional, erro-padrão convencional, IC robusto a 95%, valor-p
*   robusto, larguras de banda (esq./dir.), N efetivo (esq./dir.) e
*   valores estimados no limiar (esq. = acima do limiar de rendimento;
*   dir. = abaixo do limiar). Opção fuzzy() para a RDD difusa.
capture program drop rdlinha
program define rdlinha
  syntax varlist(min=2 max=2) [if], ROTulo(string) ///
      [C(real 0) P(integer 1) H(string) B(string) KERnel(string) FUZzy(string) COVS(string)]
  tokenize `varlist'
  if "`kernel'" == "" local kernel "triangular"
  local opt ""
  if "`h'" != "" local opt "h(`h')"
  if "`b'" != "" local opt "`opt' b(`b')"
  if "`fuzzy'" != "" local opt "`opt' fuzzy(`fuzzy')"
  if "`covs'" != "" local opt "`opt' covs(`covs')"
  quietly rdrobust `1' `2' `if', c(`c') p(`p') kernel(`kernel') `opt'
  display "RES|`rotulo'|" %8.4f e(tau_cl) "|" %7.4f e(se_tau_cl) "|" ///
      %8.4f e(ci_l_rb) "|" %8.4f e(ci_r_rb) "|" %5.3f e(pv_rb) "|" ///
      %7.1f e(h_l) "|" %7.1f e(h_r) "|" e(N_h_l) "|" e(N_h_r) "|" ///
      %7.4f e(tau_cl_l) "|" %7.4f e(tau_cl_r)
end

* janela: regressão linear numa janela fixa de h euros de cada lado,
*   núcleo retangular, declives diferentes de cada lado, erros-padrão
*   robustos. Com iv, estima a RDD difusa por mínimos quadrados em dois
*   passos (ase_a instrumentado pela elegibilidade).
capture program drop janela
program define janela
  syntax varname, ROTulo(string) H(real) [IV]
  tempvar E Ex
  quietly generate byte `E' = (folga >= 0)
  quietly generate double `Ex' = `E'*folga
  if "`iv'" == "" {
    quietly regress `varlist' `E' folga `Ex' if abs(folga) <= `h', vce(robust)
    local b = _b[`E']
    local se = _se[`E']
  }
  else {
    quietly ivregress 2sls `varlist' folga `Ex' (ase_a = `E') if abs(folga) <= `h', vce(robust)
    local b = _b[ase_a]
    local se = _se[ase_a]
  }
  local lo = `b' - invnormal(0.975)*`se'
  local hi = `b' + invnormal(0.975)*`se'
  local p = 2*normal(-abs(`b'/`se'))
  display "OLS|`rotulo'|" %8.4f `b' "|" %7.4f `se' "|" %8.4f `lo' "|" %8.4f `hi' "|" %5.3f `p' "|" e(N)
end

* ------------------------------------------------------------------
* 1. Dados
* ------------------------------------------------------------------
use "../dados/ias_ase_simulado.dta", clear
describe, short
notes
generate double folga = -dist_limiar
generate byte elegivel = (folga >= 0)
generate byte superior = (escol_pais == 3)
generate byte basico = (escol_pais == 1)
tab escalao_abono escalao_ase
summarize rend_ref, detail
tab regiao
summarize retencao nota rapariga idade superior basico dim_agregado desemprego

* Proporção de alunos do 2.º escalão reposicionados no escalão A
quietly summarize ase_a if escalao_abono == 2
display "DESC|prop. escalão A no 2.º escalão do abono=" %5.3f r(mean)

* Tabela descritiva: escalões 1 e 2 completos e janela de 250 euros
foreach g in "1" "2" {
  quietly count if escalao_abono == `g'
  display "DESC|abono `g'|N=" r(N)
  foreach v in ase_a retencao nota rapariga idade superior basico dim_agregado desemprego {
    quietly summarize `v' if escalao_abono == `g'
    display "DESC|abono `g'|`v'|" %7.3f r(mean)
  }
}
foreach lado in "abaixo" "acima" {
  if "`lado'" == "abaixo" local cond "folga >= 0 & folga <= 250"
  if "`lado'" == "acima"  local cond "folga < 0 & folga >= -250"
  quietly count if `cond'
  display "DESC|250 `lado'|N=" r(N)
  foreach v in ase_a retencao nota rapariga idade superior basico dim_agregado desemprego {
    quietly summarize `v' if `cond'
    display "DESC|250 `lado'|`v'|" %7.3f r(mean)
  }
}

* Comparação ingénua (todos os alunos dos escalões 1 e 2 do abono)
regress retencao ase_a if escalao_abono <= 2, vce(robust)
display "NAIVE|retencao|" %8.4f _b[ase_a] "|" %7.4f _se[ase_a]
regress retencao ase_a rapariga i.ano_esc i.regiao i.escol_pais dim_agregado if escalao_abono <= 2, vce(robust)
display "NAIVE|retencao com controlos|" %8.4f _b[ase_a] "|" %7.4f _se[ase_a]
regress nota ase_a if escalao_abono <= 2, vce(robust)
display "NAIVE|nota|" %8.4f _b[ase_a] "|" %7.4f _se[ase_a]
regress nota ase_a rapariga i.ano_esc i.regiao i.escol_pais dim_agregado if escalao_abono <= 2, vce(robust)
display "NAIVE|nota com controlos|" %8.4f _b[ase_a] "|" %7.4f _se[ase_a]

* ------------------------------------------------------------------
* 2. Densidade da variável de afetação
* ------------------------------------------------------------------
rddensity folga, c(0)
display "DENS|T=" %6.3f e(T_q) "|p=" %5.3f e(pv_q) "|hl=" %6.1f e(h_l) "|hr=" %6.1f e(h_r) "|Nl=" e(N_h_l) "|Nr=" e(N_h_r)
* binomial: 50 euros de cada lado
bitest elegivel == 0.5 if abs(folga) <= 50
display "BINOM|50|n=" r(N) "|k=" r(k) "|p=" %5.3f r(p)

* Figura: histograma do rendimento de referência (classes de 100 euros)
preserve
set dp comma
generate double rk = rend_ref/1000
twoway (histogram rk if rend_ref <= 12500, start(0) width(0.1) frequency color(gs10) lcolor(gs7) lwidth(vthin)), ///
    xline(3.6575 7.315, lcolor(black) lpattern(dash)) ///
    xlabel(0(2)12, format(%3.0f)) ylabel(, angle(0) format(%9.0gc)) ///
    xtitle("Rendimento de referência (milhares de euros)") ytitle("Número de alunos") ///
    title("(a) Distribuição completa", size(medium) color(black)) ///
    graphregion(color(white)) plotregion(color(white)) name(h1, replace)
twoway (histogram rk if inrange(rend_ref, 2657.5, 4657.5), start(2.6575) width(0.05) frequency color(gs10) lcolor(gs7) lwidth(vthin)), ///
    xline(3.6575, lcolor(black) lpattern(dash)) ///
    xlabel(2.7(0.3)4.6, format(%3.1f)) ylabel(, angle(0) format(%9.0gc)) ///
    xtitle("Rendimento de referência (milhares de euros)") ytitle("Número de alunos") ///
    title("(b) Mil euros de cada lado do limiar", size(medium) color(black)) ///
    graphregion(color(white)) plotregion(color(white)) name(h2, replace)
graph combine h1 h2, rows(1) iscale(*1.35) graphregion(color(white)) xsize(10) ysize(4.2)
graph export "../figuras/c11_densidade.png", replace width(2600)
set dp period
restore

* ------------------------------------------------------------------
* 3. Equilíbrio das covariáveis no limiar
* ------------------------------------------------------------------
foreach v in rapariga idade superior basico dim_agregado desemprego {
  rdlinha `v' folga, rotulo(bal_`v')
  janela `v', rotulo(bal_`v'_j1000) h(1000)
}

* Figura: médias por classes de 100 euros contadas a partir do limiar
*   (2 000 euros de cada lado; nenhuma classe atravessa o limiar)
preserve
keep if abs(folga) <= 2000
generate double classe = (3657.5 + (floor(dist_limiar/100) + 0.5)*100)/1000
generate double rk = rend_ref/1000
generate byte acima = (folga < 0)
set dp comma
local i = 0
foreach v in superior idade dim_agregado desemprego {
  local ++i
  if "`v'" == "superior"     local tit "(a) Pais com ensino superior"
  if "`v'" == "idade"        local tit "(b) Idade (anos)"
  if "`v'" == "dim_agregado" local tit "(c) Dimensão do agregado"
  if "`v'" == "desemprego"   local tit "(d) Progenitor desempregado"
  local fy "%5.2f"
  bysort classe: egen double m_`v' = mean(`v')
  bysort classe: generate byte pr_`v' = (_n == 1)
  twoway (scatter m_`v' classe if pr_`v', msymbol(O) mcolor(gs6) msize(small)) ///
         (lfit `v' rk if acima == 0, lcolor(black) lwidth(medthick)) ///
         (lfit `v' rk if acima == 1, lcolor(black) lwidth(medthick)), ///
      xline(3.6575, lcolor(black) lpattern(dash)) legend(off) ///
      xlabel(1.7(0.5)5.7, format(%3.1f)) ylabel(, angle(0) format(`fy')) ///
      xtitle("Rendimento de referência (milhares de euros)") ytitle("") ///
      title("`tit'", size(medium) color(black)) ///
      graphregion(color(white)) plotregion(color(white)) name(e`i', replace)
}
graph combine e1 e2 e3 e4, rows(2) graphregion(color(white)) xsize(9) ysize(7)
graph export "../figuras/c11_equilibrio.png", replace width(2600)
set dp period
restore

* ------------------------------------------------------------------
* 4. Primeira fase, forma reduzida e RDD difusa
* ------------------------------------------------------------------
* 4.1 Médias simples numa janela estreita (50 euros de cada lado)
foreach v in ase_a retencao nota {
  ttest `v' if abs(folga) <= 50, by(elegivel)
  display "MEAN50|`v'|abaixo=" %6.4f r(mu_2) "|acima=" %6.4f r(mu_1) "|dif=" %7.4f r(mu_2)-r(mu_1) "|ep=" %6.4f r(se)
}

* 4.2 Primeira fase: probabilidade de escalão A
rdlinha ase_a folga, rotulo(fs_tri)
janela ase_a, rotulo(fs_j1000) h(1000)

* 4.3 Forma reduzida (efeito da elegibilidade) — especificação de referência
rdrobust retencao folga, c(0) p(1) kernel(triangular) bwselect(mserd)
local h_opt = e(h_l)
local b_opt = e(b_l)
display "HOPT|" %8.2f `h_opt' "|BOPT|" %8.2f `b_opt'
rdlinha retencao folga, rotulo(fr_ret_tri)
rdlinha nota folga, rotulo(fr_nota_tri)
janela retencao, rotulo(fr_ret_j1000) h(1000)
janela retencao, rotulo(fr_ret_j500) h(500)
janela nota, rotulo(fr_nota_j1000) h(1000)

* 4.4 RDD difusa (efeito do escalão A)
rdrobust retencao folga, c(0) p(1) kernel(triangular) fuzzy(ase_a)
rdlinha retencao folga, rotulo(fz_ret_tri) fuzzy(ase_a)
rdlinha nota folga, rotulo(fz_nota_tri) fuzzy(ase_a)
rdlinha retencao folga, rotulo(fz_ret_covs) fuzzy(ase_a) covs(rapariga idade superior basico dim_agregado)
janela retencao, rotulo(fz_ret_j1000) h(1000) iv
janela nota, rotulo(fz_nota_j1000) h(1000) iv

* 4.5 Variantes da RDD difusa para a retenção
rdlinha retencao folga, rotulo(fz_ret_uni) fuzzy(ase_a) kernel(uniform)
rdlinha retencao folga, rotulo(fz_ret_p2) fuzzy(ase_a) p(2)

* 4.6 Figura principal: (a) primeira fase, (b) retenção, (c) pontuação
preserve
keep if abs(folga) <= 2000
generate double classe = (3657.5 + (floor(dist_limiar/100) + 0.5)*100)/1000
generate double rk = rend_ref/1000
generate byte acima = (folga < 0)
set dp comma
local i = 0
foreach v in ase_a retencao nota {
  local ++i
  if "`v'" == "ase_a"    local tit "(a) Proporção com escalão A"
  if "`v'" == "retencao" local tit "(b) Taxa de retenção"
  if "`v'" == "nota"     local tit "(c) Pontuação na prova"
  bysort classe: egen double m_`v' = mean(`v')
  bysort classe: generate byte pr_`v' = (_n == 1)
  twoway (lfitci `v' rk if acima == 0, lcolor(black) lwidth(medthick) fcolor(gs13) alcolor(gs13)) ///
         (lfitci `v' rk if acima == 1, lcolor(black) lwidth(medthick) fcolor(gs13) alcolor(gs13)) ///
         (scatter m_`v' classe if pr_`v', msymbol(O) mcolor(gs6) msize(small)), ///
      xline(3.6575, lcolor(black) lpattern(dash)) legend(off) ///
      xlabel(1.7(1)5.7, format(%3.1f)) ylabel(, angle(0) format(%4.2f)) ///
      xtitle("Rendimento de referência (milhares de euros)") ytitle("") ///
      title("`tit'", size(medium) color(black)) ///
      graphregion(color(white)) plotregion(color(white)) name(r`i', replace)
}
graph combine r1 r2 r3, rows(1) iscale(*1.5) graphregion(color(white)) xsize(12) ysize(4.2)
graph export "../figuras/c11_rdplot.png", replace width(2800)
set dp period
restore

* ------------------------------------------------------------------
* 5. Robustez (forma reduzida da retenção, salvo indicação)
* ------------------------------------------------------------------
* 5.1 Largura de banda: 0,5 a 2 vezes a ótima (mesma largura piloto)
tempname pf
tempfile bandas
postfile `pf' double(h tau cil cir) using `bandas'
foreach m in 0.5 0.75 1 1.25 1.5 1.75 2 {
  local hh = `m'*`h_opt'
  quietly rdrobust retencao folga, c(0) p(1) kernel(triangular) h(`hh') b(`b_opt')
  display "BW|m=`m'|h=" %7.1f `hh' "|" %8.4f e(tau_cl) "|" %7.4f e(se_tau_cl) "|" ///
      %8.4f e(ci_l_rb) "|" %8.4f e(ci_r_rb) "|" %5.3f e(pv_rb) "|" e(N_h_l) "|" e(N_h_r)
  post `pf' (`hh') (e(tau_cl)) (e(ci_l_rb)) (e(ci_r_rb))
}
postclose `pf'

* 5.2 Núcleo, polinómio e covariáveis (forma reduzida)
rdlinha retencao folga, rotulo(fr_ret_uni) kernel(uniform)
rdlinha retencao folga, rotulo(fr_ret_p2) p(2)
rdlinha retencao folga, rotulo(fr_ret_covs) covs(rapariga idade superior basico dim_agregado)

* 5.3 Donut: exclusão dos alunos a k euros ou menos do limiar
tempname pd
tempfile donut
postfile `pd' k tau cil cir using `donut'
foreach k in 0 25 50 100 200 {
  if `k' == 0 local cond ""
  else local cond "if abs(folga) > `k'"
  quietly rdrobust retencao folga `cond', c(0) p(1) kernel(triangular)
  display "DONUT|k=`k'|" %8.4f e(tau_cl) "|" %7.4f e(se_tau_cl) "|" ///
      %8.4f e(ci_l_rb) "|" %8.4f e(ci_r_rb) "|" %5.3f e(pv_rb) "|" %7.1f e(h_l) "|" e(N_h_l) "|" e(N_h_r)
  post `pd' (`k') (e(tau_cl)) (e(ci_l_rb)) (e(ci_r_rb))
}
postclose `pd'

* 5.4 Limiares placebo (só alunos do mesmo lado do limiar verdadeiro;
*     acima, só até ao limiar seguinte, 7 315 euros, i.e. folga > -3 657,50)
tempname pp
tempfile placebo
postfile `pp' c tau cil cir using `placebo'
foreach c in 500 1000 1500 {
  quietly rdrobust retencao folga if folga >= 0, c(`c') p(1) kernel(triangular)
  display "PLAC|c=+`c'|" %8.4f e(tau_cl) "|" %7.4f e(se_tau_cl) "|" ///
      %8.4f e(ci_l_rb) "|" %8.4f e(ci_r_rb) "|" %5.3f e(pv_rb) "|" %7.1f e(h_l) "|" e(N_h_l) "|" e(N_h_r)
  post `pp' (`c') (e(tau_cl)) (e(ci_l_rb)) (e(ci_r_rb))
}
foreach c in -500 -1000 -1500 -2000 -2500 {
  quietly rdrobust retencao folga if folga < 0 & folga > -3657.5, c(`c') p(1) kernel(triangular)
  display "PLAC|c=`c'|" %8.4f e(tau_cl) "|" %7.4f e(se_tau_cl) "|" ///
      %8.4f e(ci_l_rb) "|" %8.4f e(ci_r_rb) "|" %5.3f e(pv_rb) "|" %7.1f e(h_l) "|" e(N_h_l) "|" e(N_h_r)
  post `pp' (`c') (e(tau_cl)) (e(ci_l_rb)) (e(ci_r_rb))
}
quietly rdrobust retencao folga, c(0) p(1) kernel(triangular)
post `pp' (0) (e(tau_cl)) (e(ci_l_rb)) (e(ci_r_rb))
postclose `pp'

* 5.5 Figura de robustez
set dp comma
use `bandas', clear
twoway (rcap cil cir h, lcolor(gs7)) (scatter tau h, msymbol(O) mcolor(black)), ///
    yline(0, lcolor(gs10) lpattern(dash)) xline(`h_opt', lcolor(black) lpattern(dash)) legend(off) ///
    xlabel(, format(%9.0gc)) ylabel(, angle(0) format(%5.3f)) ///
    xtitle("Largura de banda h (euros)") ytitle("Efeito estimado") ///
    title("(a) Largura de banda", size(medium) color(black)) ///
    graphregion(color(white)) plotregion(color(white)) name(b1, replace)
use `donut', clear
twoway (rcap cil cir k, lcolor(gs7)) (scatter tau k, msymbol(O) mcolor(black)), ///
    yline(0, lcolor(gs10) lpattern(dash)) legend(off) ///
    xlabel(0 25 50 100 200) ylabel(, angle(0) format(%5.3f)) ///
    xtitle("Exclusão em torno do limiar (euros)") ytitle("Efeito estimado") ///
    title("(b) Donut", size(medium) color(black)) ///
    graphregion(color(white)) plotregion(color(white)) name(b2, replace)
use `placebo', clear
generate double ck = (3657.5 - c)/1000
twoway (rcap cil cir ck if c != 0, lcolor(gs7)) (scatter tau ck if c != 0, msymbol(O) mcolor(black)) ///
       (rcap cil cir ck if c == 0, lcolor(black)) (scatter tau ck if c == 0, msymbol(D) mcolor(black)), ///
    yline(0, lcolor(gs10) lpattern(dash)) legend(off) ///
    xlabel(2(1)6, format(%3.0f)) ylabel(, angle(0) format(%5.3f)) ///
    xtitle("Limiar (milhares de euros)") ytitle("Efeito estimado") ///
    title("(c) Limiares placebo", size(medium) color(black)) ///
    graphregion(color(white)) plotregion(color(white)) name(b3, replace)
graph combine b1 b2 b3, rows(1) iscale(*1.5) graphregion(color(white)) xsize(12) ysize(4.2)
graph export "../figuras/c11_robustez.png", replace width(2800)
set dp period

* ------------------------------------------------------------------
* 6. Ilustração: o limiar com o IAS de 2026 (537,13 euros)
* ------------------------------------------------------------------
use "../dados/ias_ase_simulado.dta", clear
local L2026 = 0.5*537.13*14
display "IAS|limiar 2026=" %9.2f `L2026'
count if rend_ref > 3657.5 & rend_ref <= `L2026'
display "IAS|alunos entre os dois limiares=" r(N)
quietly count if escalao_abono == 2
display "IAS|alunos no 2.º escalão=" r(N)

log close
