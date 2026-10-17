* ==================================================================
*  Capítulo 10 — Regressão descontínua (RDD)
*  SIMULAÇÃO DOS DADOS do caso prático
*
*  ATENÇÃO: os dados produzidos por este ficheiro são SIMULADOS. O autor
*  não tem acesso aos microdados administrativos da Segurança Social nem
*  do Ministério da Educação. Os parâmetros foram escolhidos para fins
*  pedagógicos e NÃO devem ser lidos como estimativas do efeito real da
*  Ação Social Escolar.
*
*  Política que inspira a simulação
*    - Abono de família para crianças e jovens: escalões de rendimento de
*      referência definidos em múltiplos de IAS x 14 (Decreto-Lei
*      n.º 176/2003, de 2 de agosto, art. 14.º, na redação do Decreto-Lei
*      n.º 56/2022, de 19 de agosto). 1.º escalão: <= 0,5 x IAS x 14;
*      2.º escalão: > 0,5 e <= 1 x IAS x 14; 3.º escalão: > 1 e <= 1,7.
*    - Ação Social Escolar (Despacho n.º 8452-A/2015, de 31 de julho,
*      art. 11.º): escalão A = 1.º escalão do abono; escalão B = 2.º
*      escalão. Art. 12.º, n.º 3: alunos do escalão B com um progenitor
*      em desemprego involuntário há três ou mais meses são reposicionados
*      no escalão A (fonte do incumprimento parcial simulado).
*    - IAS de 2025 = 522,50 euros (Portaria n.º 6-B/2025/1, de 6 de
*      janeiro). Limiar entre o 1.º e o 2.º escalão (rendimentos de 2025):
*      0,5 x 522,50 x 14 = 3 657,50 euros. Limiar entre o 2.º e o 3.º:
*      7 315,00 euros. Limite superior do 3.º escalão: 12 435,50 euros.
*
*  Processo gerador dos dados (PGD) — parâmetros verdadeiros
*    N = 40 000 alunos do 5.º ao 9.º ano, continente, um ano letivo
*    hipotético, famílias nos escalões 1 a 3 do abono.
*    u ~ N(0,1): condição socioeconómica latente (não observada)
*    ln(rend. de referência) = ln(4300) + 0,40u - 0,10(n.º crianças - 1)
*        - 0,12(família monoparental) + efeito regional + 0,22 e,  e ~ N(0,1)
*    Tratamento: escalão A se rend_ref <= 3 657,50; acima do limiar,
*        escalão B, exceto se houver desemprego de um progenitor
*        (P = invlogit(-2 - 0,6u)), caso em que o aluno passa ao escalão A.
*    Retenção (probabilidade, modelo linear):
*        p = 0,11 - 0,02(s-1) + 0,004(s-1)^2 - 0,03u - 0,02(pais c/ ensino
*            superior) + 0,01(rapaz) + 0,004(ano - 7) + 0,03(atraso)
*            - 0,07 x escalão A - 0,02 x escalão B,   s = rend_ref/3657,50
*        (p truncada ao intervalo [0,005; 0,60])
*      => EFEITO VERDADEIRO do escalão A face ao B na retenção: -0,050
*         (-5 pontos percentuais), igual para todos os alunos.
*      => efeito do escalão B face a nenhum apoio (limiar de 7 315 euros):
*         -0,020 (usado apenas no exercício do capítulo).
*    Pontuação numa prova (escala aproximadamente padronizada):
*        nota = 0,35u + 0,15(superior) - 0,10(básico) + 0,05(rapariga)
*               + 0,08(s-1) - 0,01(s-1)^2 + 0,25 x A + 0,05 x B + 0,9 v
*      => EFEITO VERDADEIRO do escalão A face ao B na nota: +0,20.
*    Todas as covariáveis (sexo, ano, idade, região, escolaridade dos pais,
*    dimensão do agregado, desemprego) variam continuamente com o
*    rendimento: não há saltos no limiar, nem manipulação do rendimento.
*
*  Semente: 20251001. Transparência: a primeira semente experimentada
*  (20250901) produziu, por acaso, uma amostra invulgar (efeito estimado
*  na pontuação cerca de três desvios-padrão abaixo do verdadeiro); por
*  isso, para a amostra usada no capítulo, adotou-se a semente seguinte
*  experimentada. O estudo de Monte Carlo no fim deste ficheiro (200
*  amostras, semente própria) mostra a distribuição das estimativas e
*  permite avaliar quão típica é a amostra do capítulo.
*
*  Corra este ficheiro a partir da pasta onde se encontra. Grava
*  ../dados/ias_ase_simulado.dta (amostra do capítulo),
*  ../dados/ias_ase_montecarlo.dta (resultados do Monte Carlo) e a figura
*  ../figuras/c11_montecarlo.png. O Monte Carlo demora alguns minutos. A versão em R (R/cap10_simulacao.R)
*  reproduz o mesmo PGD, mas com outro gerador de números aleatórios:
*  produz dados estatisticamente equivalentes, não idênticos. As análises
*  do capítulo, em Stata e em R, leem o ficheiro .dta gerado aqui.
* ==================================================================
version 18
clear all
set more off
capture log close
log using "cap10_simulacao.log", text replace

* ------------------------------------------------------------------
* Programa que gera uma amostra segundo o PGD
* ------------------------------------------------------------------
capture program drop gera_amostra
program define gera_amostra

local L1  = 0.5*522.50*14      // 3 657,50 euros
local L2  = 1.0*522.50*14      // 7 315,00 euros
local L3  = 1.7*522.50*14      // 12 435,50 euros
local N   = 40000

clear
set obs 70000
generate double u = rnormal()
* agregado familiar
generate byte ncriancas = 1 + (runiform() < .45) + (runiform() < .15) + (runiform() < .04)
generate byte monoparental = (runiform() < .22)
generate byte dim_agregado = ncriancas + 2 - monoparental
* região (NUTS II do continente)
generate double r = runiform()
generate byte regiao = 1 + (r > .35) + (r > .57) + (r > .85) + (r > .93)
label define reg 1 "Norte" 2 "Centro" 3 "A. M. Lisboa" 4 "Alentejo" 5 "Algarve", replace
label values regiao reg
generate double efreg = cond(regiao==1, -.05, cond(regiao==3, .08, cond(regiao==4, -.03, 0)))
* rendimento de referência anual (euros)
generate double rend_ref = exp(ln(4300) + .40*u - .10*(ncriancas - 1) ///
    - .12*monoparental + efreg + .22*rnormal())
replace rend_ref = round(rend_ref, 0.01)
keep if rend_ref <= `L3'
keep in 1/`N'
* outras características
generate byte rapariga = (runiform() < .49)
generate byte ano_esc = 5 + floor(5*runiform())
generate byte atraso = (runiform() < invlogit(-2.3 - .7*u))
generate byte idade = ano_esc + 5 + atraso
generate double e = .8*u + rnormal()
generate byte escol_pais = 1 + (e > -.3) + (e > 1.0)
label define esc 1 "Até ao 3.º ciclo" 2 "Secundário" 3 "Superior", replace
label values escol_pais esc
generate byte desemprego = (runiform() < invlogit(-2 - .6*u))

* escalões do abono e da Ação Social Escolar
generate byte escalao_abono = 1 + (rend_ref > `L1') + (rend_ref > `L2')
generate byte escalao_ase = cond(escalao_abono == 1, 1, ///
    cond(escalao_abono == 2 & desemprego == 1, 1, cond(escalao_abono == 2, 2, 3)))
label define ase 1 "A" 2 "B" 3 "Sem escalão", replace
label values escalao_ase ase
generate byte ase_a = (escalao_ase == 1)
generate byte ase_b = (escalao_ase == 2)

* resultados
generate double s = rend_ref/`L1'
generate double p = .11 - .02*(s - 1) + .004*(s - 1)^2 - .03*u - .02*(escol_pais == 3) ///
    + .01*(1 - rapariga) + .004*(ano_esc - 7) + .03*atraso - .07*ase_a - .02*ase_b
replace p = min(max(p, .005), .60)
generate byte retencao = (runiform() < p)
generate double nota = .35*u + .15*(escol_pais == 3) - .10*(escol_pais == 1) + .05*rapariga ///
    + .08*(s - 1) - .01*(s - 1)^2 + .25*ase_a + .05*ase_b + .9*rnormal()
replace nota = round(nota, 0.001)

* variável de afetação: distância ao limiar (euros); negativa = 1.º escalão
generate double dist_limiar = rend_ref - `L1'
end

* ------------------------------------------------------------------
* 1. Amostra do capítulo
* ------------------------------------------------------------------
set seed 20251001
gera_amostra

generate long aluno_id = _n
keep aluno_id rapariga ano_esc idade regiao escol_pais dim_agregado ncriancas ///
    monoparental desemprego rend_ref dist_limiar escalao_abono escalao_ase ///
    ase_a ase_b retencao nota
order aluno_id
label variable rapariga      "Aluna (1) ou aluno (0)"
label variable ano_esc       "Ano de escolaridade (5.º a 9.º)"
label variable idade         "Idade no início do ano letivo"
label variable regiao        "Região (NUTS II, continente)"
label variable escol_pais    "Escolaridade mais elevada dos pais"
label variable dim_agregado  "Dimensão do agregado familiar"
label variable ncriancas     "Crianças e jovens com abono no agregado"
label variable monoparental  "Família monoparental"
label variable desemprego    "Progenitor em desemprego há 3 ou mais meses"
label variable rend_ref      "Rendimento de referência anual (euros, rendimentos de 2025)"
label variable dist_limiar   "Rendimento de referência menos 3 657,50 euros"
label variable escalao_abono "Escalão do abono de família (1 a 3)"
label variable escalao_ase   "Escalão da Ação Social Escolar"
label variable ase_a         "Escalão A da ASE"
label variable ase_b         "Escalão B da ASE"
label variable retencao      "Retido no final do ano letivo"
label variable nota          "Pontuação numa prova (escala aprox. padronizada)"
label data "DADOS SIMULADOS para fins pedagógicos (cap. de RDD). Não são dados reais."
notes: Dados simulados por cap10_simulacao.do (semente 20251001). Efeito verdadeiro do escalão A face ao B: retenção -0,050; nota +0,20.
compress
save "../dados/ias_ase_simulado.dta", replace
describe
summarize

* ------------------------------------------------------------------
* 2. Estudo de Monte Carlo: 200 amostras do mesmo PGD
*    Em cada amostra: efeito da elegibilidade (forma reduzida) e efeito
*    do escalão A (RDD difusa) sobre a retenção, com regressão linear
*    local, núcleo triangular, banda ótima e IC robusto a 95%.
*    Valores verdadeiros: forma reduzida = -0,050 x (salto na
*    probabilidade de escalão A); RDD difusa = -0,050.
* ------------------------------------------------------------------
capture program drop umaamostra
program define umaamostra, rclass
  gera_amostra
  generate double folga = -dist_limiar
  quietly rdrobust retencao folga
  return scalar fr = e(tau_cl)
  return scalar fr_l = e(ci_l_rb)
  return scalar fr_r = e(ci_r_rb)
  quietly rdrobust ase_a folga
  return scalar fs = e(tau_cl)
  quietly rdrobust retencao folga, fuzzy(ase_a)
  return scalar fz = e(tau_cl)
  return scalar fz_l = e(ci_l_rb)
  return scalar fz_r = e(ci_r_rb)
end
set seed 4242
simulate fr=r(fr) fr_l=r(fr_l) fr_r=r(fr_r) fs=r(fs) fz=r(fz) fz_l=r(fz_l) fz_r=r(fz_r), ///
    reps(200) nodots: umaamostra
generate byte cobre_fz = (fz_l <= -0.05 & fz_r >= -0.05)
generate double fr_verdadeiro = -0.05*fs
generate byte cobre_fr = (fr_l <= fr_verdadeiro & fr_r >= fr_verdadeiro)
summarize fr fs fz fr_verdadeiro
quietly summarize fz
display "MC|difusa|média=" %7.4f r(mean) "|dp=" %6.4f r(sd) "|mín=" %7.4f r(min) "|máx=" %7.4f r(max)
quietly summarize fr
display "MC|reduzida|média=" %7.4f r(mean) "|dp=" %6.4f r(sd)
quietly summarize fs
display "MC|1.ª fase|média=" %6.4f r(mean)
quietly summarize cobre_fz
display "MC|cobertura IC robusto, difusa=" %5.3f r(mean)
quietly summarize cobre_fr
display "MC|cobertura IC robusto, reduzida=" %5.3f r(mean)
count if fz_r < 0
display "MC|amostras com IC robusto da difusa abaixo de zero=" r(N)
save "../dados/ias_ase_montecarlo.dta", replace

* Figura: distribuição das estimativas do efeito do escalão A (RDD difusa)
set dp comma
twoway (histogram fz, width(0.005) color(gs11) lcolor(gs7)), ///
    xline(-0.05, lcolor(black) lpattern(dash) lwidth(medthick)) ///
    xlabel(, format(%5.3f)) ylabel(, angle(0) format(%4.0f)) ///
    xtitle("Efeito estimado do escalão A na retenção") ytitle("Densidade") ///
    graphregion(color(white)) plotregion(color(white)) xsize(7) ysize(4)
graph export "../figuras/c11_montecarlo.png", replace width(2200)
set dp period
log close
