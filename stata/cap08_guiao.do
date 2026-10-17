* ==================================================================
*  Capítulo 8 — Séries temporais interrompidas
*  Guião de acompanhamento: corra os blocos pela ordem do capítulo.
*  Cada bloco «*>> nome ... *<<» corresponde a uma caixa do livro.
*  Caso: Lei da Paridade (Lei Orgânica n.º 3/2006) e mulheres eleitas
*  nos executivos municipais, eleições autárquicas de 1979 a 2017.
*  Não precisa de pacotes adicionais.
* ==================================================================
version 18
clear all
set more off
set linesize 80

*>> c08-abrir
use "../dados/paridade_serie.dta", clear
describe
*<<

*>> c08-serie
rename (TotalMulheres ParLaw propostas Total ProMulheres) ///
       (mulheres paridade quotas_vol eleitos prop_mulheres)
tsset time
generate ln_mulheres = ln(mulheres)
generate dif = D.mulheres
generate dif_ln = D.ln_mulheres
list year time mulheres dif ln_mulheres dif_ln, noobs sep(0)
*<<

*>> c08-variaveis
generate pos_tempo = paridade*(time - 8)
list year time paridade pos_tempo, noobs sep(0)
*<<

*>> c08-mqo
regress ln_mulheres time paridade pos_tempo, vce(robust)
*<<

*>> c08-combinacoes
lincom paridade + pos_tempo
lincom time + pos_tempo
*<<

*>> c08-autocorrelacao
quietly regress ln_mulheres time paridade pos_tempo
estat dwatson
estat bgodfrey, lags(1)
estat durbinalt, lags(1)
*<<

*>> c08-newey
newey ln_mulheres time paridade pos_tempo, lag(1)
*<<

*>> c08-prais
prais ln_mulheres time paridade pos_tempo
*<<

*>> c08-contrafactual
quietly regress ln_mulheres time paridade pos_tempo, vce(robust)
predict ln_prev, xb
generate prev = exp(ln_prev)
generate contraf = exp(_b[_cons] + _b[time]*time) if time >= 8
list year mulheres prev contraf if time >= 8, noobs
*<<

*>> c08-painel
use "../dados/paridade_municipios.dta", clear
rename (Women_in_office GenderQuotas) (mulheres paridade)
generate pos_tempo = paridade*(time - 8)
xtset id time
xtpoisson mulheres time paridade pos_tempo, fe vce(robust) irr
*<<
