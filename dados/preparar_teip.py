#!/usr/bin/env python3
"""
Capítulo 9 — Matching
Preparação dos dados do caso TEIP (Territórios Educativos de Intervenção Prioritária).

Gera dados/brutos/teip/teip_agrupamentos.csv, que o do-file stata/cap09_matching.do
importa, rotula e grava em dados/teip_agrupamentos.dta.

Fontes (ficheiros originais em dados/brutos/teip/):
  * InfoEscolas2024_3Ciclo_DadosPorUO.xlsx  — DGEEC/Infoescolas, 3.º ciclo, dados por
    unidade orgânica (anos letivos 2019/20 a 2022/23; publicado em dez. 2024)
    https://infoescolas.medu.pt/docs/2024/InfoEscolas2024_3Ciclo_DadosPorUO.xlsx
  * InfoEscolas2024_2Ciclo_DadosPorUO.xlsx  — idem, 2.º ciclo
  * lista_uo_teip_2021.pdf — DGE, lista das unidades orgânicas TEIP (TEIP3), com código
    DGAE, concelho e fase de entrada no programa
    https://www.dge.mec.pt/sites/default/files/EPIPSE/lista_uo_teip_2021.pdf
  * escolas_teip4.pdf — DGE, lista das unidades orgânicas admitidas ao TEIP4
    (homologada a 16/07/2024; Despacho n.º 7798/2023)
    https://www.dge.mec.pt/sites/default/files/Projetos/TEIP/escolas_teip4.pdf
  * ine_densidade_2011.json — INE, densidade populacional por município, 2011
    (indicador 0008337, API https://www.ine.pt/ine/json_indicador/pindica.jsp)

Regras de construção (ver também o capítulo):
  * universo: unidades orgânicas PÚBLICAS do continente com alunos no 3.º ciclo geral;
  * teip = 1 se a unidade consta da lista TEIP3 de 2021 (fases 1 a 5);
  * teip4_novo = 1 se consta da lista TEIP4 (2024) mas não da lista de 2021
    (entrou no programa só em 2024/25: os seus resultados de 2020/21 a 2022/23 são
    anteriores ao tratamento e servem para um teste placebo);
  * resultados e covariáveis do 3.º ciclo agregados nas três coortes que terminaram
    o ciclo em 2020/21, 2021/22 e 2022/23 (médias ponderadas pelo número de alunos).
"""
import json, os, re, subprocess, unicodedata
import openpyxl
import pandas as pd

AQUI = os.path.dirname(os.path.abspath(__file__))
BR = os.path.join(AQUI, "brutos", "teip")


def norm(s):
    s = unicodedata.normalize("NFKD", str(s).lower())
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = s.replace("´", " ").replace("'", " ")
    s = re.sub(r"\bdr\.?a?\b|\bdra\b|\bprof\.?\b|\beng\.?o?\b", " ", s)
    s = re.sub(r"[^a-z0-9 ]", " ", s)
    s = re.sub(r"\b(agrupamento|escolas?|de|do|da|dos|das|e|n|o|a)\b", " ", s)
    return " ".join(s.split())


def num(x):
    try:
        return float(x)
    except (TypeError, ValueError):
        return None


def folha(livro, nome):
    """Linhas de dados de uma folha Infoescolas, indexadas pelo código da unidade
    (código do agrupamento ou, nas escolas não agrupadas, código da escola)."""
    wb = openpyxl.load_workbook(os.path.join(BR, livro), read_only=True)
    ws = [w for w in wb.worksheets if norm(w.title) == norm(nome)][0]
    out = {}
    for r in ws.iter_rows(min_row=9, values_only=True):
        cod = r[0] if isinstance(r[0], int) else r[1]
        if not isinstance(cod, int):
            continue  # linhas de total, notas e linhas vazias
        out[cod] = r
    return out


# ---------------------------------------------------------------- Infoescolas 3.º ciclo
F3 = "InfoEscolas2024_3Ciclo_DadosPorUO.xlsx"
pop = folha(F3, "Populacao")
ret = folha(F3, "Retencao")
sexo = folha(F3, "Sexo")
cte = folha(F3, "ConclusaoTempoEsperado")
ase = folha(F3, "CTEcomASEeEquidade")
pds = folha(F3, "PercursosDiretosSucesso")
F2 = "InfoEscolas2024_2Ciclo_DadosPorUO.xlsx"
cte2 = folha(F2, "ConclusãoTempoEsperado")
ase2 = folha(F2, "CTEcomASEeEquidade")

linhas = []
for cod, r in pop.items():
    if r[2] != "Pública":
        continue
    d = dict(codigo=cod, nome=r[4], distrito=r[5],
             nao_agrupada=int(r[3] != "Agrupamento"))
    # alunos do 3.º ciclo (7.º a 9.º ano) em 2019/20 e 2022/23
    d["alunos_2020"] = sum(num(x) or 0 for x in r[6:9])
    d["alunos_2023"] = sum(num(x) or 0 for x in r[15:18])
    # taxa de retenção/desistência no 3.º ciclo em 2021/22 e 2022/23, ponderada pelos alunos
    if cod in ret:
        rr = ret[cod]
        num_, den_ = 0.0, 0.0
        for k, (cr, cp) in enumerate([(12, 12), (13, 13), (14, 14), (15, 15), (16, 16), (17, 17)]):
            t, n = num(rr[cr]), num(r[cp])
            if t is not None and n:
                num_ += t * n
                den_ += n
        d["retencao"] = 100 * num_ / den_ if den_ else None
    if cod in sexo:
        d["pct_rapazes"] = 100 * num(sexo[cod][6]) if num(sexo[cod][6]) is not None else None
    # conclusão do 3.º ciclo em 3 anos (três coortes): colunas (n, %, % esperada)
    if cod in cte:
        c = cte[cod]
        a = ase.get(cod)
        n_t = s_obs = s_esp = 0.0
        n_a = n_b = 0.0  # alunos ASE e alunos totais nas coortes com as duas contagens
        for j, ja in ((6, 6), (9, 10), (12, 14)):
            n, o, e = num(c[j]), num(c[j + 1]), num(c[j + 2])
            if n and o is not None and e is not None:
                n_t += n; s_obs += n * o; s_esp += n * e
            if n and a is not None and num(a[ja]) is not None:
                n_a += num(a[ja]); n_b += n
        if n_t:
            d["n_coorte"] = n_t
            d["conclusao"] = 100 * s_obs / n_t
            d["conclusao_esp"] = 100 * s_esp / n_t
        # proporção de alunos com ASE nas mesmas coortes («-» no ficheiro = sem informação)
        if n_b:
            d["pct_ase"] = 100 * n_a / n_b
    # percursos diretos de sucesso (duas coortes: 2021/22 e 2022/23): (n, %, % média nac.)
    if cod in pds:
        p = pds[cod]
        n_t = s_obs = s_esp = 0.0
        for j in (6, 11):
            n, o, e = num(p[j]), num(p[j + 1]), num(p[j + 2])
            if n and o is not None and e is not None:
                n_t += n; s_obs += n * o; s_esp += n * e
        if n_t:
            d["n_pds"] = n_t
            d["pds"] = 100 * s_obs / n_t
            d["pds_esp"] = 100 * s_esp / n_t
    # 2.º ciclo: conclusão em 2 anos (2020/21 a 2022/23) e proporção de ASE
    if cod in cte2:
        c = cte2[cod]
        n_t = s_obs = 0.0
        for j in (9, 12, 15):
            n, o = num(c[j]), num(c[j + 1])
            if n and o is not None:
                n_t += n; s_obs += n * o
        if n_t:
            d["conclusao2"] = 100 * s_obs / n_t
            a2 = ase2.get(cod)
            n_a = n_b = 0.0
            for j, ja in ((9, 6), (12, 10), (15, 14)):
                n = num(c[j])
                if n and a2 is not None and num(a2[ja]) is not None:
                    n_a += num(a2[ja]); n_b += n
            d["pct_ase2"] = 100 * n_a / n_b if n_b else None
    linhas.append(d)

df = pd.DataFrame(linhas)

# ---------------------------------------------------------------- município (a partir do nome)
DIST = {1: 'Aveiro', 2: 'Beja', 3: 'Braga', 4: 'Bragança', 5: 'Castelo Branco', 6: 'Coimbra',
        7: 'Évora', 8: 'Faro', 9: 'Guarda', 10: 'Leiria', 11: 'Lisboa', 12: 'Portalegre',
        13: 'Porto', 14: 'Santarém', 15: 'Setúbal', 16: 'Viana do Castelo', 17: 'Vila Real',
        18: 'Viseu'}
NUTS2 = {"11": "Norte", "16": "Centro", "17": "AM Lisboa", "18": "Alentejo", "15": "Algarve"}
dens = json.load(open(os.path.join(BR, "ine_densidade_2011.json"), encoding="utf-8"))[0]["Dados"]["2011"]
mun = {}
for r in dens:
    g = r["geocod"]
    if len(g) == 7 and g[0] == "1":
        mun[r["geodsg"]] = dict(dist=DIST[int(g[3:5])], dens=float(r["valor"]), nuts2=NUTS2[g[:2]])


def mun_norm(s):
    s = unicodedata.normalize("NFKD", s.lower())
    s = "".join(c for c in s if not unicodedata.combining(c))
    return " ".join(re.sub(r"[^a-z0-9 ]", " ", s).split())


CORRECAO = {160763: "Castelo Branco"}  # Escolas José Sanches e S. Vicente da Beira (Alcains)


def municipio(nome, dist, cod):
    if cod in CORRECAO:
        return CORRECAO[cod]
    cands = [m for m, v in mun.items() if v["dist"] == dist]
    ult = mun_norm(nome.split(",")[-1])
    hit = [m for m in cands if mun_norm(m) == ult]
    if not hit:
        hit = sorted([m for m in cands if re.search(r"\b" + mun_norm(m) + r"\b", mun_norm(nome))],
                     key=len, reverse=True)
    return hit[0] if hit else None


df["municipio"] = [municipio(n, d, c) for n, d, c in zip(df.nome, df.distrito, df.codigo)]
# Exclui as escolas públicas que não pertencem à rede do Ministério da Educação
# (colégios militares e centros da Casa Pia de Lisboa, as únicas sem município no nome)
# e as escolas artísticas especializadas, cujo acesso depende de provas de aptidão.
fora = df.municipio.isna() | df.nome.str.startswith("Escola Artística")
print("Excluídas (fora da rede do ME):", list(df.loc[fora, "nome"]))
df = df[~fora].copy()

# ---------------------------------------------------------------- listas TEIP
txt = subprocess.run(["pdftotext", "-layout", os.path.join(BR, "lista_uo_teip_2021.pdf"), "-"],
                     capture_output=True, text=True).stdout
teip = []
for l in txt.splitlines():
    m = re.match(r"^\s*(\d{6})\s+(\S+(?: \S+)*?)\s{2,}(\S+(?: \S+)*?)\s{2,}(\S+(?: \S+)*?)\s{2,}(.+?)\s{2,}(\d|\(\*\))\s*$", l)
    if m:
        teip.append(dict(codigo=int(m.group(1)), concelho_teip=m.group(4).strip(),
                         nome_teip=m.group(5).strip(),
                         fase=int(m.group(6)) if m.group(6).isdigit() else 9))
    else:
        m = re.match(r"^\s*(\d{6})\s+.*?\s{2,}(Agrupamento.+?|Escola.+?)\s{2,}(\d|\(\*\))\s*$", l)
        if m:
            teip.append(dict(codigo=int(m.group(1)), concelho_teip=None, nome_teip=m.group(2).strip(),
                             fase=int(m.group(3)) if m.group(3).isdigit() else 9))
teip = pd.DataFrame(teip)
assert len(teip) == 146, len(teip)

# Lista TEIP4 (2024), com código DGEEC da escola-sede, fase de entrada (6 = entrada no TEIP4)
# e grupo TEIP4 (1 = em desenvolvimento; 2 = em transição).
txt4 = subprocess.run(["pdftotext", "-layout", os.path.join(BR, "lista_uo_teip4_2024.pdf"), "-"],
                      capture_output=True, text=True).stdout
teip4 = []
for l in txt4.splitlines():
    m = re.search(r"\s(\d{6,7})\s+(\d)\s+((?:Agrupamento|Escola).+?)\s{2,}([12])\s*$", l)
    if m:
        teip4.append(dict(cod_dgeec=int(m.group(1)), fase4=int(m.group(2)),
                          nome4=m.group(3).strip(), grupo4=int(m.group(4))))
    else:
        m = re.search(r"\s(\d{6,7})\s+(\d)\s+((?:Agrupamento|Escola).+?)\s*$", l)
        if m:  # nome partido em duas linhas (o grupo fica na linha seguinte)
            teip4.append(dict(cod_dgeec=int(m.group(1)), fase4=int(m.group(2)),
                              nome4=m.group(3).strip(), grupo4=None))
# linha partida no PDF (nome em duas linhas; código e fase na linha seguinte)
teip4.append(dict(cod_dgeec=914907, fase4=4, grupo4=1,
                  nome4="Agrupamento de Escolas Tenente Coronel Adão Carrapatoso, Vila Nova de Foz Côa"))
teip4 = pd.DataFrame(teip4)
print("Linhas TEIP4 lidas:", len(teip4), "| por fase:", teip4.fase4.value_counts().sort_index().to_dict())


def variantes(nome):
    """Nome normalizado completo e sem o concelho final (texto após a última vírgula)."""
    v = {norm(nome)}
    if "," in nome:
        v.add(norm(nome.rsplit(",", 1)[0]))
    return {x for x in v if x}


# Códigos do Infoescolas: agrupamentos pelo código DGAE; escolas não agrupadas pelo código
# DGEEC da escola. A lista de 2021 usa sempre o código DGAE; nas escolas não agrupadas é
# preciso fazer a correspondência pelo nome.
ref = [(c, v) for c, n in zip(df.codigo, df.nome) for v in variantes(n)]


def por_nome(nome):
    for v in sorted(variantes(nome), key=len, reverse=True):
        hit = sorted({c for c, n in ref if n == v})
        if len(hit) == 1:
            return hit[0]
    return None


MANUAL = {"Agrupamento de Escolas João Maria Botas Carriço, Monforte": 135290,  # antigo AE de Monforte
          "Agrupamento de Escolas NunÁlvares, Seixal": 170859}                # «Nun´Álvares» no Infoescolas
teip["cod_ie"] = [c if c in set(df.codigo) else por_nome(n) for c, n in zip(teip.codigo, teip.nome_teip)]
teip4["cod_ie"] = [c if (c in set(df.codigo) and n.startswith("Escola ")) else
                   MANUAL.get(n) or por_nome(n) for c, n in zip(teip4.cod_dgeec, teip4.nome4)]
for nome_lista, t, col in (("2021", teip, "nome_teip"), ("TEIP4", teip4, "nome4")):
    falta = t[t.cod_ie.isna()]
    if len(falta):
        print(f"Lista {nome_lista}: sem correspondência no Infoescolas (3.º ciclo):")
        for n in falta[col]:
            print("   ", n)

df = df.merge(teip.dropna(subset=["cod_ie"]).rename(columns={"codigo": "cod_dgae"})
              [["cod_ie", "fase", "concelho_teip"]], left_on="codigo", right_on="cod_ie", how="left")
df = df.drop(columns="cod_ie")
df["teip"] = df.fase.notna().astype(int)
df["fase"] = df.fase.fillna(0).astype(int)
t4 = teip4.dropna(subset=["cod_ie"]).drop_duplicates("cod_ie").set_index("cod_ie")
df["teip4"] = df.codigo.isin(t4.index).astype(int)
df["grupo4"] = df.codigo.map(t4.grupo4)
df["fase4"] = df.codigo.map(t4.fase4)
df["teip4_novo"] = ((df.teip4 == 1) & (df.teip == 0)).astype(int)
inc = df[(df.teip4 == 1) & (((df.fase4 == 6) & (df.teip == 1)) | ((df.fase4 < 6) & (df.teip == 0)))]
if len(inc):
    print("Incoerências entre as duas listas:")
    print(inc[["codigo", "nome", "fase", "fase4"]].to_string())

# verificação do município com o concelho da lista TEIP
chk = df[(df.teip == 1) & df.concelho_teip.notna()]
validos = {mun_norm(m) for m in mun}
dif = chk[[mun_norm(a) != mun_norm(b) and mun_norm(b) in validos
           for a, b in zip(chk.municipio, chk.concelho_teip)]]
if len(dif):
    print("Municípios divergentes (corrigidos para o da lista DGE):")
    print(dif[["codigo", "nome", "municipio", "concelho_teip"]].to_string())
    df.loc[dif.index, "municipio"] = dif.concelho_teip.map(
        {c: [m for m in mun if mun_norm(m) == mun_norm(c)][0] for c in dif.concelho_teip})
df["densidade"] = df.municipio.map({m: v["dens"] for m, v in mun.items()})
df["nuts2"] = df.municipio.map({m: v["nuts2"] for m, v in mun.items()})

print("Unidades públicas com 3.º ciclo:", len(df))
print("TEIP (lista 2021) no Infoescolas:", df.teip.sum(), "de", len(teip))
print("TEIP4 no Infoescolas:", df.teip4.sum(), "de", len(teip4), "; novas no TEIP4:", df.teip4_novo.sum())
print(df.groupby("teip")[["conclusao", "pds", "retencao", "pct_ase", "conclusao_esp"]].mean().round(2))

df.drop(columns=["concelho_teip"]).to_csv(os.path.join(BR, "teip_agrupamentos.csv"), index=False,
                                           encoding="utf-8")
teip4.to_csv(os.path.join(BR, "teip4_correspondencia.csv"), index=False, encoding="utf-8")
