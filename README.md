# 🗺️ GeoSampa — O Garimpeiro de Tesouros da Cidade

> Imaginou ter uma **pá mágica** que abre o baú de mapas da Prefeitura de São Paulo
> e traz pra você, em segundos, **onde ficam** as UBS, escolas, bibliotecas, feiras,
> praças e muito mais? Pois é exatamente isso que este projeto faz! 🦾✨

## O que é isso? 🧐

O **GeoSampa** é o mapa digital oficial da Prefeitura (aquele site de mapas com
centenas de camadas). O site em si não deixa robôs entrarem, mas as **portas dos
fundos** — os serviços web abertos — entregam os dados de verdade para qualquer
um. A gente entra por essas portas e **garimpa os equipamentos públicos**:

- 🏥 **Saúde**: UBS, hospitais, pronto-socorros
- 🏫 **Educação**: escolas, CEUs, creches
- 🎭 **Cultura**: bibliotecas, museus, teatros
- ⚽ **Esporte**: clubes, centros esportivos, estádios
- 🍅 **Serviços**: feiras livres, mercados, wi-fi livre
- 🛡️ **Segurança**: delegacias, bombeiros, guarda civil
- 🤝 **Assistência**: CRAS, conselhos tutelares, Bom Prato

E cada tesouro sai em **dois formatos**: o **GeoJSON** (o mapa de verdade, em
metros — EPSG:31983) e o **CSV** (a planilha, com `latitude` e `longitude` em graus,
do jeito que o Google Maps entende).

📖 A história completa e cheia de detalhes está em **[DOCUMENTACAO.md](DOCUMENTACAO.md)**.

---

## Preparando a mochila 🎒

Requisitos: **R** (testado na 4.6.1) e alguns pacotes. Só na primeira vez:

```r
install.packages(c("httr", "jsonlite", "sf", "readr", "xml2"))
```

> 💡 Para os mapas (estático e interativo) e análises do módulo de CEP, instale
> também os opcionais:
> ```r
> install.packages(c("ggplot2", "leaflet", "htmlwidgets"))   # mapas
> install.packages(c("spdep"))                               # Moran's I, LISA, Getis-Ord
> install.packages(c("osrm"))                                # distância por rede viária
> install.packages(c("spatstat"))                            # função K de Ripley
> install.packages(c("htmltools", "base64enc"))              # relatório HTML
> install.packages(c("testthat"))                            # testes automatizados
> ```
> Tudo funciona sem eles — as funções avisam quando um pacote opcional falta.

---

## Passo a passo — do zero ao tesouro 🚀

### Passo 1: entre na caverna do projeto 🕳️

No R ou no RStudio, a partir da pasta do projeto:

```r
setwd("/home/paulo/Documentos/meus_codigos/ph_codes/R/geosampa")
```

> 💡 No RStudio é ainda mais fácil: `Session > Set Working Directory > To Project Directory`.

### Passo 2: carregue as ferramentas do garimpeiro 🧰

Use o atalho (ele acha a pasta certa sozinho e carrega tudo **em silêncio** —
nada de aquela chuva de `[[1]]`, `[[2]]`, `[[3]]...` na tela):

```r
source("scripts/carregar_funcoes.R")
```

Deve aparecer: `✅ Funções do GeoSampa carregadas! Boa garimpagem! 🗺️✨`

> 🔁 Preferia o jeito antigo? `invisible(lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), source))`
> faz a mesma coisa. O `invisible()` só esconde aquele monte de números de `[[ ]]`.

### Passo 3: mãos à obra! 🔥

Veja o que existe, baixe, garimpe tudo:

```r
gs_catalogo_equipamentos()                        # o cardápio completo de camadas
gs_baixar_servicos("saude")                       # baixa TODAS as camadas de saúde
gs_baixar_camada("equipamento_cultura_bibliotecas") # baixa só as bibliotecas
gs_baixar_todos_equipamentos()                    # baixa TUDO (o grande garimpo!)
gs_metadados("UBS")                               # procura o "RG" das camadas
```

Pronto! Os arquivos caem na pasta `data/`:

```
data/
├── equipamento_saude_ubs_posto_centro.geojson   🗺️ o mapa
├── equipamento_saude_ubs_posto_centro.csv       📊 a planilha com lat/long
└── ... (mais de 80 pares de arquivos)
```

---

## O manual das ferramentas 🧰

| Função | O que faz | Exemplo |
|--------|-----------|---------|
| `gs_catalogo_equipamentos()` | Mostra o "cardápio" de equipamentos | `gs_catalogo_equipamentos()` |
| `gs_baixar_servicos(tema)` | Baixa um tema inteiro | `gs_baixar_servicos("educacao")` |
| `gs_baixar_camada(camada)` | Baixa uma camada específica | `gs_baixar_camada("equipamento_feira_livre")` |
| `gs_baixar_todos_equipamentos()` | Baixa **tudo** de uma vez | `gs_baixar_todos_equipamentos()` |
| `gs_buscar_camadas("termo")` | Procura camadas por palavra-chave | `gs_buscar_camadas("ceu")` |
| `gs_metadados("UBS")` | Acha os metadados (RG dos dados) | `gs_metadados("UBS")` |
| `gs_metadado_registro(uuid)` | Mostra o RG completo (título, resumo, órgão) | `gs_metadado_registro("12f1...")` |

---

## 🧭 E o CEP? O que tem por perto? (o radar mágico)

> Pegue o **radar mágico**: digite um CEP e ele diz onde é, se a coordenada
> confere, o que tem por perto (com distância!), desenha o mapa e ainda analisa
> a distribuição dos serviços. É o módulo de **CEP + coordenadas + serviços
> próximos + mapas + análises espaciais**. 📡✨

### Os conceitos rapidinhos 🎓

- **CEP** é o "CPF do endereço": 8 dígitos que identificam um lugar (aceitamos
  `03175-001` ou `03175001`).
- **Geocodificação** é traduzir o endereço em **coordenada** (latitude e
  longitude). Nosso sistema faz em **cascata**: primeiro o **índice local**
  (offline, feito dos seus próprios `data/*.csv` — hoje com **+7 mil CEPs**),
  depois **viaCEP** (valida o endereço) e **Nominatim/OSM** (acha a coordenada
  na internet, com 1s de pausa entre consultas). Se o CEP não estiver no índice
  local, o sistema usa o **endereço do viaCEP** para pedir a **rua** ao
  Nominatim — e, em último caso, o centróide da cidade.
- **Latitude** = norte/sul; **longitude** = leste/oeste. São Paulo fica em
  ~(-23,5°, -46,6°).
- **Distância**: tem "linha reta" (geodésica, euclidiana, haversine) e tem o
  "caminho real de carro/pé" (rede viária via OSRM).
- **Raio** é o círculo mágico ao redor do ponto: tudo dentro dele é "próximo".
- **Análises espaciais**: contagens, vizinho mais próximo, acessibilidade,
  cobertura, NNI, Voronoi, KDE, raios progressivos, Moran's I, LISA,
  Getis-Ord, Ripley's K e distritos — tudo com um comando, com direito a
  relatório consolidado em HTML/Markdown.

> 🎒 Preparou o ambiente? `source("scripts/carregar_funcoes.R")` — pronto!

### Passo a passo no R 🚀

**1) Valide o CEP e veja o endereço:**
```r
gs_ler_cep("03175-001")
#        cep         logradouro        bairro    cidade uf    ibge
# 1 03175-001 Rua Serra de Jairé Quarta Parada São Paulo SP 3550308
```

**2) Transforme o CEP em coordenada (lat/long):**
```r
gs_cep_para_coordenadas("03175-001")
#        cep  latitude longitude fonte
# 1 03175-001 -23.55334 -46.58032 local
```

**3) Confira se uma coordenada bate com o CEP:**
```r
gs_verificar_cep("03175-001", -23.553640, -46.580180)
# $confere   : logi TRUE
# $veredito  : chr "CONFERE"        # tolerância padrão: 300 m
# $distancia_m: num 0
```

**4) Ache os serviços próximos (raio em metros):**
```r
proximos <- gs_servicos_proximos(cep = "03175-001", raio_m = 2000,
                                 camadas = c("equipamento_saude_ubs_posto_centro",
                                             "equipamento_bombeiros"))
head(proximos[, c("camada", "nome", "distancia_m")])
```

> 🎯 Em `camadas` você pode passar o **tema** (`"saude"` expande para todas as
> camadas de saúde), o **nome completo** (`"equipamento_saude_ubs_posto_centro"`),
> um **pedaço do nome** (`"ubs"`), ou até um `data.frame` vindo de
> `gs_catalogo_equipamentos()`. Para ver todas as opções: `gs_listar_servicos()`.

**5) Desenhe o mapa (interativo HTML ou estático PNG):**
```r
gs_mapa_servicos(proximos, interativo = TRUE,  salvar = "mapas/cep_03175001.html")
gs_mapa_servicos(proximos, interativo = FALSE, salvar = "mapas/cep_03175001.png")
```

> 🖱️ No HTML: ao passar o mouse aparece um tooltip (nome/tipo/distância) e ao
> clicar, o popup completo. No PNG: legenda no rodapé em várias colunas (com
> rótulos quebrados para caber), resolução ajustável via `largura`, `altura` e
> `dpi` — a altura padrão se ajusta automaticamente ao número de itens da
> legenda para ela não ser cortada.

**6) Analise a distribuição (escolha o `tipo`):**
```r
analises <- gs_analise_servicos(proximos,
                                tipo = c("descritivas", "raios_progressivos",
                                         "acessibilidade_media", "nni"))
analises$raios_progressivos$contagem   # tabela
analises$acessibilidade_media$grafico_ecdf  # curva acumulada das distâncias
```

**7) Gere o relatório consolidado (HTML auto-contido ou Markdown):**
```r
gs_relatorio_analises(proximos,
                      tipo = c("descritivas", "raio_otimo", "nni"),
                      arquivo = "relatorios/relatorio.html")

# Exporte as tabelas e polígonos em CSV/GeoJSON:
gs_exportar_resultado(proximos, analises, dir = "saidas")
```
> 📝 Cada seção do relatório vem com **tabela + gráfico + parágrafo de
> interpretação automática** dos principais resultados (mediana, percentis,
> R do NNI, Moran etc.), para as análises ficarem explicadas.

### O manual do CEP 🧰

| Função | O que faz | Exemplo |
|--------|-----------|---------|
| `gs_indice_cep()` | Monta o índice local CEP → coordenadas (offline) | `gs_indice_cep(force = TRUE)` |
| `gs_listar_servicos()` | Lista os serviços locais por tema (o que usar em `camadas`) | `gs_listar_servicos("saude")` |
| `gs_ler_cep(cep)` | Valida o CEP e devolve o endereço | `gs_ler_cep("03175-001")` |
| `gs_cep_para_coordenadas(cep)` | Lat/long do CEP (índice local → Nominatim) | `gs_cep_para_coordenadas("03175001")` |
| `gs_verificar_cep(cep, lat, lon)` | Confere se a coordenada bate com o CEP | `gs_verificar_cep("03175-001", -23.5536, -46.5802)` |
| `gs_servicos_proximos(...)` | Serviços dentro do raio, mais próximos primeiro | `gs_servicos_proximos(cep="03175-001", raio_m=2000)` |
| `gs_tipos_distancia()` | Manual das métricas de distância | `gs_tipos_distancia()` |
| `gs_mapa_servicos(...)` | Mapa estático (PNG/PDF) ou interativo (HTML) | `gs_mapa_servicos(cep="03175-001", raio_m=2000)` |
| `gs_analise_servicos(...)` | Análises estatísticas/espaciais | `gs_analise_servicos(cep="03175-001", tipo="descritivas")` |
| `gs_relatorio_analises(...)` | Relatório consolidado (HTML/MD) com as análises | `gs_relatorio_analises(proximos, arquivo="relatorios/r.html")` |
| `gs_exportar_resultado(...)` | Exporta resultados em CSV/GeoJSON | `gs_exportar_resultado(proximos, analises, dir="saidas")` |

### Tipos de distância 📏

| Tipo | O que mede | Quando usar |
|------|-----------|-------------|
| `geodesica` (padrão) | Elipsoidal, precisa | Em geral |
| `euclidiana` | Em metros (UTM/SIRGAS2000) | Rápida, até ~20 km |
| `haversine` | Sobre a esfera | Leve, sem transformar CRS |
| `manhattan` | \|Δx\|+\|Δy\| em metros | Caminhabilidade |
| `rede_viaria` | Rota real de carro (OSRM) | Requer pacote `osrm` (opcional) |

### Análises disponíveis 📊

| Tipo | O que devolve | Dependência |
|------|---------------|-------------|
| `descritivas` | Contagens por tipo/camada, resumo, histograma e boxplot anotados (mediana/média) | nenhuma |
| `vizinho_mais_proximo` | Distância ao serviço mais próximo (geral e por camada) | nenhuma |
| `acessibilidade_media` | Resumo robusto das distâncias (mediana, P25/P75, IQR, CV) + curva ECDF | nenhuma |
| `raio_otimo` | Raio que alcança 50%, 75%, 90% e 95% dos serviços + gráfico ECDF | nenhuma |
| `cobertura_buffer` | Área coberta por buffers por camada vs casco convexo | nenhuma |
| `nni` | Índice de Vizinho Mais Próximo (agrupado/aleatório/disperso) | nenhuma |
| `voronoi` | Polígonos de Thiessen (áreas de influência) | nenhuma |
| `kde` / `kde_banda` | Mapa de densidade de kernel (banda padrão / estimada) | nenhuma |
| `raios_progressivos` | Oportunidades acumuladas por raio (tabela + curva) | nenhuma |
| `getis_ord` | Getis-Ord G* local (aglomerados quentes/frios por célula hex) | `spdep` (opcional) |
| `lisa` | Moran local (LISA) por célula hexagonal | `spdep` (opcional) |
| `ripley_k` | Função K de Ripley (agregação em múltiplas escalas) | `spatstat` (opcional) |
| `moran` | Moran's I sobre contagens em grade hexagonal (padrão) | `spdep` (opcional) |
| `moran_distrital` | Moran's I agregado por distrito (LISA por distrito) | `spdep` (opcional) |
| `por_distrito` | Contagem e densidade de serviços por distrito (mapa) | internet (1ª vez) |
| `cobertura_populacional` | População atendida no raio (por camada de população ou densidade) | opcional |
| `rede_viaria` | Distância de percurso (OSRM) comparada à linha reta | `osrm` (opcional) |

> 💡 As análises que dependem de pacote opcional **não quebram** se o pacote
> faltar: devolvem uma mensagem orientando a instalação.

> 📖 Quer o **curso completo** de cada conceito, com explicação lúdica e
> exemplos passo a passo? É a **seção 10** do **[DOCUMENTACAO.md](DOCUMENTACAO.md)**
> — do "o que é um CEP?" até Moran's I. 🌟

### Bônus: abrindo os tesouros baixados 🧑🔬

```r
# O mapa (GeoJSON)
ubs <- sf::st_read("data/equipamento_saude_ubs_posto_centro.geojson")

# Ou a planilha, com latitude/longitude prontas
tab <- readr::read_csv("data/equipamento_saude_ubs_posto_centro.csv")
head(tab[, c("nm_equipamento", "nm_bairro_equipamento", "latitude", "longitude")])
```

---

## Sem abrir o R? Sem problema! 💻

Pode usar direto no terminal:

```bash
Rscript scripts/carregar_funcoes.R                          # só carrega as funções
Rscript scripts/baixar_tudo.R                               # baixa TUDO
Rscript scripts/baixar_tudo.R saude                         # baixa só saúde
Rscript scripts/baixar_tudo.R educacao esporte              # educação e esporte
Rscript scripts/baixar_tudo.R --camada equipamento_saude_ubs_posto_centro   # uma camada
```

---

## Estrutura do baú 📦

```
geosampa/
├── R/
│   ├── 00_config.R        → endereços, projeções e pastas
│   ├── gs_camadas.R       → o cardápio de camadas (GetCapabilities)
│   ├── gs_baixar.R        → o garimpo de verdade (WFS → GeoJSON + CSV)
│   ├── gs_metadados.R     → os "RGs" das camadas (GeoNetwork)
│   ├── gs_indice_cep.R    → índice local CEP → coordenadas (offline)
│   ├── gs_cep.R           → ler/geocodificar/verificar CEP
│   ├── gs_proximidade.R   → serviços próximos + tipos de distância
│   ├── gs_mapa.R          → mapas estático (ggplot2) e interativo (leaflet)
│   ├── gs_analise.R       → análises estatísticas e espaciais
│   └── gs_relatorio.R     → relatório consolidado e exportação
├── scripts/
│   ├── carregar_funcoes.R → atalho: carrega tudo em silêncio
│   └── baixar_tudo.R      → o botão "baixar tudo" do terminal
├── tests/                 → testes automatizados (testthat)
├── data/                  → os tesouros baixados (GeoJSON + CSV)
├── DOCUMENTACAO.md        → a história completa, capítulo por capítulo
└── README.md              → este guia rapidinho
```

---

## Fontes oficiais 📚

- Portal GeoSampa: https://geosampa.prefeitura.sp.gov.br
- Tutorial: https://geoinfo-smdu.github.io/tutorial-GeoSampa/
- Metadados: https://metadados.geosampa.prefeitura.sp.gov.br
- WFS (os dados): https://wfs.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wfs
- Projeção oficial: SIRGAS2000 / UTM 23S — **EPSG:31983**

Dados abertos da **Prefeitura de São Paulo — Geoinfo (SMUL)**.

**Boa garimpagem!** 🗺️✨
