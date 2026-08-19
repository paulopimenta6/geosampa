# 🗺️ O Baú de Tesouros do GeoSampa

> Uma viagem divertida para entender o que é o GeoSampa, o que tem lá dentro
> e como criamos um "garimpeiro robô" que baixa dados de equipamentos públicos
> da cidade de São Paulo — tudo com linguagem simples e exemplos prontos.

---

## 1. Era uma vez... a cidade de São Paulo

São Paulo tem mais de **11 milhões de habitantes**, **310 setores**, **55 mil quadras**
e **2,5 milhões de lotes**. Para cuidar de uma cidade tão gigante, a Prefeitura precisa
saber **onde** estão as coisas: onde tem posto de saúde, escola, biblioteca, praça,
parque, feira livre, hospital... Isso é chamado de **dado geográfico**: um dado que
tem um **endereço no mapa** (coordenadas), além de informações como nome, telefone e
horário de funcionamento.

Mas onde a Prefeitura guarda tudo isso? Num lugar muito especial chamado...

## 2. O GeoSampa: o mapa digital da cidade 🏙️

O **GeoSampa** é o portal de mapas da Prefeitura de São Paulo, disponível em
[geosampa.prefeitura.sp.gov.br](https://geosampa.prefeitura.sp.gov.br). Pense nele
como um **baú de tesouros cartográficos**: lá dentro existem **mais de 300 camadas
de informações** sobre a cidade — saúde, educação, transporte, zoneamento, IPTU,
riscos, vegetação e muito mais.

Imagine cada camada como uma **folha transparente** desenhada em cima do mapa da cidade:

- Uma folha com todos os postos de saúde 🏥
- Outra com as escolas 🏫
- Outra com as praças e parques 🌳
- Outra com as feiras livres 🍅

Junte todas as folhas e você tem a cidade completa em detalhes!

### Como funciona por dentro

O GeoSampa foi construído com tecnologias abertas (GeoServer + OpenLayers) e usa
padrões internacionais chamados **OGC**. O importante para nós são os **serviços web**,
que são como "portas" por onde os dados saem do baú. São duas portas principais:

| Porta | Nome | O que ela entrega? | Analogia |
|-------|------|---------------------|----------|
| 🖼️ | **WMS** (Web Map Service) | Imagens do mapa | Um **espelho**: mostra, mas você não pode tocar |
| 🗄️ | **WFS** (Web Feature Service) | Os dados de verdade (vetores) | Um **baú**: abre e você pega os dados |

- **WMS** serve imagens prontas (mapas). Útil para olhar, mas não dá para cruzar dados.
- **WFS** entrega os **dados brutos** com coordenadas. É por aqui que o nosso
  "garimpeiro robô" entra! 🦾

> ⚠️ Curiosidade: a página visual do GeoSampa tem uma trava de segurança (um desafio
> de JavaScript) que impede robôs de entrarem pela porta da frente. Por isso, a
> maneira correta e **oficial** de baixar dados é usar as portas WMS/WFS — que são
> abertas a qualquer pessoa. É exatamente o que nosso sistema faz.

### O idioma dos mapas (projeção cartográfica) 🌐

Coordenadas no mapa são como um "idioma". O GeoSampa fala vários idiomas, mas o
**oficial** (desde 2014) é o **SIRGAS2000 / UTM 23S**, código **EPSG:31983**.

- **EPSG** é um catálogo mundial que dá números para cada "idioma" de coordenadas.
- UTM 23S é um jeito de "achatar" a região de São Paulo para medir em **metros**.
- Para planilhas, costumamos converter para **WGS84 (EPSG:4326)**, que usa
  **latitude e longitude em graus** — o formato que o Google Maps entende.

Nosso sistema respeita os dois idiomas:
- O arquivo **.geojson** fica no idioma oficial (EPSG:31983), perfeito para GIS.
- O arquivo **.csv** ganha colunas `latitude` e `longitude` em graus (EPSG:4326),
  fácil de abrir em qualquer planilha.

---

## 3. E os "documentos de identidade" dos dados? (Metadados) 📇

Toda camada de dados tem um **metadado** — que significa literalmente **"dados sobre
os dados"**. É o RG da camada: diz quem produziu, do que se trata, desde quando
existe, em que escala, qual a periodicidade de atualização.

O GeoSampa mantém esses documentos num **Catálogo de Metadados Geográficos**
(baseado no software livre **GeoNetwork**), disponível em
[metadados.geosampa.prefeitura.sp.gov.br](https://metadados.geosampa.prefeitura.sp.gov.br).

Quando você pesquisa uma camada, o catálogo devolve um **UUID** (uma "impressão
digital" única) e um registro com **título, resumo e órgão responsável**. Isso é
importante para você saber **em quem confiar** na hora de usar o dado.

---

## 4. Nossa missão: o "Garimpeiro de Equipamentos" 🦾

A Prefeitura disponibiliza camadas de **equipamentos públicos** — todos aqueles
pontos da cidade que prestam serviços: UBS, hospitais, CEUs, escolas, CRAS,
bibliotecas, museus, centros esportivos, feiras livres, mercados, praças, e por
aí vai. São **48 camadas** e mais de **18 mil equipamentos**!

Criamos um sistema em **R** que:

1. 🗂️ **Pergunta ao GeoSampa** quais camadas existem (via GetCapabilities).
2. 🔍 **Acha** as camadas de equipamentos (as que têm o prefixo `equipamento_`).
3. ⬇️ **Baixa** cada uma pelo WFS, em páginas (para não sobrecarregar o servidor).
4. 💾 **Salva** em dois formatos: **GeoJSON** (o mapa) e **CSV** (a tabela).
5. 📇 **Consulta os metadados** das camadas no catálogo.

### Onde mora o código

```
geosampa/
├── R/
│   ├── 00_config.R        → endereços, projeções e pastas
│   ├── gs_camadas.R       → catálogo de camadas (GetCapabilities)
│   ├── gs_baixar.R        → o download de verdade (WFS → GeoJSON + CSV)
│   └── gs_metadados.R     → consulta ao catálogo de metadados
├── scripts/
│   └── baixar_tudo.R      → o botão "baixar tudo"
├── data/                  → os tesouros baixados (GeoJSON + CSV)
├── DOCUMENTACAO.md        → este documento
└── README.md              → guia rápido
```

### As ferramentas do garimpeiro 🧰

| Função | O que faz |
|--------|-----------|
| `gs_camadas_disponiveis()` | Lista todas as camadas que o GeoSampa oferece |
| `gs_catalogo_equipamentos()` | Mostra só as camadas de equipamentos públicos |
| `gs_buscar_camadas("saude")` | Procura camadas por palavra-chave |
| `gs_baixar_camada("...ubs...")` | Baixa uma camada inteira |
| `gs_baixar_servicos("saude")` | Baixa todas as camadas de um tema |
| `gs_baixar_todos_equipamentos()` | Baixa tudo! (o grande garimpo) |
| `gs_metadados("UBS")` | Procura registros de metadados |
| `gs_metadado_registro(uuid)` | Mostra o "RG" completo de uma camada |

---

## 5. Passo a passo para você usar 🚀

### Antes de começar

Rode o R a partir da pasta do projeto e carregue as funções:

```r
# Carrega todas as funções do projeto
lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)
```

### Exemplo 1: ver o que existe

```r
# Todas as camadas de equipamentos e seus temas
gs_catalogo_equipamentos()
```

### Exemplo 2: baixar um tema (saúde, por exemplo)

```r
gs_baixar_servicos("saude")
```

Os arquivos caem em `data/`:

- `data/equipamento_saude_ubs_posto_centro.geojson` (o mapa)
- `data/equipamento_saude_ubs_posto_centro.csv` (a tabela)

### Exemplo 3: baixar UMA camada específica

```r
gs_baixar_camada("equipamento_cultura_bibliotecas")
```

### Exemplo 4: baixar TUDO de uma vez

```r
gs_baixar_todos_equipamentos()
```

Ou, direto pelo terminal:

```bash
Rscript scripts/baixar_tudo.R            # tudo
Rscript scripts/baixar_tudo.R saude      # só saúde
Rscript scripts/baixar_tudo.R educacao esporte   # educação e esporte
```

### Exemplo 5: consultar metadados

```r
reg <- gs_metadados("UBS")
reg$uuid            # a "impressão digital" do registro
gs_metadado_registro(reg$uuid[1])   # o RG completo (título, resumo, órgão)
```

### Exemplo 6: abrir os dados baixados

```r
ubs <- sf::st_read("data/equipamento_saude_ubs_posto_centro.geojson")

# ... ou em forma de tabela (CSV), com latitude/longitude prontas:
tab <- readr::read_csv("data/equipamento_saude_ubs_posto_centro.csv")
head(tab[, c("nm_equipamento", "nm_bairro_equipamento", "latitude", "longitude")])
```

> 💡 **Dica de bairro**: quer filtrar UBS por tipo? Use o filtro do WFS:
> ```r
> gs_baixar_camada("equipamento_saude_ubs_posto_centro",
>                  filtro = "nm_tipo_equipamento LIKE '%Centro%'")
> ```

---

## 6. O que já está no baú 📦

Rodamos o sistema e o baú ficou assim (em `data/`):

| Tema | Camadas | Alguns exemplos |
|------|---------|-----------------|
| 🏥 Saúde | 12 | UBS, hospitais, urgência/emergência, saúde mental, DST/AIDS |
| 🏫 Educação | 7 | CEU, ensino fundamental/médio, educação infantil, ensino técnico |
| 🎭 Cultura | 5 | Bibliotecas, museus, teatros/cinema, espaços culturais |
| ⚽ Esporte | 5 | Centros esportivos, clubes, clubes da comunidade, estádios |
| 🤝 Assistência social | + | CRAS, conselhos tutelares, Bom Prato, casa mediação |
| 🍅 Serviços urbanos | + | Feiras livres, mercados, sacolões, wi-fi livre, shoppings |
| 🛡️ Segurança | + | Polícia civil, polícia militar, bombeiros, guarda civil |
| 🌳 Espaços verdes | + | Praças e largos, parques |

**Números reais do último garimpo:**
- ✅ **48 camadas** baixadas com sucesso
- 📊 **18.608 equipamentos** catalogados
- 💾 **~54 MB** de dados (48 GeoJSON + 48 CSV)

> 💡 Nem toda camada chamada de "equipamento" pelo sistema é um ponto de serviço
> no sentido clássico (por exemplo, há camadas de abrangência/cobertura e de
> coordenação regional). Confira o `tema` e o `titulo` no catálogo
> (`gs_catalogo_equipamentos()`) para escolher as que interessam.

---

## 7. Como o download funciona por dentro 🔧

Quando você pede para baixar uma camada, o robô:

1. **Conta** quantos registros existem (uma consulta rápida).
2. **Busca em páginas** de 1.000 registros (para ser gentil com o servidor).
   - ⚠️ Se uma camada não tem chave primária, o GeoServer reclama de paginação.
     Nosso código detecta isso e ordena por um atributo estável (por exemplo,
     `cd_identificador`) — e se ainda assim não der, busca tudo de uma vez.
3. **Junta** todas as páginas num único GeoJSON (na projeção oficial EPSG:31983).
4. **Converte** para CSV adicionando `latitude` e `longitude` em graus (EPSG:4326).

Detalhe importante: o WFS devolve um campo `numberMatched` dizendo quantos
registros existem — é assim que o robô sabe quando parar de buscar.

---

## 8. Respeito e boas práticas 🤝

- Os dados do GeoSampa são **abertos** (o portal usa licença de dados abertos),
  mas é sempre bom citar a fonte: **Prefeitura de São Paulo / Geoinfo (SMUL)**.
- Consulte o **metadado** da camada antes de usá-la em análises sérias — ele
  informa escala, atualização e órgão responsável.
- Não faça download em rajada: nosso sistema já pausa entre páginas. Se for
  baixar muitas camadas de uma vez, dê um tempo entre execuções.

---

## 9. Fontes oficiais 📚

| O quê | Onde |
|-------|------|
| Tutorial do GeoSampa | https://geoinfo-smdu.github.io/tutorial-GeoSampa/ |
| Portal GeoSampa | https://geosampa.prefeitura.sp.gov.br |
| Catálogo de Metadados | https://metadados.geosampa.prefeitura.sp.gov.br |
| Serviço WFS (camadas) | https://wfs.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wfs |
| Serviço WMS (imagens) | https://wms.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wms |
| Projeção oficial | SIRGAS2000 / UTM 23S — EPSG:31983 |

---

## 10. Bônus: o módulo de CEP, serviços próximos, mapas e análises 🌟

> Imagine que você está em pé numa esquina de São Paulo, segurando um **radar
> mágico**. Você digita um CEP e o radar responde: *"este endereço é aqui"*,
> *"a coordenada que me deram bate? sim!"* e *"olha, tem 12 serviços num raio
> de 2 km — o mais perto é uma UBS a 36 metros!"* — e ainda desenha o mapa e
> diz se os serviços estão "grudadinhos" ou espalhados. É exatamente isso que
> este módulo faz. 🧭📡

### 10.1 A missão: quatro perguntas e uma resposta

O módulo responde a quatro perguntas clássicas de quem mora (ou trabalha) na
cidade:

| Pergunta | Função |
|----------|--------|
| "Este CEP é válido? Qual o endereço?" | `gs_ler_cep()` |
| "Onde fica este CEP no mapa?" | `gs_cep_para_coordenadas()` |
| "Esta coordenada que me deram confere com o CEP?" | `gs_verificar_cep()` |
| "O que tem por perto? Como está distribuído?" | `gs_servicos_proximos()`, `gs_mapa_servicos()`, `gs_analise_servicos()` |

Antes de apertar os botões, vamos entender as peças do brinquedo. 🧩

### 10.2 Os conceitos, do jeito simples 🎓

**CEP — o "CPF" do endereço.** O CEP (Código de Endereçamento Postal) é um
número de 8 dígitos que identifica um endereço ou uma faixa de endereços. Na
nossa caixa de ferramentas, ele vira a "chave" para procurar coisas no mapa.
Um CEP não é um ponto exato: pode representar uma rua inteira. Por isso
falamos em "coordenada de referência".

**Geocodificação — traduzir endereço em coordenada.** É o ato de transformar
"Rua Serra de Jairé, 340" em um par de números: latitude e longitude. Nosso
sistema faz isso em **cascata**: primeiro consulta o índice local (offline,
instantâneo), depois a internet (viaCEP para o endereço e Nominatim/OSM para
a coordenada). Se o CEP não está no índice local, o sistema usa o **endereço
do viaCEP** para pedir a **rua** ao Nominatim — e, em último caso, o
**centróide da cidade**. A primeira fonte que responder, vence. 🏆

**Latitude e longitude — a teia de aranha da Terra.** Imagine uma teia de
aranha envolvendo o planeta:
- **Latitude** diz o quanto você está **ao norte ou ao sul** da linha do
  Equador (varia de -90 a +90; São Paulo fica em ~-23,5°, negativo = sul).
- **Longitude** diz o quanto você está **a leste ou a oeste** do meridiano de
  Greenwich (varia de -180 a +180; São Paulo fica em ~-46,6°, negativo = oeste).

**Distância — linha reta ou rua de verdade?** Existem vários jeitos de medir
"quão longe" está um serviço. São os **tipos de distância** do módulo (tabela
na seção 10.5). Alguns medem em linha reta (ignorando prédios e ruas), outros
tentam imitar o caminho real.

**Raio (buffer) — o círculo mágico.** Ao redor do ponto de interesse, você
traça um círculo de raio X metros. Tudo o que cair dentro do círculo é
considerado "próximo". O módulo usa essa ideia em `gs_servicos_proximos()` e
desenha o círculo nos mapas. 🔵

**Mapa estático vs interativo.** O mapa **estático** (ggplot2) vira uma
imagem PNG/PDF — ótimo para relatórios. O mapa **interativo** (leaflet) vira
um arquivo HTML que abre no navegador com zoom, arrastar e "bolhinhas"
(popups) com os detalhes de cada serviço. 🖱️

**Análise espacial — a ciência do "onde".** Além de listar serviços, o módulo
responde perguntas de estatística espacial: *"quantos serviços tem em cada
raio?"*, *"qual é o mais próximo?"*, *"quais as áreas de influência?"*
(Voronoi), *"tem zonas de concentração?"* (KDE) e *"os serviços se agrupam
mais do que o acaso?"* (Moran's I). Tudo com funções que devolvem tabelas,
polígonos e gráficos prontos. 📊

### 10.3 Preparando o ambiente 🎒

Só na primeira vez:

```r
install.packages(c("httr", "jsonlite", "sf", "readr", "xml2"))
```

Opcionais (o módulo avisa se faltarem):

```r
install.packages(c("ggplot2", "leaflet", "htmlwidgets"))  # mapas
install.packages(c("spdep"))                              # Moran's I, LISA, Getis-Ord
install.packages(c("osrm"))                               # rede viária
install.packages(c("spatstat"))                           # função K de Ripley
install.packages(c("htmltools", "base64enc"))             # relatório HTML
install.packages(c("testthat"))                           # testes automatizados
```

E carregue as funções:

```r
source("scripts/carregar_funcoes.R")
```

### 10.4 Missão 1 — Ler e validar um CEP 📮

```r
gs_ler_cep("03175-001")
```

```
        cep         logradouro        bairro    cidade uf    ibge
1 03175-001 Rua Serra de Jairé Quarta Parada São Paulo SP 3550308
```

O que aconteceu? O **viaCEP** (serviço público dos Correios, sem chave de
acesso) validou o CEP e devolveu endereço, bairro, cidade, UF e código IBGE.
Se o CEP não existir, você recebe um aviso claro. Os CEPs devem ter **8
dígitos** — aceitamos com ou sem hífen (`"03175-001"` ou `"03175001"`).

### 10.5 Missão 2 — Transformar CEP em coordenada 📍

```r
gs_cep_para_coordenadas("03175-001")
```

```
        cep  latitude longitude fonte                        precisao
1 03175-001 -23.55334 -46.58032 local  coordenada mediana do índice local ...
```

Como funciona por dentro (a **cascata**):

1. **Índice local** (`gs_indice_cep()`): varre os `data/*.csv` e guarda todas
   as ocorrências CEP → coordenada (hoje: **7.148 CEPs** e **11.131
   registros**). Se um CEP tem vários endereços, a coordenada "representante"
   é a **mediana** deles.
2. **viaCEP + Nominatim**: se o CEP não está no índice, o sistema consulta a
   internet (com pausa de 1 segundo entre chamadas, respeitando a política do
   OSM). O Nominatim tenta primeiro pelo **código postal**; se não achar (muito
   comum no Brasil, onde o OSM raramente tem `postal_code` por rua), o sistema
   usa o endereço do **viaCEP** e pede a **rua** (`street + city + state`); se
   ainda assim não achar, cai para o **centróide da cidade**. A coluna `fonte`
   mostra qual caminho venceu (`local` ou `nominatim`) e a coluna `precisao`
   indica o nível: código postal, rua ou cidade.

Você também pode construir/atualizar o índice manualmente:

```r
indice <- gs_indice_cep(force = TRUE)   # reconstrói do zero
nrow(indice)                            # 11131
refs <- gs_cep_referencia()             # coordenada mediana por CEP
```

### 10.6 Missão 3 — A coordenada bate com o CEP? ✅

```r
gs_verificar_cep("03175-001", -23.553640, -46.580180)
```

```
$cep                    : chr "03175-001"
$latitude_cep           : num -23.6
$longitude_cep          : num -46.6
$distancia_m            : num 0
$confere                : logi TRUE
$veredito               : chr "CONFERE"
$tolerancia_m           : num 300
$n_ocorrencias          : int 2
$equipamento_referencia : chr "AMA/UBS ÁGUA RASA - DR. MARCOS ANDRADE CORSATO"
$camada_referencia      : chr "equipamento_saude_ubs_posto_centro"
```

O veredito compara a coordenada informada com a de referência do CEP: se a
distância for menor que a **tolerância** (padrão **300 m**, configurável),
diz `CONFERE`; senão, `NAO CONFERE`. É o nosso "detector de mentiras" do
mapa. 🕵️

### 10.7 Missão 4 — O que tem por perto? 📡

```r
proximos <- gs_servicos_proximos(
  cep     = "03175-001",
  raio_m  = 2000,
  camadas = c("equipamento_saude_ubs_posto_centro", "equipamento_educacao_ceu")
)
head(proximos[, c("camada", "nome", "distancia_m")])
```

```
  camada                                   nome            distancia_m
1 equipamento_saude_ubs_posto_centro       AMA/UBS ÁGUA RASA...   36.6
2 equipamento_saude_ubs_posto_centro       UBS ...                 1801
```

Dica: se você **não** informar `camadas`, ele usa **todas** as camadas locais:

```r
gs_servicos_proximos(cep = "03175-001", raio_m = 1000)
```

```
  camada                                nome                      distancia_m
1 equipamento_assistencia_social        CENTRO SOCIAL COMUNIT...     243.
2 equipamento_educacao_infantil_rede_publica  CR P CONV JOSE PE      239.
...
```

Outros botões úteis:

```r
# Usar coordenadas em vez de CEP
gs_servicos_proximos(coordenadas = c(-23.55334, -46.58032), raio_m = 1500)

# Limitar a N resultados por camada
gs_servicos_proximos(cep = "03175-001", raio_m = 5000, n_por_camada = 3)

# Trocar a métrica de distância
gs_servicos_proximos(cep = "03175-001", raio_m = 2000, tipo_distancia = "manhattan")
```

> 🎯 **Em `camadas` você pode passar:**
> - o **tema** inteiro — `"saude"` expande para todas as camadas de saúde
>   (UBS, ambulatórios, saúde mental, urgência/emergência etc.);
> - o **nome completo** da camada — `"equipamento_saude_ubs_posto_centro"`;
> - um **pedaço do nome** — `"ubs"` casa com todas as camadas que contêm "ubs";
> - um **`data.frame`** vindo de `gs_catalogo_equipamentos()` (usa a coluna
>   `camada`).
> Para ver todas as opções locais agrupadas por tema, use `gs_listar_servicos()`
> (ou `gs_listar_servicos("saude")` para filtrar). Se um valor não bater com
> nada, o sistema avisa e sugere a listagem.

### 10.8 Missão 5 — Os tipos de distância 📏

Nem toda distância é igual. A função `gs_tipos_distancia()` é o "manual de
instruções" embutido:

```r
gs_tipos_distancia()
```

| Tipo | O que mede | Quando usar |
|------|-----------|-------------|
| `geodesica` (padrão) | Elipsoidal via `sf::st_distance` em CRS geográfico | Referência, mais precisa |
| `euclidiana` | Metros na projeção UTM/SIRGAS2000 (EPSG:31983) | Rápida, boa até ~20 km |
| `haversine` | Aproximação esférica sobre WGS84 | Leve, sem transformar CRS |
| `manhattan` | \|Δx\| + \|Δy\| em metros projetados | "Caminhabilidade" em quadrículas |
| `rede_viaria` | Rota real de carro via OSRM | Requer `osrm` (opcional) |

**Na prática:** as três primeiras são variações de "linha reta" — a
**geodésica** é a mais fiel à curvatura da Terra; a **euclidiana** é a mais
rápida em metros; a **haversine** é uma aproximação esférica leve. A
**manhattan** soma os deslocamentos leste-oeste e norte-sul (imagine andar
por quarteirões retos, como numa grade). A **rede viária** usa o grafo real
de ruas: é a mais próxima do tempo real de caminhada/viagem, mas depende do
pacote `osrm` e de um servidor com cobertura.

### 10.9 Missão 6 — Desenhar os mapas 🗺️

Um comando, dois mundos:

```r
# Mapa interativo (HTML) — abre no navegador, dá zoom, mostra popups
gs_mapa_servicos(proximos, interativo = TRUE,  salvar = "mapas/cep_03175001.html")

# Mapa estático (PNG) — para colar em relatório
gs_mapa_servicos(proximos, interativo = FALSE, salvar = "mapas/cep_03175001.png")
```

O mapa mostra: o **ponto de interesse** (marcador vermelho), o **círculo do
raio** (contorno azul tracejado) e os **serviços** coloridos por tipo. No mapa
interativo, cada serviço mostra um **tooltip ao passar o mouse** (nome, tipo e
distância) e um **popup ao clicar** com nome, endereço, bairro, distância e
camada; há também escala, seletor de camadas e três basemaps (OSM, CartoDB e
satélite). O mapa estático sai com **legenda no rodapé em várias colunas** (o
número de colunas se ajusta à quantidade de tipos, e rótulos longos são
quebrados para caber) e **resolução configurável** (`largura`/`altura`/`dpi`;
largura padrão 12 polegadas a 300 dpi, com a altura ajustada automaticamente
para a legenda não ser cortada). Se você passar `cep`/`camadas` direto, ele
calcula tudo sozinho:

```r
gs_mapa_servicos(cep = "03175-001",
                 camadas = "equipamento_saude_ubs_posto_centro",
                 raio_m = 1500, interativo = FALSE,
                 salvar = "mapas/ubs_agua_rasa.png")
```

### 10.10 Missão 7 — Análises estatísticas e espaciais 📊

Uma função, várias análises — escolha com o argumento `tipo` (aceita vários):

```r
analises <- gs_analise_servicos(
  proximos,
  tipo = c("descritivas", "vizinho_mais_proximo",
           "voronoi", "kde", "raios_progressivos", "moran", "rede_viaria")
)
```

| Tipo | O que devolve | Dependência |
|------|---------------|-------------|
| `descritivas` | Contagens por tipo/camada, resumo, histograma e boxplot anotados (mediana/média) | nenhuma |
| `vizinho_mais_proximo` | Distância ao serviço mais próximo (geral e por camada) | nenhuma |
| `acessibilidade_media` | Resumo robusto das distâncias (mediana, P25/P75, IQR, CV) + curva ECDF | nenhuma |
| `raio_otimo` | Raio que alcança 50%, 75%, 90% e 95% dos serviços + gráfico ECDF | nenhuma |
| `cobertura_buffer` | Área coberta por buffers (por camada e geral) vs casco convexo | nenhuma |
| `nni` | Índice de Vizinho Mais Próximo (padrão agrupado/aleatório/disperso) | nenhuma |
| `voronoi` | Polígonos de Thiessen: áreas de influência de cada serviço (sf) | nenhuma |
| `kde` | Mapa de densidade de kernel (concentração) | nenhuma |
| `kde_banda` | KDE com largura de banda estimada (Silverman) | nenhuma |
| `raios_progressivos` | Oportunidades acumuladas por raio (tabela + curva) | nenhuma |
| `getis_ord` | Getis-Ord G* local em grade hexagonal (pontos quentes/frios) | `spdep` (opcional) |
| `lisa` | Moran local (LISA) em grade hexagonal (alto-alto/baixo-baixo) | `spdep` (opcional) |
| `ripley_k` | Função K de Ripley (agregação em múltiplas escalas) | `spatstat` (opcional) |
| `moran` | Moran's I sobre contagens em grade hexagonal (padrão) | `spdep` (opcional) |
| `moran_distrital` | Moran's I e LISA agregados por distrito | `spdep` (opcional) |
| `por_distrito` | Contagem e densidade de serviços por distrito (mapa) | internet (1ª vez) |
| `cobertura_populacional` | População atendida no raio (via camada de população ou densidade) | opcional |
| `rede_viaria` | Distância de percurso (OSRM) comparada à linha reta | `osrm` (opcional) |

Exemplos de saídas:

```r
analises$descritivas$resumo_distancia
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    36.6   477.7   918.8   918.8  1359.9  1801.0

analises$raios_progressivos$contagem        # tabela de oportunidades
#   raio_m n_servicos
# 1    500          1
# 2   1000          1
# 3   2000          2

analises$raios_progressivos$grafico         # curva de oportunidades acumuladas
analises$acessibilidade_media$grafico_ecdf  # curva acumulada das distâncias
```

Se um pacote opcional (`spdep` ou `osrm`) não estiver instalado, a análise
**não quebra**: devolve uma mensagem orientando a instalação.

**Mas o que cada análise significa, no dia a dia?**

- `descritivas`: responde *"quantos serviços tem e como são as distâncias?"*
  — histograma e boxplot trazem linhas da mediana e da média.
- `vizinho_mais_proximo`: responde *"qual é o serviço mais perto?"*
- `acessibilidade_media`: resume as distâncias com medidas **robustas**
  (mediana, P25/P75, IQR e coeficiente de variação) — preferíveis à média pura
  em distribuições assimétricas — e mostra a curva ECDF ("X% dos serviços a até
  Y m").
- `raio_otimo`: diz o raio que alcança 50%, 75%, 90% dos serviços, com o
  gráfico ECDF que permite **ver** o percentil correspondente a cada raio.
- `cobertura_buffer`: estima qual fração da área estudada fica "coberta" por
  buffers ao redor dos serviços.
- `nni` (Índice de Vizinho Mais Próximo): responde *"os serviços se agrupam?"*
  comparando a distância média real ao vizinho mais próximo com a esperada
  num padrão aleatório (R < 1 = agrupado; R > 1 = disperso). O resultado traz
  um aviso sobre efeito de borda.
- `voronoi`: divide a área em "pedaços de influência" — cada pedaço mostra a
  região atendida "primeiro" por aquele serviço.
- `kde` / `kde_banda`: fazem um "mapa de calor" — onde os serviços se amontoam.
- `raios_progressivos`: mostra quantos serviços você encontra conforme caminha
  500 m, 1 km, 2 km... — agora também com a curva de oportunidades acumuladas.
- `getis_ord`: aponta **onde** estão os aglomerados (pontos quentes/frios) em
  células hexagonais (aviso: p-valores locais sem correção para múltiplos
  testes — trate como exploratório).
- `lisa`: identifica células alto-alto (muitos serviços vizinhos com muitos
  serviços) e baixo-baixo (o oposto); mesmo aviso de múltiplos testes.
- `ripley_k`: pergunta, em várias escalas, *"os serviços se agrupam mais do que
  o acaso?"* — útil para escolher o raio de análise.
- `moran`: pergunta *"os serviços estão mais agrupados do que o acaso?"*. A
  versão padrão aplica Moran's I às **contagens por célula hexagonal** (a
  aplicação estatisticamente correta para pontos); a versão sobre a distância
  radial (argumento `sobre_grade = FALSE`) fica como diagnóstico, com ressalva.
- `moran_distrital`: o mesmo diagnóstico, mas agregado por distrito — mais
  estável e interpretável para políticas públicas.
- `por_distrito`: conta e mapeia os serviços por distrito da cidade.
- `cobertura_populacional`: estima a população dentro do raio de busca.
- `rede_viaria`: compara o "voo do pássaro" (linha reta) com o caminho real de
  carro/pé.

### 10.11 Tabela-resumo das funções do módulo 🧰

| Função | O que faz | Sai |
|--------|-----------|-----|
| `gs_indice_cep()` | Monta o índice local CEP → coordenadas | data.frame |
| `gs_listar_servicos()` | Lista os serviços locais por tema (o que usar em `camadas`) | data.frame |
| `gs_ler_cep(cep)` | Valida o CEP e devolve o endereço | data.frame |
| `gs_cep_para_coordenadas(cep)` | Lat/long do CEP (índice local → Nominatim) | data.frame |
| `gs_verificar_cep(cep, lat, lon)` | Confere se a coordenada bate com o CEP | lista |
| `gs_servicos_proximos(...)` | Serviços dentro do raio (ou N por camada) | data.frame |
| `gs_tipos_distancia()` | Manual das métricas de distância | data.frame |
| `gs_mapa_servicos(...)` | Mapa estático (PNG/PDF) ou interativo (HTML) | plot/HTML |
| `gs_analise_servicos(...)` | Análises estatísticas/espaciais | lista |
| `gs_relatorio_analises(...)` | Relatório consolidado (HTML/MD) com as análises | arquivo |
| `gs_exportar_resultado(...)` | Exporta tabelas e polígonos em CSV/GeoJSON | caminhos |

### 10.12 Relatório e exportação 📦

Um comando gera um **relatório consolidado** com as análises escolhidas:
tabelas, gráficos e mapas num único arquivo HTML auto-contido (as figuras são
embutidas em base64 — não depende de pandoc/rmarkdown) ou em Markdown. Cada
seção traz **tabela + gráfico + um parágrafo de interpretação automática**
(`gs_interpretar_analise()`) com a leitura dos principais resultados
(mediana, percentis, R do NNI, Moran, cobertura etc.) — para as análises
saírem explicadas, não só calculadas.

```r
gs_relatorio_analises(
  cep = "03175-001",
  tipo = c("descritivas", "acessibilidade_media", "raio_otimo", "nni",
           "voronoi", "getis_ord", "por_distrito"),
  arquivo = "relatorios/relatorio.html"
)
```

E para compartilhar os dados (não só as figuras):

```r
analises <- gs_analise_servicos(cep = "03175-001", tipo = "descritivas")
gs_exportar_resultado(proximos, analises, dir = "saidas")
```

### 10.13 Limitações conhecidas ⚠️

- O índice local cobre apenas os CEPs dos equipamentos públicos (≈7 mil) —
  CEPs fora dele precisam de internet (viaCEP + Nominatim).
- Um CEP de faixa de rua pode ter várias coordenadas; a verificação usa a
  ocorrência mais próxima.
- CEP de caixa postal não tem coordenada útil.
- O servidor demo do OSRM tem cobertura e limites próprios; configure outro
  com `options(gs.osrm_server = "http://...")` ou `options(osrm.server = ...)`.
  O perfil de rota também é configurável (`options(gs.osrm_profile = "driving")`).
- O código OSRM é compatível com o pacote `osrm` ≥ 4.0 (API de entrada com
  `lon`/`lat` e distâncias em metros) e com versões antigas (coluna `id` e
  distâncias em km).
- `cobertura_populacional` precisa de uma camada de população (ex.: setores
  censitários do IBGE) ou de uma densidade média estimada.
- Análises locais (`lisa`, `getis_ord`, `moran`) dependem de vizinhança e
  número de pontos; resultados com poucos pontos devem ser lidos com cautela.
- `moran` (sobre grade hexagonal) depende do tamanho da célula
  (`celula_m`); resultados podem variar com a escolha da grade. A versão
  sobre a distância radial (`sobre_grade = FALSE`) fica como diagnóstico.

---

## 11. Ideias de próximos passos 🌟

- 🧭 Cruzar os equipamentos com camadas de distritos/subprefeituras para saber
  **quais regiões têm e quais não têm** determinado serviço.
- 🗺️ Gerar mapas temáticos com `ggplot2` ou `tmap` a partir dos GeoJSON.
- 🧮 Calcular distâncias de cada casa ao serviço mais próximo.
- 🔁 Automatizar o download com agendamento (ex.: `cron`) para manter a base atualizada.

**Boa garimpagem!** 🗺️✨