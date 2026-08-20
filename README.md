# GeoSampa em R

Scripts R para consultar serviços públicos do GeoSampa, baixar camadas WFS,
geocodificar CEPs, localizar equipamentos próximos e executar análises
estatísticas e espaciais reproduzíveis.

O projeto trabalha com:

- dados vetoriais do GeoSampa em EPSG:31983;
- CSVs locais com latitude e longitude em EPSG:4326;
- ViaCEP e Nominatim para CEPs não cobertos pelos dados locais;
- OSRM para distâncias por rede viária;
- relatórios Markdown/HTML, tabelas CSV, geometrias GeoJSON e plots PNG;
- uma ou várias origens na mesma execução.

O código continua sendo um projeto de scripts carregados com `source()`, não um
pacote R.

## Início rápido

### 1. Restaurar o ambiente

O arquivo `renv.lock` fixa o ambiente usado no desenvolvimento. Na primeira
execução:

```bash
Rscript scripts/configurar_ambiente.R
```

Reinicie a sessão R depois da restauração.

Dependências de sistema normalmente exigidas no Linux incluem GDAL, GEOS,
PROJ, UDUNITS e libxml2. O CI em `.github/workflows/testes-r.yml` documenta a
instalação automatizada das dependências R.

### 2. Carregar as funções

```r
source("scripts/carregar_funcoes.R")
```

O carregador encontra a raiz sem alterar `getwd()`.

### 3. Baixar dados

```r
gs_catalogo_equipamentos()
gs_baixar_servicos("saude")
gs_baixar_camada("equipamento_cultura_bibliotecas")
```

Pelo terminal:

```bash
Rscript scripts/baixar_tudo.R saude
Rscript scripts/baixar_tudo.R --camada equipamento_saude_ubs_posto_centro
```

Os downloads usam paginação ordenada, conferência de contagem e IDs, CRS
explícito e promoção transacional dos pares CSV/GeoJSON. Leitores e aquisições
usam bloqueios por diretório/camada; arquivos incompletos ou versões mistas não
são consumidos pelo projeto.

Após a aquisição, os GeoJSON/CSV em `data/` são tratados como entradas
imutáveis e com o esquema fornecido pela fonte. As análises não acrescentam
colunas nem reescrevem esses arquivos; metadados derivados são gravados somente
em `saidas/`.

## Uma origem

```r
proximos <- gs_servicos_proximos(
  cep = "03175-001",
  camadas = "saude",
  raio_m = 3000
)

analises <- gs_analise_servicos(
  proximos,
  tipo = c(
    "descritivas", "acessibilidade_media", "raio_otimo",
    "cobertura_buffer", "nni", "moran", "getis_ord", "lisa"
  )
)

gs_salvar_analises(
  proximos,
  analises,
  dir = "saidas/agua_rasa",
  formato_relatorio = "ambos"
)
```

Também é possível usar coordenadas:

```r
proximos <- gs_servicos_proximos(
  coordenadas = c(-23.55364, -46.58018),
  raio_m = 2000
)
```

Informe exatamente um entre `cep` e `coordenadas`.

## Vários CEPs ou coordenadas

`gs_analisar_locais()` preserva o fluxo escalar e processa cada origem
sequencialmente. Erros em uma origem não interrompem as demais por padrão.

```r
lote <- gs_analisar_locais(
  cep = c(casa = "05508-090", trabalho = "03175-001"),
  coordenadas = data.frame(
    id = c("ponto_a", "ponto_b"),
    latitude = c(-23.55364, -23.57000),
    longitude = c(-46.58018, -46.65000)
  ),
  camadas = "saude",
  raio_m = 3000,
  nome_execucao = "comparacao_saude",
  formato_relatorio = "ambos"
)

print(lote)
lote$origens
lote$comparacao
```

Linha de comando:

```bash
Rscript scripts/analisar_lote.R \
  --ceps 05508090,05586001,05596090 \
  --camadas saude \
  --raio 3000 \
  --nome comparacao_ceps

Rscript scripts/analisar_lote.R \
  --coords "-23.55,-46.63;-23.57,-46.65" \
  --raio 2500
```

## Artefatos gerados

Uma execução em lote cria uma estrutura semelhante a:

```text
saidas/comparacao_saude/
├── origens.csv
├── servicos.csv
├── metricas.csv
├── comparacao_origens.csv
├── manifesto.csv
├── resultado_lote.rds
├── relatorio_lote.md
├── figuras/
│   ├── comparacao_contagens.png
│   └── comparacao_distancias.png
└── origens/
    └── cep_05508090/
        ├── servicos_proximos.csv
        ├── metadados_consulta.csv
        ├── amostragem_por_camada.csv
        ├── metricas.csv
        ├── interpretacoes.csv
        ├── manifesto.csv
        ├── relatorio_numerico.md
        ├── relatorio_analises.md
        ├── relatorio_analises.html
        ├── tabelas/
        ├── geometrias/
        └── figuras/
```

Todos os objetos `ggplot` encontrados nas análises são persistidos em PNG.
Objetos `sf` são salvos como GeoJSON e também recebem uma visualização PNG.
Métricas escalares são normalizadas em `metricas.csv`.
`metadados_consulta.csv` registra métrica/backend, raio e eventual truncamento;
`amostragem_por_camada.csv` registra quantos itens estavam disponíveis, foram
retidos ou omitidos em cada camada.
Em `servicos_proximos.csv`, `distancia_m` é arredondada para exibição e
`distancia_m_exata` preserva o valor usado nos filtros e cálculos.

## Análises disponíveis

| Tipo | Resultado principal |
|---|---|
| `descritivas` | contagens, média, mediana, MAD, IQR, percentis, IC descritivo e plots |
| `vizinho_mais_proximo` | serviço mais próximo geral e por camada |
| `acessibilidade_media` | medidas robustas por camada/tipo e ECDF |
| `raio_otimo` | raios empíricos P50, P75, P90 e P95 |
| `raios_progressivos` | oportunidades acumuladas por distância |
| `cobertura_buffer` | cobertura dos buffers na janela observacional da consulta |
| `nni` | padrão pontual com simulação Monte Carlo no domínio da consulta |
| `voronoi` | células de influência corretamente associadas aos pontos |
| `kde` / `kde_banda` | densidade em projeção métrica EPSG:31983 |
| `moran` | Moran global da densidade, com referência condicional à área observada |
| `getis_ord` | Getis-Ord G* com Monte Carlo por área e ajuste Benjamini-Hochberg |
| `lisa` | quatro quadrantes LISA com Monte Carlo por área e ajuste BH |
| `ripley_k` | curva transformada L(r)-r na janela da métrica, sem envelope inferencial |
| `por_distrito` | contagem e densidade nas partes distritais observadas |
| `moran_distrital` | Moran global e LISA da densidade distrital observada |
| `cobertura_populacional` | população estimada por densidade ou camada `sf` |
| `rede_viaria` | distância OSRM e razão rede/reta |

### Interpretação estatística

- Os serviços encontrados formam um conjunto administrativo dentro do raio,
  não uma amostra aleatória da população.
- Intervalos da média são descritivos e dependem de hipótese i.i.d.; não medem
  incerteza sobre um cadastro completo.
- Moran, LISA e Getis-Ord usam densidades e uma referência Monte Carlo
  condicional à área efetivamente observada de cada célula/distrito.
- P-valores locais de LISA e Getis-Ord são ajustados por
  Benjamini-Hochberg, mas continuam exploratórios.
- Análises que exigem janela observacional são bloqueadas quando
  `n_por_camada` realmente omitiu ocorrências ou quando a seleção usa
  `rede_viaria` sem uma isócrona.
- NNI e Ripley assumem CSR homogênea; misturar camadas heterogêneas e escolher
  uma única resolução de grade pode produzir resultados sensíveis à composição
  e ao problema da unidade espacial modificável (MAUP).
- Ripley é diagnóstico: sem envelope de simulação, a curva não fornece teste
  formal em cada distância.
- Comparações entre CEPs são descritivas e não representam efeito causal.

## CEP e precisão

A resolução de CEP segue a cascata:

1. índice local construído dos CSVs;
2. código postal no Nominatim;
3. rua obtida pelo ViaCEP;
4. município como último recurso.

Referências aproximadas de rua ou cidade não produzem um veredito binário em
`gs_verificar_cep()`: o resultado é `SEM DADO SUFICIENTE` e a distância é
apenas informativa.

## Testes

Suíte offline:

```bash
Rscript --vanilla tests/testthat.R
```

Integrações reais, executadas separadamente:

```bash
GEOSAMPA_RUN_NETWORK_TESTS=true \
Rscript --vanilla -e 'testthat::test_dir("tests/testthat", filter="integracao-rede")'
```

Os testes reais consultam GeoSampa WFS, GeoNetwork, ViaCEP, Nominatim e OSRM.
Eles não fazem parte da execução padrão para evitar instabilidade externa e uso
desnecessário dos serviços públicos.

## Estrutura

```text
R/                         funções de consulta, análise, lote e relatórios
scripts/                   carregamento, download, ambiente e CLI de lote
tests/testthat/            testes offline e integrações opt-in
data/                      downloads locais, ignorados pelo Git
saidas/                    resultados gerados, ignorados pelo Git
renv.lock                  versões reproduzíveis das dependências
DOCUMENTACAO.md            explicação detalhada e conceitos
```

## Fontes

- [GeoSampa](https://geosampa.prefeitura.sp.gov.br)
- [WFS GeoSampa](https://wfs.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wfs)
- [Catálogo de metadados](https://metadados.geosampa.prefeitura.sp.gov.br)
- [ViaCEP](https://viacep.com.br)
- [Nominatim / OpenStreetMap](https://nominatim.openstreetmap.org)
- [OSRM](https://project-osrm.org)

Dados oficiais: Prefeitura de São Paulo / GeoSampa. Dados complementares de
geocodificação e roteamento mantêm suas respectivas atribuições e políticas de
uso.
