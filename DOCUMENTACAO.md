# 🗺️ Manual da Expedição GeoSampa

Bem-vindo! Este é um guia para usar o projeto mesmo que você nunca tenha aberto
o R, ouvido falar em mapa digital ou trabalhado com dados públicos.

Imagine São Paulo como uma cidade feita de várias folhas transparentes. Em uma
folha estão as UBS; em outra, as escolas; em outra, feiras, museus e outros
serviços. O GeoSampa é o grande armário que guarda essas folhas. Este projeto é
o seu **kit de exploração**: ele busca as folhas, aponta o que existe perto de
um lugar e transforma a descoberta em mapa e relatório.

> 🎯 **Missão do projeto:** responder perguntas como “quais serviços públicos
> existem perto deste CEP?” de modo reproduzível e usando dados oficiais da
> Prefeitura de São Paulo.

## 🧭 Como usar este manual

Não é preciso ler tudo de uma vez. Escolha o caminho que mais combina com sua
missão:

| Quero... | Vá para... |
|---|---|
| Fazer meu primeiro mapa | [Missão 1](#missao-1-preparar-a-mochila) e [Missão 4](#missao-4-ligar-o-radar) |
| Baixar dados públicos | [Missão 2](#missao-2-abrir-o-bau-de-dados) |
| Pesquisar pelo CEP da minha casa | [Missão 3](#missao-3-encontrar-o-ponto-de-partida) |
| Entender distâncias | [Missão 5](#missao-5-escolher-a-regua) |
| Criar um mapa | [Missão 6](#missao-6-desenhar-a-descoberta) |
| Criar relatórios e análises | [Missões 7 e 8](#missao-7-fazer-perguntas-aos-dados) |
| Comparar vários lugares | [Missão 9](#missao-9-explorar-varios-locais) |
| Resolver um problema | [Socorro](#socorro-erros-comuns) |

## 🌆 O mapa mental da cidade digital

### O que é GeoSampa?

O **GeoSampa** é o portal de dados geográficos da Prefeitura de São Paulo. Ele
guarda informações sobre a cidade: saúde, educação, cultura, meio ambiente,
limites de distrito e muito mais.

Cada conjunto de informações é uma **camada**. Pense em uma camada como uma
folha transparente colocada sobre um mapa:

- 🏥 `equipamento_saude_ubs_posto_centro`: postos e UBS.
- 🏫 Camadas de educação: escolas, CEUs e outros equipamentos.
- 🍅 `equipamento_feira_livre`: feiras.
- 🎭 Camadas de cultura: bibliotecas, museus e espaços culturais.

Uma camada pode conter **pontos**, como uma UBS, ou **áreas**, como um distrito.

O radar de serviços próximos usa somente as camadas de pontos que têm latitude
e longitude no arquivo CSV.

### O que são CEP, coordenada e raio?

- **CEP** é o código usado para localizar um endereço. Pode representar uma
  rua inteira, uma instituição ou uma faixa de endereços; não é sempre um ponto
  exato no mapa.
- **Coordenadas** são dois números que indicam uma posição: latitude e
  longitude. São Paulo fica aproximadamente em `-23.55, -46.63`.
- **Raio** é o tamanho do círculo de busca. `raio_m = 2000` significa procurar
  até 2.000 metros do ponto de partida.

Uma coordenada deve ser escrita sempre nesta ordem:

```r
c(latitude, longitude)
# Exemplo em São Paulo
c(-23.55364, -46.58018)
```

### Como os dados chegam até você?

O projeto usa a porta pública WFS do GeoSampa para buscar dados vetoriais. Ela
entrega o conteúdo da camada, não apenas uma imagem do mapa.

```text
GeoSampa (WFS) -> data/ (GeoJSON + CSV) -> busca de proximidade
-> mapa, análises e relatórios -> saidas/
```

O `GeoJSON` é especialmente útil em programas de mapa. O `CSV` é uma tabela que
pode ser aberta numa planilha; nas camadas de pontos, ele também é o arquivo
usado pela busca de proximidade.

<a name="missao-1-preparar-a-mochila"></a>
## Missão 1: preparar a mochila

### O que você precisa

1. R 4.6.1.
2. Internet na primeira preparação e quando for consultar serviços externos.
3. No Linux, bibliotecas para dados espaciais: GDAL, GEOS, PROJ, UDUNITS e
   libxml2. Consulte a documentação da sua distribuição Linux para instalá-las.
4. A pasta do projeto baixada ou clonada em seu computador.

O projeto usa `renv`: ele é como uma mochila que contém as versões corretas dos
pacotes R. Prefira sempre esse caminho ao instalar pacotes manualmente.

### Preparação inicial

Abra um terminal dentro da pasta principal do projeto e execute:

```bash
Rscript scripts/configurar_ambiente.R
```

O comando lê `renv.lock`, instala `renv` se necessário e restaura os pacotes.

Quando terminar, reinicie o R ou o RStudio.

No console do R, carregue o kit:

```r
source("scripts/carregar_funcoes.R")
```

O carregador confirma a presença dos pacotes básicos e disponibiliza as funções
da pasta `R/`. O projeto não muda a pasta de trabalho atual do R. Por isso,
comece na raiz do repositório ou use caminhos completos ao salvar arquivos.

### Terminal e console do R não são o mesmo lugar

- Use o **terminal** para comandos que começam com `Rscript`.
- Use o **console do R/RStudio** para linhas que começam com funções, como
  `gs_baixar_camada()`.

| Lugar | Exemplo |
|---|---|
| Terminal | `Rscript scripts/configurar_ambiente.R` |
| Console do R | `source("scripts/carregar_funcoes.R")` |

<a name="missao-2-abrir-o-bau-de-dados"></a>
## Missão 2: abrir o baú de dados

Em uma clonagem nova, a pasta `data/` não vem preenchida. Os dados baixados são
grandes e atualizáveis, por isso não são enviados pelo Git junto com o código.

### Ver o catálogo

```r
gs_catalogo_equipamentos()
```

Essa função pede ao GeoSampa a lista de camadas de equipamentos e mostra nomes,
temas e descrições. Para procurar por uma palavra:

```r
gs_buscar_camadas("saude")
gs_buscar_camadas("feira")
```

### Baixar uma camada

Vamos baixar UBS como primeira folha do mapa:

```r
gs_baixar_camada("equipamento_saude_ubs_posto_centro")
```

Depois, confira os tesouros que chegaram:

```r
list.files("data", pattern = "ubs_posto_centro")
```

O download cria, normalmente:

```text
data/
├── equipamento_saude_ubs_posto_centro.geojson
└── equipamento_saude_ubs_posto_centro.csv
```

O projeto baixa páginas de dados, confere a integridade e só promove os arquivos
quando o conjunto está pronto. Evite interromper a internet durante o processo.

Se restar um diretório `.gs-lock`, uma execução anterior pode ter sido
interrompida: confira os arquivos antes de apagar qualquer coisa.

### Baixar por assunto ou tudo de uma vez

```r
# Todas as camadas que combinam com o tema
gs_baixar_servicos("saude")
# Uma camada específica
gs_baixar_camada("equipamento_feira_livre")
# Todo o catálogo de equipamentos: pode demorar e ocupar espaço
gs_baixar_todos_equipamentos()
```

No terminal, você também pode baixar:

```bash
Rscript scripts/baixar_tudo.R saude
Rscript scripts/baixar_tudo.R --camada equipamento_saude_ubs_posto_centro
```

### Usar filtros com cuidado

`filtro` aceita uma expressão CQL enviada ao GeoSampa. É uma ferramenta útil,
mas avançada: os nomes das colunas mudam entre camadas. Consulte o esquema da
camada antes de filtrar e teste um download pequeno.

```r
gs_baixar_camada(
  "equipamento_bombeiros",
  filtro = "cd_identificador = 150001"
)
```

Arquivos obtidos com filtro recebem um sufixo no nome para não confundir esse
recorte com a camada completa.

### Ler os dados

```r
# Tabela: boa para planilhas e para olhar colunas
ubs_tabela <- readr::read_csv("data/equipamento_saude_ubs_posto_centro.csv")
# Mapa: bom para programas GIS e operações espaciais
ubs_mapa <- sf::st_read("data/equipamento_saude_ubs_posto_centro.geojson")
```

Os CSVs de pontos recebem `latitude` e `longitude` em graus. Arquivos de áreas,
como distritos, podem ter `geometria_wkt` em vez dessas colunas e não participam
da busca de serviços próximos.

### Ver apenas o que o radar pode usar

```r
gs_listar_servicos()
gs_listar_servicos("saude")
```

Essa é a lista mais segura para preencher o argumento `camadas`, pois considera
o que já foi baixado localmente e possui coordenadas aproveitáveis.

<a name="missao-3-encontrar-o-ponto-de-partida"></a>
## Missão 3: encontrar o ponto de partida

### Ler um CEP

```r
gs_ler_cep("03175-001")
```

O projeto remove traços e espaços, mas recomenda-se usar oito dígitos, com ou
sem hífen. Essa função consulta o ViaCEP pela internet e devolve informações do
endereço quando o CEP existe.

> 💡 ViaCEP é um serviço independente de consulta de CEPs; não é uma API dos
> Correios.

### Transformar CEP em coordenada

```r
gs_cep_para_coordenadas("03175-001")
```

O explorador tenta encontrar uma referência nesta ordem:

1. 🗃️ Índice local montado a partir dos CSVs já baixados.
2. 🌐 Busca no Nominatim / OpenStreetMap pelo código postal ou endereço.
3. 🌐 Busca pela cidade no Nominatim, quando a rua não for localizada.

Quando um CEP está no índice local, a consulta pode funcionar sem internet. Fora
dele, é preciso que ViaCEP e Nominatim estejam disponíveis. Observe as colunas
`fonte` e `precisao`: uma referência de rua ou cidade não deve ser tratada como
localização exata.

`fonte` e `precisao`: uma referência de rua ou cidade não deve ser tratada como
localização exata.

Para reconstruir o índice depois de baixar novas camadas:

```r
indice <- gs_indice_cep(force = TRUE)
referencias <- gs_cep_referencia(indice)
```

### Conferir uma coordenada contra um CEP

```r
gs_verificar_cep(
  cep = "03175-001",
  latitude = -23.55364,
  longitude = -46.58018,
  tolerancia_m = 300
)
```

O resultado traz a distância até a referência disponível e um veredito. Quando
a fonte é vaga, como uma rua ou cidade, o retorno correto é `SEM DADO
SUFICIENTE`, e não uma confirmação artificial.

<a name="missao-4-ligar-o-radar"></a>
## Missão 4: ligar o radar

Agora vamos perguntar: “o que existe perto deste ponto?”

### Busca por coordenadas

```r
proximos <- gs_servicos_proximos(
  coordenadas = c(-23.55364, -46.58018),
  camadas = "equipamento_saude_ubs_posto_centro",
  raio_m = 2000
)
proximos[, c("nome", "tipo_servico", "bairro", "distancia_m")]
```

### Busca por CEP

```r
proximos <- gs_servicos_proximos(
  cep = "03175-001",
  camadas = "saude",
  raio_m = 3000
)
```

Informe **exatamente um** entre `cep` e `coordenadas`. Usar os dois ao mesmo
tempo, ou nenhum, deixa o explorador sem ponto de partida.

### Escolher camadas

`camadas` aceita várias formas de indicar o que você procura:

```r
"saude"                                     # tema
"equipamento_saude_ubs_posto_centro"        # nome exato
"ubs"                                       # trecho do nome
c("saude", "equipamento_feira_livre")     # mais de uma escolha
```

Se `camadas` for omitido, o projeto tenta usar todas as camadas locais válidas.

Comece com uma camada ou tema pequeno: será mais fácil entender a tabela e o
mapa gerados.

### Limitar resultados por camada

```r
proximos <- gs_servicos_proximos(
  cep = "03175-001",
  camadas = "saude",
  raio_m = 5000,
  n_por_camada = 3
)
```

Isso guarda, no máximo, os três mais próximos de cada camada. É ótimo para uma
lista curta, mas significa que você não está olhando todos os serviços do raio.

Algumas análises são bloqueadas para evitar conclusões enganosas quando houve
omissão de ocorrências.

### O que vem na tabela?

| Coluna | Significado |
|---|---|
| `nome` | Nome do equipamento, quando disponível. |
| `tipo_servico` | Categoria declarada pela camada. |
| `endereco` e `bairro` | Informações de localização, quando disponíveis. |
| `camada` | A folha do GeoSampa de onde veio o registro. |
| `distancia_m` | Distância arredondada para leitura. |
| `distancia_m_exata` | Valor usado nos filtros e cálculos. |

<a name="missao-5-escolher-a-regua"></a>
## Missão 5: escolher a régua

Distância pode significar coisas diferentes. Chame `gs_tipos_distancia()` para
ver a cola rápida construída pelo próprio projeto.

```r
gs_tipos_distancia()
```

| Tipo | Ideia simples | Quando usar |
|---|---|---|
| `geodesica` | Linha reta sobre a superfície da Terra. É o padrão. | Comparações reproduzíveis em geral. |
| `euclidiana` | Linha reta medida em metros na projeção de São Paulo. | Cálculos rápidos em distâncias locais. |
| `haversine` | Outra aproximação de linha reta usando latitude/longitude. | Cenários leves sem conversão de projeção. |
| `manhattan` | Soma os deslocamentos norte-sul e leste-oeste. | Simular uma grade geométrica; não é rota real. |
| `rede_viaria` | Percurso de carro calculado pelo OSRM. | Quando a rota por ruas é mais importante que a linha reta. |

Exemplo:

```r
proximos <- gs_servicos_proximos(
  coordenadas = c(-23.55364, -46.58018),
  camadas = "saude",
  raio_m = 3000,
  tipo_distancia = "euclidiana"
)
```

`rede_viaria` exige o pacote `osrm` e um servidor OSRM disponível. O perfil
padrão é `driving` (carro), não caminhada. A seleção por rede não cria uma
isócrona; por isso análises que dependem de uma área circular completa não são
executadas para esse resultado.

<a name="missao-6-desenhar-a-descoberta"></a>
## Missão 6: desenhar a descoberta

Depois de obter `proximos`, você pode criar dois tipos de mapa.

### Mapa estático: uma imagem para relatório

```r
gs_mapa_servicos(
  proximos,
  interativo = FALSE,
  salvar = "mapas/ubs_perto_de_mim.png"
)
```

O PNG mostra o ponto de partida, o círculo do raio e os serviços coloridos por
tipo. Você pode controlar `largura`, `altura` e `dpi` quando precisar de uma
imagem maior.

### Mapa interativo: para explorar no navegador

```r
gs_mapa_servicos(
  proximos,
  interativo = TRUE,
  salvar = "mapas/ubs_perto_de_mim.html"
)
```

Abra o arquivo HTML no navegador. Você poderá aproximar, afastar e clicar nos
pontos para ver detalhes. Os mapas de fundo vêm de serviços externos e precisam
de internet ao serem visualizados.

Também é possível pedir busca e mapa de uma vez:

```r
gs_mapa_servicos(
  cep = "03175-001",
  camadas = "saude",
  raio_m = 1500,
  interativo = FALSE,
  salvar = "mapas/saude_no_cep.png"
)
```

<a name="missao-7-fazer-perguntas-aos-dados"></a>
## Missão 7: fazer perguntas aos dados

`gs_analise_servicos()` recebe o resultado do radar e devolve uma lista com as
respostas. Comece pelas análises simples:

```r
analises <- gs_analise_servicos(
  proximos,
  tipo = c("descritivas", "vizinho_mais_proximo", "raio_otimo")
)
analises$descritivas
analises$vizinho_mais_proximo
```

> 🔎 Uma análise ser produzida não torna automaticamente sua conclusão certa.
> Leia os limites metodológicos antes de usar resultados para decisões públicas,
> comparação de bairros ou divulgação científica.

### Nível 1: perguntas do dia a dia

| Tipo | Pergunta que ajuda a responder |
|---|---|
| `descritivas` | Quantos serviços foram encontrados e como as distâncias se distribuem? |
| `vizinho_mais_proximo` | Qual é o serviço mais perto, no total e por camada? |
| `acessibilidade_media` | Como resumir as distâncias sem depender só da média? |
| `raio_otimo` | Qual raio alcança 50%, 75%, 90% ou 95% dos serviços observados? |
| `raios_progressivos` | Quantos serviços aparecem a cada distância crescente? |
| `cobertura_buffer` | Que parte da área de busca fica coberta por buffers dos serviços? |

`raios_progressivos` usa, por padrão, 500 m, 1.000 m e 2.000 m. Escolha um
raio de consulta de ao menos 2.000 m se quiser essa análise.

### Nível 2: enxergar a distribuição no mapa

| Tipo | Pergunta que ajuda a responder |
|---|---|
| `nni` | Os pontos parecem agrupados, aleatórios ou dispersos? |
| `voronoi` | Qual região está mais perto de cada serviço? |
| `kde` | Onde há maior concentração de pontos? |
| `kde_banda` | Como fica a densidade com largura de banda estimada? |
| `por_distrito` | Quantos serviços e qual densidade aparecem em cada parte distrital observada? |
| `cobertura_populacional` | Qual população estimada está dentro da área de busca? |

`cobertura_populacional` precisa de `densidade_km2` ou de uma camada `sf` de
polígonos com uma coluna chamada `populacao`. Sem isso, a pergunta não tem dados
suficientes para ser respondida.

### Nível 3: investigação espacial avançada

| Tipo | O que investiga | Necessidade adicional |
|---|---|---|
| `moran` | Associação espacial da densidade na grade. | `spdep` |
| `getis_ord` | Áreas quentes e frias na grade. | `spdep` |
| `lisa` | Agrupamentos locais alto-alto, baixo-baixo e mistos. | `spdep` |
| `moran_distrital` | Associação espacial da densidade por distrito. | `spdep` |
| `ripley_k` | Agrupamento em diferentes escalas de distância. | `spatstat.geom` e `spatstat.explore` |
| `rede_viaria` | Diferença entre rota de carro e linha reta. | `osrm` e servidor OSRM |

Essas análises podem retornar uma mensagem em vez de um número quando faltam
pontos, pacote, dados ou condições adequadas. Isso é uma proteção, não um erro
silencioso.

### Dependências opcionais

O `renv` restaura as dependências registradas pelo projeto. Se uma análise ainda
avisar que um pacote opcional está ausente, instale somente o pacote indicado e
registre-o no ambiente conforme a política do seu projeto. As análises básicas,
todavia, também dependem do ambiente espacial e gráfico restaurado pelo `renv`.

<a name="missao-8-guardar-as-descobertas"></a>
## Missão 8: guardar as descobertas

O comando abaixo salva a expedição em uma pasta organizada:

```r
gs_salvar_analises(
  proximos,
  analises,
  dir = "saidas/minha_primeira_expedicao",
  formato_relatorio = "ambos"
)
```

`formato_relatorio` aceita `"md"`, `"html"`, `"ambos"` ou `"nenhum"`.

```text
saidas/minha_primeira_expedicao/
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

| Arquivo | Para que serve |
|---|---|
| `servicos_proximos.csv` | Lista de equipamentos usados na análise. |
| `metadados_consulta.csv` | Raio, métrica de distância e outras escolhas da consulta. |
| `amostragem_por_camada.csv` | Quantos itens existiam, foram mantidos ou omitidos. |
| `metricas.csv` | Resultados numéricos normalizados. |
| `interpretacoes.csv` | Leituras automáticas para ajudar na primeira interpretação. |
| `manifesto.csv` | Inventário de todos os arquivos criados. |
| `tabelas/`, `geometrias/`, `figuras/` | Resultados específicos de cada análise. |

Por segurança, uma pasta não vazia não é sobrescrita por padrão. Para trocar
arquivos existentes conscientemente:

```r
gs_salvar_analises(
  proximos,
  analises,
  dir = "saidas/minha_primeira_expedicao",
  sobrescrever = TRUE
)
```

Você também pode usar funções separadas quando precisar de mais controle:

```r
gs_exportar_resultado(proximos, analises, dir = "saidas/exportacao")
gs_relatorio_numerico(proximos, analises, arquivo = "saidas/numerico.md")
gs_relatorio_analises(
  resultado = proximos,
  analises = analises,
  arquivo = "saidas/analises.html"
)
```

<a name="missao-9-explorar-varios-locais"></a>
## Missão 9: explorar vários locais

Uma expedição em lote compara várias origens. Você pode misturar CEPs e
coordenadas:

```r
lote <- gs_analisar_locais(
  cep = c(casa = "05508-090", trabalho = "03175-001"),
  coordenadas = data.frame(
    id = c("parque", "escritorio"),
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

Cada origem ganha sua própria pasta e a pasta principal reúne as comparações:

```text
saidas/comparacao_saude/
├── origens.csv
├── servicos.csv
├── metricas.csv
├── comparacao_origens.csv
├── resultado_lote.rds
├── relatorio_lote.md
├── figuras/
└── origens/
```

O status de uma origem pode ser `ok`, `parcial`, `sem_servicos` ou `erro`.

`parcial` quer dizer que a busca terminou, mas alguma análise não pôde rodar.

Por padrão, um problema em uma origem não impede as outras de serem processadas.

### Lote pelo terminal

```bash
Rscript scripts/analisar_lote.R \
  --ceps 05508090,03175001 \
  --coords "-23.55364,-46.58018" \
  --camadas saude \
  --raio 3000 \
  --nome comparacao_saude \
  --formato md
```

Opções disponíveis:

| Opção | Uso |
|---|---|
| `--ceps` | CEPs separados por vírgula. |
| `--coords` | Pares `latitude,longitude` separados por ponto e vírgula. |
| `--camadas` | Camadas ou temas separados por vírgula. |
| `--ids` | Identificadores separados por vírgula, um para cada origem. |
| `--raio` | Raio em metros. |
| `--saida` | Pasta que receberá a execução. |
| `--nome` | Nome da pasta da execução. |
| `--formato` | `md`, `html`, `ambos` ou `nenhum`. |

A interface de terminal não expõe todas as opções do R, como escolha detalhada
de análises, `n_por_camada`, distância por rede ou `sobrescrever`. Use
`gs_analisar_locais()` no R para esses casos.

## 🧰 Catálogo de ferramentas

| Ferramenta | O que ela faz |
|---|---|
| `gs_camadas_disponiveis()` | Lê o catálogo remoto completo do WFS. |
| `gs_camadas_equipamentos()` | Separa camadas cujo nome começa por `equipamento_`. |
| `gs_catalogo_equipamentos()` | Mostra camadas de equipamento organizadas por tema. |
| `gs_buscar_camadas(termo)` | Procura uma palavra no catálogo remoto. |
| `gs_baixar_camada(camada)` | Baixa uma camada. |
| `gs_baixar_camadas(camadas)` | Baixa várias camadas. |
| `gs_baixar_servicos(tema)` | Baixa as camadas que combinam com um tema. |
| `gs_baixar_todos_equipamentos()` | Baixa todos os equipamentos. |
| `gs_metadados(termo)` | Procura documentos sobre os dados no catálogo GeoNetwork. |
| `gs_metadado_registro(uuid)` | Lê um registro completo de metadados. |
| `gs_listar_servicos()` | Lista camadas locais de pontos prontas para o radar. |
| `gs_ler_cep(cep)` | Consulta e valida um CEP. |
| `gs_indice_cep()` | Cria o índice local CEP -> coordenadas. |
| `gs_cep_referencia()` | Produz uma referência representativa por CEP. |
| `gs_cep_para_coordenadas(cep)` | Localiza a referência de um CEP. |
| `gs_verificar_cep(cep, latitude, longitude)` | Compara CEP e coordenada. |
| `gs_tipos_distancia()` | Explica as réguas disponíveis. |
| `gs_servicos_proximos(...)` | Encontra equipamentos perto de uma origem. |
| `gs_mapa_servicos(...)` | Cria mapa estático ou interativo. |
| `gs_analise_servicos(...)` | Executa uma ou várias análises. |
| `gs_salvar_analises(...)` | Salva resultados, figuras e relatórios juntos. |
| `gs_exportar_resultado(...)` | Exporta artefatos de forma direta. |
| `gs_relatorio_numerico(...)` | Gera um relatório de métricas. |
| `gs_relatorio_analises(...)` | Gera relatório analítico em Markdown ou HTML. |
| `gs_analisar_locais(...)` | Executa a expedição para vários locais. |

## 🌐 O que funciona sem internet?

| Tarefa | Precisa de internet? |
|---|---|
| Carregar funções depois de restaurar o ambiente | Não. |
| Procurar serviços em CSVs já baixados, usando coordenadas e linha reta | Não. |
| Localizar um CEP que esteja no índice local | Não. |
| Listar ou baixar catálogo/camadas | Sim, GeoSampa WFS. |
| Consultar metadados | Sim, GeoNetwork. |
| Ler um CEP com `gs_ler_cep()` | Sim, ViaCEP. |
| Localizar CEP fora do índice | Sim, ViaCEP e Nominatim. |
| Calcular rota por rede | Sim, OSRM. |
| Ver fundos de um mapa HTML | Sim, provedores de mapas. |

Serviços públicos externos podem ficar lentos, mudar ou ficar indisponíveis.

O projeto usa tentativas e espera progressiva, mas não há garantia de resposta.

Respeite as políticas de uso dos provedores: evite downloads repetidos e não
execute grandes lotes desnecessariamente.

## ⚙️ Ajustes para quem precisa

Quase todas as missões funcionam sem configuração extra. Se sua conexão for
lenta, se você tiver seu próprio servidor de rotas ou se executar o projeto fora
da pasta principal, estes ajustes podem ajudar. Execute-os no console do R antes
da operação desejada:

```r
# Esperar até 180 segundos por cada pedido remoto e tentar quatro vezes
options(gs.http_timeout_s = 180, gs.http_tentativas = 4)
# Informar onde está a raiz se ela não for localizada automaticamente
options(gs.raiz = "/caminho/para/geosampa")
# Usar outro servidor e perfil do OSRM, se você tiver acesso a eles
options(gs.osrm_server = "https://meu-servidor-osrm.exemplo/")
options(gs.osrm_profile = "driving")
```

Essas opções valem somente enquanto a sessão R estiver aberta. Não é necessário
alterá-las para o uso comum. O servidor público do OSRM possui limites próprios;
use um servidor controlado por você apenas quando tiver permissão para isso.

## ⚠️ Limites importantes da expedição

### Dados não são uma amostra aleatória

Os equipamentos encontrados são registros administrativos dentro do raio de
busca. Eles não representam uma amostra aleatória da cidade. Diferenças entre
dois CEPs são descritivas e não provam causa e efeito.

### O radar enxerga um círculo, não o mundo inteiro

O círculo da consulta não é automaticamente recortado pelo limite municipal ou
pela cobertura de cada cadastro. Perto das bordas, a interpretação exige cuidado.

### Poucos pontos limitam análises

Análises como Moran, LISA, Getis-Ord, NNI, Voronoi, KDE e Ripley precisam de
quantidade e distribuição adequadas de pontos. Uma mensagem de “não executada”
é preferível a uma conclusão fraca.

### Truncar muda a pergunta

Se `n_por_camada` retirou ocorrências, você passou a analisar apenas os mais
próximos, não o conjunto inteiro. Resultados que dependem da cobertura completa
são bloqueados; não tente compará-los como se nada tivesse sido omitido.

### Resultados espaciais são exploratórios

Moran, LISA e Getis-Ord dependem de escolhas como tamanho de célula, vizinhança
e área observada. Os p-valores e ajustes existentes ajudam na leitura, mas não
eliminam sensibilidade à grade, a limites administrativos e à composição das
camadas. Use resultados como investigação, não como prova final isolada.

### CEP não é endereço preciso

Um CEP pode cobrir uma rua inteira, e as bases local, ViaCEP e OpenStreetMap
podem divergir. Antes de usar uma coordenada em situação crítica, confira a
coluna de precisão e valide a fonte apropriada.

<a name="socorro-erros-comuns"></a>
## 🆘 Socorro: erros comuns

| Situação | Causa provável | Caminho de saída |
|---|---|---|
| `Nenhum CSV` | Nenhuma camada de pontos foi baixada. | Use `gs_baixar_camada()` e depois `gs_listar_servicos()`. |
| Camada não encontrada | Nome ou tema não combina com o catálogo local. | Veja `gs_listar_servicos()` ou `gs_catalogo_equipamentos()`. |
| CEP inválido | O valor não tem oito dígitos ou não existe. | Revise o CEP; tente com ou sem hífen. |
| CEP sem coordenada local | O índice é construído apenas com os CSVs já baixados. | Baixe mais dados ou conecte-se à internet. |
| Coordenada inválida | Latitude e longitude foram trocadas ou estão fora do intervalo. | Use `c(latitude, longitude)`. |
| Pacote ausente | O ambiente não foi restaurado ou falta dependência opcional. | Rode `Rscript scripts/configurar_ambiente.R`, reinicie o R e leia a mensagem. |
| Saída já existe | A função protege arquivos anteriores. | Escolha outro nome ou use `sobrescrever = TRUE` conscientemente. |
| Análise não executada | Faltam pontos, pacote, área adequada ou a amostra foi truncada. | Leia a mensagem da análise; amplie o raio ou escolha uma análise básica. |
| Rota por rede falhou | OSRM ou a rede está indisponível. | Tente novamente mais tarde ou use `geodesica`/`euclidiana`. |
| Mapa HTML sem fundo | Os mapas de base são carregados da internet. | Abra com conexão ativa. |

## 🧪 Testar o kit

Depois de preparar o ambiente, na raiz do projeto, execute:

```bash
Rscript tests/testthat.R
```

Não acrescente `--vanilla`: ele ignora `.Rprofile` e pode impedir a ativação do
`renv`. Parte da suíte depende de dados locais; numa clonagem sem `data/`, alguns
testes são pulados.

Os testes de integração real são opcionais e consultam serviços externos:

```bash
GEOSAMPA_RUN_NETWORK_TESTS=true \
Rscript -e 'testthat::test_dir("tests/testthat", filter = "integracao-rede", stop_on_failure = TRUE)'
```

## 📚 Glossário de bolso

| Palavra | Tradução simples |
|---|---|
| Camada | Uma folha de informações sobre a cidade. |
| CSV | Tabela de texto que abre em planilhas. |
| GeoJSON | Arquivo que guarda dados e geometria para mapas. |
| Geocodificação | Transformar endereço ou CEP em coordenadas. |
| Latitude/longitude | Os dois números que localizam um ponto no planeta. |
| Metadado | A ficha de identidade do dado: origem, data, descrição e responsável. |
| Raio/buffer | Círculo em volta de um ponto, medido em metros. |
| WFS | Porta pela qual o GeoSampa entrega dados de mapa. |
| EPSG:31983 | Sistema oficial de coordenadas do GeoSampa, útil para medir metros em São Paulo. |
| EPSG:4326 | Latitude e longitude em graus, formato comum em mapas e planilhas. |
| OSRM | Serviço externo que calcula rotas pela rede viária. |
| Isócrona | Área alcançável por rota em determinado custo; não é criada automaticamente neste projeto. |

## 📖 Fontes e cuidado com os dados

- [Portal GeoSampa](https://geosampa.prefeitura.sp.gov.br)
- [WFS GeoSampa](https://wfs.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wfs)
- [Catálogo de metadados](https://metadados.geosampa.prefeitura.sp.gov.br)
- [ViaCEP](https://viacep.com.br)
- [Nominatim / OpenStreetMap](https://nominatim.openstreetmap.org)
- [OSRM](https://project-osrm.org)

Antes de publicar um mapa ou análise, leia o metadado da camada utilizada,
cite a fonte e confira a data de atualização. Boa expedição! 🧭
