# GeoSampa em R: seu radar de serviços públicos

Este projeto ajuda você a descobrir **quais serviços públicos existem perto de
um lugar da cidade de São Paulo**. Você pode informar um CEP ou coordenadas,
desenhar mapas e gerar relatórios.

Pense nele como uma pequena expedição:

1. Você escolhe um ponto de partida: um CEP ou uma coordenada.
2. O projeto consulta os dados de equipamentos públicos da Prefeitura.
3. Você recebe uma lista, mapas, tabelas e análises sobre o que encontrou.

Não é preciso conhecer geoprocessamento para seguir o primeiro exemplo. O
projeto é formado por scripts R: ele **não** é instalado como um pacote do R.

> 🧭 Quer entender cada recurso com calma? Leia o
> [manual completo](DOCUMENTACAO.md) depois de concluir o primeiro exemplo.

## O que você pode fazer

- Procurar UBS, escolas, feiras, museus e outros equipamentos perto de um local.
- Baixar camadas públicas do GeoSampa em tabela (`CSV`) e mapa (`GeoJSON`).
- Usar um CEP, latitude e longitude como ponto de partida.
- Medir distância em linha reta, por uma grade ou por rota de carro.
- Criar mapas em imagem (`PNG`) ou interativos (`HTML`).
- Produzir relatórios, gráficos, tabelas e análises espaciais.
- Comparar vários CEPs ou coordenadas em uma única execução.

## Antes de começar

Você precisará de:

- **R 4.6.1**, a versão registrada pelo projeto.
- Internet para restaurar o ambiente e baixar dados pela primeira vez.
- No Linux, bibliotecas usadas pelo pacote espacial `sf`: GDAL, GEOS, PROJ,
  UDUNITS e libxml2. A forma de instalá-las depende da sua distribuição.
- Espaço em disco para os dados que você decidir baixar.

O projeto usa `renv`, uma caixa que guarda as versões corretas dos pacotes R.
Assim, você não precisa instalar pacote por pacote nem adivinhar versões.

## Início rápido

Abra um terminal na pasta principal do projeto, a mesma onde está este arquivo.

### 1. Preparar a caixa de ferramentas

Execute uma única vez:

```bash
Rscript scripts/configurar_ambiente.R
```

Esse comando instala ou restaura os pacotes registrados em `renv.lock`. Quando
ele terminar, feche e abra novamente o R ou o RStudio.

### 2. Carregar as funções

No **console do R**, execute:

```r
source("scripts/carregar_funcoes.R")
```

Se nada parecer acontecer, está tudo bem: as ferramentas foram carregadas.

### 3. Baixar uma camada pequena e conhecida

Uma clonagem nova não traz a pasta `data/` preenchida. Baixe primeiro os dados
de UBS, que serão usados no exemplo:

```r
gs_baixar_camada("equipamento_saude_ubs_posto_centro")
```

Ao terminar, veja os arquivos criados:

```r
list.files("data", pattern = "ubs_posto_centro")
```

Você encontrará um arquivo `GeoJSON`, para programas de mapas, e um `CSV`, que
abre em planilhas e é usado pela busca de proximidade.

### 4. Ligar o radar

Agora procure UBS em até 2 km de uma coordenada de São Paulo. A ordem é sempre
**latitude primeiro e longitude depois**:

```r
proximos <- gs_servicos_proximos(
  coordenadas = c(-23.55364, -46.58018),
  camadas = "equipamento_saude_ubs_posto_centro",
  raio_m = 2000
)

proximos[, c("nome", "bairro", "distancia_m")]
```

Cada linha é um serviço encontrado. `distancia_m` mostra a distância em metros.
Os dados podem mudar quando a Prefeitura atualiza seus cadastros, então não se
espante se a quantidade de resultados for diferente em outra data.

### 5. Criar um mapa e um pequeno relatório

```r
gs_mapa_servicos(
  proximos,
  interativo = FALSE,
  salvar = "mapas/primeiro_mapa.png"
)

analises <- gs_analise_servicos(
  proximos,
  tipo = c("descritivas", "vizinho_mais_proximo")
)

gs_salvar_analises(
  proximos,
  analises,
  dir = "saidas/primeiro_exemplo",
  formato_relatorio = "md"
)
```

Abra `mapas/primeiro_mapa.png` e `saidas/primeiro_exemplo/relatorio_analises.md`.
Na próxima vez, escolha outro nome de pasta ou adicione `sobrescrever = TRUE`
à chamada de `gs_salvar_analises()`.

## Prefere usar um CEP?

Depois de baixar ao menos uma camada, use um CEP no lugar das coordenadas:

```r
proximos <- gs_servicos_proximos(
  cep = "03175-001",
  camadas = "saude",
  raio_m = 3000
)
```

O projeto tenta localizar o CEP primeiro nos dados baixados. Se ele não estiver
ali, consulta ViaCEP e Nominatim pela internet. Um CEP pode representar uma rua
inteira, portanto a coordenada encontrada é uma referência, não uma garantia de
um endereço exato.

## Escolhendo o que procurar

Use `gs_listar_servicos()` para ver as camadas locais que já podem participar da
busca:

```r
gs_listar_servicos()
gs_listar_servicos("saude")
```

No argumento `camadas`, você pode informar:

```r
"saude"                                     # um tema
"equipamento_saude_ubs_posto_centro"        # nome completo
"ubs"                                       # parte do nome
```

Antes de procurar, baixe a camada desejada. Por exemplo:

```r
gs_baixar_servicos("cultura")
gs_baixar_camada("equipamento_feira_livre")
```

Nem toda camada do GeoSampa representa pontos. A busca de serviços próximos usa
somente arquivos CSV com colunas de latitude e longitude; áreas, limites e
polígonos não entram nesse radar.

## Caminhos possíveis

| Se você quer... | Comece por... |
|---|---|
| Ver as camadas disponíveis na Prefeitura | `gs_catalogo_equipamentos()` |
| Baixar dados de saúde, cultura ou educação | `gs_baixar_servicos("saude")` |
| Procurar perto de uma coordenada | `gs_servicos_proximos(coordenadas = c(lat, lon))` |
| Procurar perto de um CEP | `gs_servicos_proximos(cep = "00000-000")` |
| Medir outros tipos de distância | `gs_tipos_distancia()` |
| Produzir um mapa | `gs_mapa_servicos()` |
| Gerar análises | `gs_analise_servicos()` |
| Salvar tudo | `gs_salvar_analises()` |
| Comparar vários locais | `gs_analisar_locais()` |

## Vários locais de uma vez

Você pode comparar CEPs e coordenadas na mesma expedição:

```r
lote <- gs_analisar_locais(
  cep = c(casa = "05508-090", trabalho = "03175-001"),
  coordenadas = data.frame(
    id = "ponto_manual",
    latitude = -23.55364,
    longitude = -46.58018
  ),
  camadas = "saude",
  raio_m = 3000,
  nome_execucao = "comparacao_de_locais",
  formato_relatorio = "md"
)

print(lote)
```

Também existe um comando para o terminal:

```bash
Rscript scripts/analisar_lote.R \
  --ceps 05508090,03175001 \
  --camadas saude \
  --raio 3000 \
  --nome comparacao_de_ceps
```

As opções do comando de lote são detalhadas no [manual completo](DOCUMENTACAO.md).

## Onde as coisas ficam

| Pasta | Conteúdo |
|---|---|
| `R/` | Funções do projeto. |
| `scripts/` | Comandos para preparar o ambiente, baixar dados e analisar lotes. |
| `data/` | Dados baixados do GeoSampa. Não acompanha uma clonagem nova e é ignorada pelo Git. |
| `mapas/` | Mapas salvos manualmente. |
| `saidas/` | Relatórios, tabelas, gráficos e geometrias gerados pelas análises. É ignorada pelo Git. |
| `tests/` | Testes automatizados do projeto. |

O carregador encontra a raiz do projeto, mas não troca sua pasta de trabalho
atual. Para que `mapas/` e `saidas/` sejam criadas onde você espera, trabalhe a
partir da pasta principal do repositório ou use caminhos completos.

## Problemas comuns

| Mensagem ou situação | O que fazer |
|---|---|
| `Nenhum CSV` ou nenhuma camada encontrada | Baixe uma camada com `gs_baixar_camada()` e confira `gs_listar_servicos()`. |
| O CEP não foi localizado | Confira os oito dígitos; se ele não estiver no índice local, conecte-se à internet. |
| Latitude e longitude parecem trocadas | Use `c(latitude, longitude)`, por exemplo `c(-23.55, -46.63)`. |
| A pasta de saída já existe | Use outro nome ou passe `sobrescrever = TRUE`. |
| Mapa ou análise pede pacote ausente | Rode novamente `Rscript scripts/configurar_ambiente.R`, reinicie o R e tente de novo. |
| A rota por ruas falhou | A distância `rede_viaria` depende do serviço externo OSRM; tente uma distância em linha reta ou verifique a internet. |

## Testes

Depois de restaurar o ambiente, execute os testes locais a partir da raiz:

```bash
Rscript tests/testthat.R
```

Não use `--vanilla` nesse comando: ele impede a ativação automática do `renv`.
Os testes que consultam serviços externos são opcionais:

```bash
GEOSAMPA_RUN_NETWORK_TESTS=true \
Rscript -e 'testthat::test_dir("tests/testthat", filter = "integracao-rede", stop_on_failure = TRUE)'
```

Esses testes usam GeoSampa, GeoNetwork, ViaCEP, Nominatim e OSRM; podem falhar
por indisponibilidade temporária desses serviços.

## Fontes dos dados

- [GeoSampa](https://geosampa.prefeitura.sp.gov.br)
- [WFS GeoSampa](https://wfs.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wfs)
- [Catálogo de metadados](https://metadados.geosampa.prefeitura.sp.gov.br)
- [ViaCEP](https://viacep.com.br)
- [Nominatim / OpenStreetMap](https://nominatim.openstreetmap.org)
- [OSRM](https://project-osrm.org)

Os dados oficiais são da Prefeitura de São Paulo / GeoSampa. Consulte os
metadados de cada camada antes de publicar ou tomar decisões importantes com os
resultados.
