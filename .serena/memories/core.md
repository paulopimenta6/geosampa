# GeoSampa — Mapa do projeto

Wrapper em R sobre os serviços web do GeoSampa (Prefeitura de São Paulo). Baixa equipamentos públicos (WFS → GeoJSON + CSV) e oferece módulo de CEP + serviços próximos + mapas + análises estatísticas/espaciais.

## Estrutura
- `R/00_config.R` — URLs (WFS/WMS/metadados/viaCEP/nominatim), EPSGs (oficial=31983, geográfica=4674, wgs84=4326), tolerâncias e helpers de raiz/pastas. `gs_raiz()` acha a raiz subindo até achar `R/` + `scripts/`.
- `R/gs_camadas.R` — catálogo de camadas (GetCapabilities), temas (`equipamento_*`), `gs_listar_servicos()`.
- `R/gs_baixar.R` — download paginado (WFS) → GeoJSON + CSV (lat/lon em 4326). `gs_baixar_todos_equipamentos()`, `gs_baixar_servicos(tema)`.
- `R/gs_metadados.R` — consulta ao catálogo GeoNetwork.
- `R/gs_indice_cep.R` — índice local CEP→coordenadas a partir dos CSVs; colunas `n_ocorrencias` e `representante` (1 por CEP, mediana). `gs_cep_referencia()` = mediana por CEP.
- `R/gs_cep.R` — ler (viaCEP), geocodificar (cascata: local → nominatim postalcode → rua via viaCEP → cidade), verificar (veredito CONFERE / NAO CONFERE / SEM DADO SUFICIENTE).
- `R/gs_proximidade.R` — `gs_servicos_proximos()` com 5 tipos de distância (geodesica, euclidiana, haversine, manhattan, rede_viaria/OSRM). Resultado SEMPRE ordenado por `distancia_m` (global). Atributos: `ponto`, `tipo_distancia`, `raio_m`.
- `R/gs_mapa.R` — mapa estático (ggplot2) e interativo (leaflet).
- `R/gs_analise.R` — 18 tipos de análise: descritivas, vizinho_mais_proximo, acessibilidade_media, cobertura_buffer, raio_otimo, nni, voronoi, kde, kde_banda, raios_progressivos, getis_ord, lisa, ripley_k, moran, moran_distrital, por_distrito, cobertura_populacional, rede_viaria. Novas análises usam grade hexagonal (`gs_grade_hex`) e distritos (`gs_baixar_distritos`, baixa `distrito_municipal` sob demanda; sempre `st_make_valid`).
- `R/gs_relatorio.R` — `gs_relatorio_analises()` (HTML auto-contido via base64 ou Markdown) e `gs_exportar_resultado()` (CSV/GeoJSON).
- `scripts/` — `carregar_funcoes.R` (carrega tudo em silêncio) e `baixar_tudo.R` (terminal).
- `tests/` — testthat (30 testes).

## Invariantes
- `data/` é gitignored (regenerável com scripts/baixar_tudo.R). `saidas/` e `relatorios/` também.
- Fontes de dados externas ao índice local precisam de internet (viaCEP, Nominatim com pausa de 1s, OSRM demo, WFS).
- Pacotes opcionais (spdep, osrm, spatstat, htmltools/base64enc, testthat) são usados sob demanda com `requireNamespace` e mensagem orientativa se faltarem.
- EPSG 31983 é o CRS oficial; tudo é convertido p/ 4326 para lat/lon.