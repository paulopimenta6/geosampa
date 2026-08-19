# Tech stack

- **Linguagem:** R 4.6.x (testado na 4.6.1).
- **Base R** + dplyr/sf/tidyr estilo tidyverse de código (data.frames simples).
- **Pacotes obrigatórios:** httr, jsonlite, sf, readr, xml2.
- **Mapas:** ggplot2, leaflet, htmlwidgets (opcionais no carregamento).
- **Análises espaciais:** spdep (moran/lisa/getis), osrm (rede viária), spatstat (Ripley's K) — todos opcionais/sob demanda.
- **Relatório:** htmltools + base64enc (HTML auto-contido), sem rmarkdown/pandoc.
- **Testes:** testthat (test_dir em tests/).
- **Dados:** GeoJSON (EPSG:31983) + CSV (lat/lon EPSG:4326) em data/.
- **APIs externas:** WFS/WMS do GeoSampa, viaCEP, Nominatim/OSM, GeoNetwork, OSRM demo.
- **Sem gerenciador de dependências** (sem DESCRIPTION); instalação manual via install.packages.