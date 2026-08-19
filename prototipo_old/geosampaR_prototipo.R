## =====================================================================
## geosampaR — protótipo de wrapper em R para os webservices do GeoSampa
## =====================================================================
##
## O GeoSampa (https://geosampa.prefeitura.sp.gov.br) já expõe seus dados
## por meio de webservices geoespaciais padrão OGC, hospedados em um
## GeoServer da Prefeitura de São Paulo:
##
##   - WFS (Web Feature Service) -> dados vetoriais (feições + atributos)
##       http://wfs.geosampa.prefeitura.sp.gov.br/geoserver/ows
##       (mesmo backend do endpoint https listado no tutorial:
##        https://wfs.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wfs)
##
##   - WMS (Web Map Service) -> imagens/mapas renderizados
##       https://wms.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wms
##       https://raster.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wms
##
## Ou seja: NÃO é necessário criar uma API nova "do zero" — o GeoSampa já
## é, na prática, uma API geoespacial aberta (padrão OGC WFS/WMS, sem
## necessidade de chave de acesso). O que este script faz é construir uma
## camada de conveniência ("wrapper"/SDK) em R por cima desse WFS, para
## tornar o consumo dos dados tão simples quanto uma função:
##
##      gs_get_layer("geoportal:distrito_municipal")
##
## em vez de montar URLs manualmente.
##
## Dependências: sf, httr2, xml2, dplyr, ggplot2 (opcional p/ o exemplo)
## =====================================================================

# install.packages(c("sf", "httr2", "xml2", "dplyr", "ggplot2"))
library(sf)
library(httr2)
library(xml2)
library(dplyr)

## ---------------------------------------------------------------------
## 1) Configuração básica
## ---------------------------------------------------------------------

# Endpoint WFS "ows" do GeoSampa (aceita GetCapabilities / GetFeature)
GEOSAMPA_WFS <- "http://wfs.geosampa.prefeitura.sp.gov.br/geoserver/ows"

# Endpoints WMS (imagem), conforme documentado no tutorial oficial
GEOSAMPA_WMS_CAMADAS <- "https://wms.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wms"
GEOSAMPA_WMS_BASE    <- "https://raster.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wms"

# CRS oficial (SIRGAS2000 / UTM 23S), conforme tabela do tutorial
GEOSAMPA_EPSG_SIRGAS2000_UTM <- 31983
GEOSAMPA_EPSG_SIRGAS2000_GEO <- 4674   # geográfico (lat/long)
GEOSAMPA_EPSG_WGS84          <- 4326   # útil p/ Leaflet/Google Maps


## ---------------------------------------------------------------------
## 2) gs_list_layers() — descobre as camadas disponíveis (GetCapabilities)
## ---------------------------------------------------------------------

#' Lista as camadas (FeatureTypes) disponíveis no WFS do GeoSampa
#'
#' @param filtro string opcional (regex) para filtrar pelo título/nome
#'   da camada, ex: "arvore", "escola", "zoneamento"
#' @return data.frame com colunas: name (typeName p/ usar no GetFeature),
#'   title, abstract, bbox_minx/miny/maxx/maxy (em lat/long)
gs_list_layers <- function(filtro = NULL) {

  resp <- request(GEOSAMPA_WFS) |>
    req_url_query(
      service = "WFS",
      version = "1.0.0",
      request = "GetCapabilities"
    ) |>
    req_perform()

  xml <- resp_body_xml(resp)
  ns <- xml_ns(xml)

  feature_types <- xml_find_all(xml, ".//d1:FeatureType", ns)

  df <- data.frame(
    name     = xml_text(xml_find_first(feature_types, "d1:Name", ns)),
    title    = xml_text(xml_find_first(feature_types, "d1:Title", ns)),
    abstract = xml_text(xml_find_first(feature_types, "d1:Abstract", ns)),
    stringsAsFactors = FALSE
  )

  bbox <- xml_find_first(feature_types, "d1:LatLongBoundingBox", ns)
  df$bbox_minx <- as.numeric(xml_attr(bbox, "minx"))
  df$bbox_miny <- as.numeric(xml_attr(bbox, "miny"))
  df$bbox_maxx <- as.numeric(xml_attr(bbox, "maxx"))
  df$bbox_maxy <- as.numeric(xml_attr(bbox, "maxy"))

  if (!is.null(filtro)) {
    idx <- grepl(filtro, df$name, ignore.case = TRUE) |
      grepl(filtro, df$title, ignore.case = TRUE)
    df <- df[idx, ]
  }

  tibble::as_tibble(df)
}


## ---------------------------------------------------------------------
## 3) gs_get_layer() — baixa feições (GetFeature) como objeto sf
## ---------------------------------------------------------------------

#' Baixa uma camada do GeoSampa via WFS GetFeature e retorna um objeto sf
#'
#' @param type_name nome da camada, ex: "geoportal:distrito_municipal"
#'   (ver gs_list_layers())
#' @param bbox vetor numérico c(xmin, ymin, xmax, ymax) para recortar
#'   espacialmente a consulta (opcional, mesmo CRS de srs_name)
#' @param cql_filter string com filtro CQL do GeoServer, ex:
#'   "ds_nome = 'SE'"  (opcional — permite filtrar por atributo no servidor,
#'   sem precisar baixar a camada inteira)
#' @param srs_name código EPSG de saída (default SIRGAS2000/UTM 23S,
#'   o oficial do Município, conforme o tutorial)
#' @param max_features limite de feições retornadas (evita downloads
#'   gigantes sem querer; use Inf para trazer tudo)
#' @return objeto sf (simple features) pronto para usar com sf/ggplot2/leaflet
gs_get_layer <- function(type_name,
                          bbox = NULL,
                          cql_filter = NULL,
                          srs_name = paste0("EPSG:", GEOSAMPA_EPSG_SIRGAS2000_UTM),
                          max_features = 5000) {

  req <- request(GEOSAMPA_WFS) |>
    req_url_query(
      service      = "WFS",
      version      = "1.0.0",
      request      = "GetFeature",
      typeName     = type_name,
      outputFormat = "json",
      srsName      = srs_name
    )

  if (!is.null(bbox)) {
    bbox_str <- paste(c(bbox, srs_name), collapse = ",")
    req <- req |> req_url_query(bbox = bbox_str)
  }

  if (!is.null(cql_filter)) {
    req <- req |> req_url_query(CQL_FILTER = cql_filter)
  }

  if (is.finite(max_features)) {
    req <- req |> req_url_query(maxFeatures = max_features)
  }

  resp <- req_perform(req)

  # GeoServer retorna GeoJSON -> sf lê diretamente do corpo da resposta
  tmp <- tempfile(fileext = ".geojson")
  writeBin(resp_body_raw(resp), tmp)
  sf::st_read(tmp, quiet = TRUE)
}


## ---------------------------------------------------------------------
## 4) gs_download_layer() — baixa arquivo (SHP/GPKG/CSV) para o disco,
##    equivalente ao botão "Download de arquivos" do portal
## ---------------------------------------------------------------------

#' Baixa uma camada inteira em formato de arquivo (shapefile zipado ou
#' geopackage), replicando em código o botão de "Download" do GeoSampa
#'
#' @param type_name nome da camada (ex: "geoportal:distrito_municipal")
#' @param destfile caminho de saída, ex: "distritos.zip" ou "distritos.gpkg"
#' @param formato "SHAPE-ZIP" ou "gpkg" (formatos suportados pelo GeoServer)
gs_download_layer <- function(type_name, destfile,
                               formato = c("SHAPE-ZIP", "gpkg")) {
  formato <- match.arg(formato)

  url <- request(GEOSAMPA_WFS) |>
    req_url_query(
      service      = "WFS",
      version      = "1.0.0",
      request      = "GetFeature",
      typeName     = type_name,
      outputFormat = formato
    ) |>
    req_url() # monta a URL final (httr2 >= 1.0)

  download.file(url, destfile, mode = "wb", quiet = TRUE)
  invisible(destfile)
}


## =======================================================================
## 5) EXEMPLO DE USO / PROTÓTIPO
## =======================================================================

if (interactive() || sys.nframe() == 0) {

  ## --- 5.1 Descobrir camadas disponíveis --------------------------------
  camadas <- gs_list_layers()
  cat("Total de camadas publicadas no WFS do GeoSampa:", nrow(camadas), "\n")

  # busca por camadas relacionadas a "escola" ou "arvore"
  gs_list_layers("arvore")
  gs_list_layers("distrito")

  ## --- 5.2 Baixar os distritos do município (polígonos) -----------------
  distritos <- gs_get_layer("geoportal:distrito_municipal")
  print(distritos)

  ## --- 5.3 Filtrar no servidor (CQL) — ex: só o distrito da Sé ----------
  # (ajuste o nome do campo de acordo com o retorno de `names(distritos)`)
  # se_ <- gs_get_layer(
  #   "geoportal:distrito_municipal",
  #   cql_filter = "ds_nome = 'SE'"
  # )

  ## --- 5.4 Visualizar rapidamente com sf base ----------------------------
  plot(sf::st_geometry(distritos), col = "grey85", border = "white",
       main = "Distritos do Município de São Paulo (via WFS GeoSampa)")

  ## --- 5.5 Visualização "bonita" com ggplot2 -----------------------------
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    ggplot(distritos) +
      geom_sf(fill = "#2c7fb8", color = "white", linewidth = 0.15) +
      theme_minimal() +
      labs(
        title = "Distritos do Município de São Paulo",
        subtitle = "Dados obtidos em tempo real via WFS do GeoSampa (GeoServer)",
        caption = "Fonte: Prefeitura de São Paulo / GeoSampa (WFS)"
      )
  }

  ## --- 5.6 Baixar a camada como GeoPackage (arquivo local) ---------------
  # gs_download_layer("geoportal:distrito_municipal", "distritos.gpkg",
  #                    formato = "gpkg")

  ## --- 5.7 Exemplo de consulta espacial: árvores dentro de uma bbox ------
  ## (bbox pequena no centro de SP, em SIRGAS2000/UTM — EPSG:31983)
  # arvores_centro <- gs_get_layer(
  #   "geoportal:arvore",
  #   bbox = c(325000, 7394000, 326000, 7395000),
  #   max_features = 2000
  # )
  # plot(sf::st_geometry(arvores_centro), pch = 20, col = "forestgreen")
}
