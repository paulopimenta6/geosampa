# ============================================================
# GeoSampa — Configuração central
# ------------------------------------------------------------
# Aqui moram os "endereços" dos serviços web do GeoSampa, as
# projeções cartográficas e as pastas padrão do projeto.
# ============================================================

# --- URLs dos serviços web do GeoSampa -------------------------------------
# WFS  = "Web Feature Service": entrega os DADOS VETORIAIS (o "baú").
# WMS  = "Web Map Service": entrega IMAGENS do mapa (o "espelho").
# GeoNetwork = catálogo de metadados (os "documentos de identidade" das camadas).
gs_urls <- list(
  wfs         = "https://wfs.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wfs",
  wms         = "https://wms.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wms",
  metadados   = "https://metadados.geosampa.prefeitura.sp.gov.br/geonetwork/srv/por",
  geonet_api  = "https://metadados.geosampa.prefeitura.sp.gov.br/geonetwork/srv/api",
  viacep      = "https://viacep.com.br/ws/{cep}/json/",
  nominatim   = "https://nominatim.openstreetmap.org/search"
)

# --- Sistemas de referência cartográfica ----------------------------------
# EPSG 31983 = SIRGAS2000 / UTM 23S  -> referência oficial do GeoSampa.
# EPSG 4326  = WGS84 (latitude/longitude) -> usado no CSV para ficar "legível".
gs_epsg <- list(
  oficial    = 31983,  # SIRGAS2000 / UTM 23S
  geografica = 4674,   # SIRGAS2000 geográfica
  wgs84      = 4326    # WGS84 (graus)
)

# --- Página padrão do WFS (quantas feições buscar por requisição) ----------
gs_tamanho_pagina <- 1000

# --- Configurações do módulo de CEP -----------------------------------------
# Tolerância padrão (metros) para verificar se uma coordenada confere com um CEP.
gs_tolerancia_cep_m <- 300
# Pausa (segundos) entre consultas ao Nominatim, respeitando a política de uso
# (máximo ~1 requisição por segundo).
gs_pausa_nominatim_s <- 1
# Raio padrão (metros) para buscar serviços próximos.
gs_raio_padrao_m <- 2000
# Raio padrão (metros) usado nas análises de cobertura por buffer.
gs_raio_buffer_m <- 1000
# Tamanho padrão (metros) das células hexagonais das análises LISA/Getis-Ord.
gs_celula_hex_m <- 1000
# Servidor OSRM para distância por rede viária. Configurável por
# options(gs.osrm_server = 'http://...') ou options(osrm.server = ...).
gs_osrm_server <- function() {
  getOption("gs.osrm_server",
            getOption("osrm.server",
                      "https://router.project-osrm.org/"))
}
# Perfil OSRM (rota). Configurável por options(gs.osrm_profile = 'driving')
# ou options(osrm.profile = 'driving').
gs_osrm_profile <- function() {
  getOption("gs.osrm_profile",
            getOption("osrm.profile",
                      "driving"))
}
# Tabela de origem/destino para o pacote osrm, compatível com a API antiga
# (< 4.0: colunas id/lon/lat) e nova (>= 4.0: apenas lon/lat, id nos row.names).
gs_osrm_input <- function(ids, lon, lat) {
  if (utils::packageVersion("osrm") >= "4.0.0") {
    data.frame(lon = lon, lat = lat, row.names = ids, stringsAsFactors = FALSE)
  } else {
    data.frame(id = ids, lon = lon, lat = lat, stringsAsFactors = FALSE)
  }
}
# Converte distâncias do osrmTable para metros: < 4.0 devolve km; >= 4.0 devolve m.
gs_osrm_dist_m <- function(distancias) {
  if (utils::packageVersion("osrm") >= "4.0.0") {
    round(as.numeric(distancias), 1)
  } else {
    round(as.numeric(distancias) * 1000, 1)
  }
}
# Nomes das camadas administrativas (baixadas sob demanda do WFS do GeoSampa).
gs_camadas_apoio <- list(
  distritos = "distrito_municipal",
  setores   = "setor_censitario_2022"
)

# --- Localização da raiz do projeto ----------------------------------------
# Sobe de diretório em diretório até encontrar a pasta que contém R/ e scripts/.
# Também respeita a opção gs.raiz, que pode ser definida pelo usuário.
gs_raiz <- function() {
  raiz <- getOption("gs.raiz")
  if (!is.null(raiz)) return(raiz)
  dir <- getwd()
  repeat {
    if (dir.exists(file.path(dir, "R")) && dir.exists(file.path(dir, "scripts"))) {
      return(dir)
    }
    pai <- dirname(dir)
    if (identical(pai, dir)) {
      stop("Não consegui achar a raiz do projeto GeoSampa (procuro por R/ e scripts/). ",
           "Defina com options(gs.raiz = 'caminho') ou rode a partir da pasta do projeto.")
    }
    dir <- pai
  }
}

# --- Pasta de dados (criada automaticamente) --------------------------------
gs_pasta_dados <- function() {
  dir <- file.path(gs_raiz(), "data")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

# --- Funções auxiliares internas -------------------------------------------
# Garante que o vetor de camadas comece com o prefixo "geoportal:".
gs_nome_completo <- function(camada) {
  if (!grepl("^geoportal:", camada)) paste0("geoportal:", camada) else camada
}