# ============================================================
# GeoSampa — CEP: leitura, geocodificação e verificação
# ------------------------------------------------------------
# Funcionalidades:
#   1. Normalizar e validar um CEP (viaCEP, sem chave de acesso).
#   2. Obter a coordenada geográfica do CEP (índice local offline
#      ou Nominatim/OpenStreetMap como fallback).
#   3. Verificar se uma coordenada informada confere com um CEP.
# ============================================================

# --- Normaliza um CEP para 8 dígitos ---------------------------------------
gs_normalizar_cep <- function(cep) {
  if (length(cep) != 1 || is.na(cep) || !is.atomic(cep)) {
    stop("`cep` deve ser um valor escalar não ausente.")
  }
  cep_original <- as.character(cep)
  cep <- gsub("[^0-9]", "", cep_original)
  if (!nzchar(cep) || nchar(cep) != 8) {
    stop("CEP inválido: '", cep_original,
         "'. Informe 8 dígitos (ex.: 03175001 ou 03175-001).")
  }
  cep
}

# --- Máscara 00000-000 -------------------------------------------------------
gs_cep_mascarado <- function(cep) {
  cep <- gs_normalizar_cep(cep)
  paste0(substr(cep, 1, 5), "-", substr(cep, 6, 8))
}

# --- Lê e valida um CEP no serviço público viaCEP ---------------------------
# Devolve endereço, bairro, cidade, UF e código IBGE.
gs_ler_cep <- function(cep) {
  cep <- gs_normalizar_cep(cep)
  url <- sub("{cep}", cep, gs_urls$viacep, fixed = TRUE)
  resp <- gs_http_get(url, timeout_s = 30)
  httr::stop_for_status(resp)
  dados <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  if (!is.null(dados$erro) &&
      (isTRUE(dados$erro) || identical(as.character(dados$erro), "true"))) {
    stop("CEP ", gs_cep_mascarado(cep), " não encontrado na base do viaCEP.")
  }
  campo <- function(nm) {
    if (is.null(dados[[nm]])) NA_character_ else as.character(dados[[nm]])
  }
  data.frame(
    cep       = gs_cep_mascarado(cep),
    logradouro = campo("logradouro"),
    bairro     = campo("bairro"),
    cidade     = campo("localidade"),
    uf         = campo("uf"),
    ibge       = campo("ibge"),
    stringsAsFactors = FALSE
  )
}

# --- Consulta interna ao Nominatim (User-Agent identificado + pausa) ---------
# Respeita a política de uso (~1 requisição por segundo): espera antes de cada
# chamada e identifica o usuário. Devolve lista com lat/lon ou NULL se vazio.
gs_consultar_nominatim <- function(query) {
  if (!is.list(query)) stop("A consulta ao Nominatim deve ser uma lista nomeada.")
  query$format <- "json"
  query$limit <- "1"
  query$countrycodes <- "br"
  Sys.sleep(gs_pausa_nominatim_s)
  resp <- gs_http_get(
    gs_urls$nominatim,
    query = query,
    timeout_s = 30,
    httr::user_agent("geosampaR/1.0 (contato: paulopimenta6@gmail.com)")
  )
  httr::stop_for_status(resp)
  dados <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  if (length(dados) == 0) return(NULL)
  list(
    latitude  = as.numeric(dados[[1]]$lat),
    longitude = as.numeric(dados[[1]]$lon),
    nome      = if (is.null(dados[[1]]$display_name)) "" else dados[[1]]$display_name
  )
}

# --- Coordenadas (lat/long) de um CEP ----------------------------------------
# Cascata de fontes:
#   1. "local"       -> índice local construído dos data/*.csv (offline);
#   2. "nominatim"   -> busca por código postal no OpenStreetMap;
#   3. viaCEP + Nominatim por rua/cidade -> quando o código postal não existe
#      na base do OSM (comum no Brasil), usa o endereço do viaCEP para achar
#      a rua; em último caso, o centróide da cidade.
# Retorna data.frame com cep, latitude, longitude, fonte e precisão.
gs_cep_para_coordenadas <- function(cep, fonte = c("local", "nominatim"),
                                    dir = gs_caminho_dados()) {
  fonte <- match.arg(fonte)
  cep <- gs_normalizar_cep(cep)
  cep_masc <- gs_cep_mascarado(cep)

  if (fonte == "local") {
    ref <- tryCatch(
      gs_cep_referencia(dir = dir),
      gs_indice_ausente = function(e) data.frame()
    )
    achou <- ref[ref$cep == cep, , drop = FALSE]
    if (nrow(achou) > 0) {
      return(data.frame(
        cep       = cep_masc,
        latitude  = as.numeric(achou$latitude[1]),
        longitude = as.numeric(achou$longitude[1]),
        fonte     = "local",
        precisao  = "coordenada mediana do índice local (equipamentos públicos)",
        stringsAsFactors = FALSE
      ))
    }
    message("CEP ", cep_masc, " não está no índice local. Consultando o Nominatim/OSM...")
  }

  # Fallback / fonte direta: Nominatim. Cascata:
  #   1) código postal;  2) rua via viaCEP;  3) cidade via viaCEP.
  r <- gs_consultar_nominatim(list(postalcode = cep_masc, country = "Brazil"))
  precisao <- "coordenada aproximada do código postal (OpenStreetMap)"
  endereco <- NULL
  if (is.null(r)) {
    endereco <- tryCatch(gs_ler_cep(cep), error = function(e) NULL)
    if (!is.null(endereco) && !is.na(endereco$logradouro) &&
        nzchar(endereco$logradouro)) {
      r <- gs_consultar_nominatim(list(
        street = endereco$logradouro,
        city   = endereco$cidade, state = endereco$uf, country = "Brazil"))
      precisao <- "coordenada aproximada da rua (OpenStreetMap via viaCEP)"
    }
  }
  if (is.null(r)) {
    if (!is.null(endereco) && !is.na(endereco$cidade) && nzchar(endereco$cidade)) {
      r <- gs_consultar_nominatim(list(
        city = endereco$cidade, state = endereco$uf, country = "Brazil"))
      precisao <- "coordenada aproximada da cidade (OpenStreetMap via viaCEP)"
    }
  }
  if (is.null(r)) {
    stop("Não consegui obter coordenadas para o CEP ", cep_masc,
         ". Ele não está no índice local (só cobre CEPs de equipamentos ",
         "públicos) e o Nominatim não achou o código postal nem o endereço ",
         "no OpenStreetMap. Confira o CEP ou tente outro.")
  }
  coordenadas <- gs_validar_coordenadas(c(r$latitude, r$longitude))
  data.frame(
    cep       = cep_masc,
    latitude  = coordenadas[1],
    longitude = coordenadas[2],
    fonte     = "nominatim",
    precisao  = precisao,
    stringsAsFactors = FALSE
  )
}

# --- Resolve um ponto de interesse a partir de CEP ou coordenadas -----------
# Uso interno do módulo: devolve lista com latitude, longitude, origem (rótulo)
# e o ponto como objeto sf (EPSG:4326).
gs_resolver_ponto <- function(cep = NULL, coordenadas = NULL,
                              dir = gs_caminho_dados()) {
  tem_cep <- !is.null(cep)
  tem_coordenadas <- !is.null(coordenadas)
  if (identical(tem_cep, tem_coordenadas)) {
    stop("Informe exatamente um entre `cep` e `coordenadas`.")
  }

  if (tem_cep) {
    coord <- gs_cep_para_coordenadas(cep, dir = dir)
    ponto <- gs_validar_coordenadas(c(coord$latitude, coord$longitude))
    lat <- ponto[1]
    lon <- ponto[2]
    origem <- paste0("CEP ", coord$cep, " (fonte: ", coord$fonte, ")")
  } else {
    ponto <- gs_validar_coordenadas(coordenadas)
    lat <- ponto[1]
    lon <- ponto[2]
    origem <- "coordenadas informadas"
  }
  list(
    latitude  = lat,
    longitude = lon,
    origem    = origem,
    sf        = sf::st_sfc(sf::st_point(c(lon, lat)), crs = gs_epsg$wgs84)
  )
}

# --- Verifica se uma coordenada confere com um CEP ---------------------------
# Compara a coordenada informada com a(s) coordenada(s) de referência do CEP
# (índice local; se ausente, usa o Nominatim). Devolve uma lista com a
# distância mínima e o veredito dentro da tolerância.
gs_verificar_cep <- function(cep, latitude, longitude,
                              tolerancia_m = gs_tolerancia_cep_m,
                              dir = gs_caminho_dados()) {
  cep <- gs_normalizar_cep(cep)
  latitude <- gs_validar_numero_escalar(latitude, "latitude",
                                        minimo = -90, maximo = 90)
  longitude <- gs_validar_numero_escalar(longitude, "longitude",
                                         minimo = -180, maximo = 180)
  tolerancia_m <- gs_validar_numero_escalar(
    tolerancia_m, "tolerancia_m", minimo = 0
  )
  ponto <- sf::st_sfc(sf::st_point(c(longitude, latitude)), crs = gs_epsg$wgs84)

  indice <- tryCatch(
    gs_indice_cep(dir = dir),
    gs_indice_ausente = function(e) NULL
  )
  ocorrencias <- if (is.null(indice)) {
    data.frame()
  } else {
    indice[indice$cep == cep, , drop = FALSE]
  }

  if (nrow(ocorrencias) > 0) {
    pts <- sf::st_as_sf(ocorrencias, coords = c("longitude", "latitude"),
                        crs = gs_epsg$wgs84)
    dists <- as.numeric(sf::st_distance(ponto, pts))
    i <- which.min(dists)
    dmin <- dists[i]
    return(list(
      cep                    = gs_cep_mascarado(cep),
      latitude_cep           = ocorrencias$latitude[i],
      longitude_cep          = ocorrencias$longitude[i],
      distancia_m            = round(dmin, 1),
      confere                = dmin <= tolerancia_m,
      veredito               = if (dmin <= tolerancia_m) "CONFERE" else "NAO CONFERE",
      tolerancia_m           = tolerancia_m,
      n_ocorrencias          = nrow(ocorrencias),
      equipamento_referencia = if ("nm_equipamento" %in% names(ocorrencias)) {
        ocorrencias$nm_equipamento[i]
      } else {
        NA_character_
      },
      camada_referencia      = if ("camada" %in% names(ocorrencias)) {
        ocorrencias$camada[i]
      } else {
        NA_character_
      }
    ))
  }

  ref <- tryCatch(gs_cep_para_coordenadas(
    cep, fonte = "nominatim", dir = dir
  ),
  error = function(e) NULL)
  if (is.null(ref)) {
    return(list(
      cep                    = gs_cep_mascarado(cep),
      latitude_cep           = NA_real_,
      longitude_cep          = NA_real_,
      distancia_m            = NA_real_,
      confere                = NA,
      veredito               = "SEM DADO SUFICIENTE",
      tolerancia_m           = tolerancia_m,
      n_ocorrencias          = 0L,
      equipamento_referencia = NA_character_,
      camada_referencia      = NA_character_,
      motivo                 = paste0(
        "CEP fora do índice local e sem coordenada obtida no Nominatim/OSM ",
        "(nem viaCEP). Não foi possível verificar."
      )
    ))
  }
  ref_pt <- sf::st_sfc(sf::st_point(c(ref$longitude, ref$latitude)),
                       crs = gs_epsg$wgs84)
  d <- as.numeric(sf::st_distance(ponto, ref_pt))
  precisao_baixa <- length(ref$precisao) == 1 && !is.na(ref$precisao) &&
    grepl("rua|cidade", ref$precisao, ignore.case = TRUE)
  confere <- if (precisao_baixa) NA else d <= tolerancia_m
  resultado <- list(
    cep                    = gs_cep_mascarado(cep),
    latitude_cep           = ref$latitude,
    longitude_cep          = ref$longitude,
    distancia_m            = round(d, 1),
    confere                = confere,
    veredito               = if (precisao_baixa) {
      "SEM DADO SUFICIENTE"
    } else if (confere) {
      "CONFERE"
    } else {
      "NAO CONFERE"
    },
    tolerancia_m           = tolerancia_m,
    n_ocorrencias          = 0L,
    equipamento_referencia = NA_character_,
    camada_referencia      = NA_character_
  )
  if (precisao_baixa) {
    resultado$motivo <- paste0(
      "A referência disponível tem precisão de ", ref$precisao,
      "; a distância é apenas informativa."
    )
  }
  resultado
}
