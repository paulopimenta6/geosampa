# ============================================================
# GeoSampa — Download de dados (WFS)
# ------------------------------------------------------------
# Aqui está o "garimpo": funções que buscam as camadas no
# serviço WFS e salvam em GeoJSON (o mapa) e CSV (a tabela).
# ============================================================

# --- Consulta interna: uma página do WFS ------------------------------------
# Devolve a resposta GeoJSON já interpretada como lista R.
# Atenção: só enviamos startIndex quando ele é > 0, pois algumas camadas do
# GeoSampa não têm chave primária e o GeoServer responde 400 se o parâmetro
# estiver presente (erro de "natural order without a primary key").
gs_requisitar_pagina <- function(camada, count, startIndex = 0, filtro = NULL,
                                 sortBy = NULL) {
  query <- list(
    service = "WFS",
    version = "2.0.0",
    request = "GetFeature",
    typeNames = gs_nome_completo(camada),
    outputFormat = "application/json",
    count = count
  )
  if (startIndex > 0) query$startIndex <- startIndex
  if (!is.null(filtro)) query$cql_filter <- filtro
  if (!is.null(sortBy)) query$sortBy <- sortBy

  resp <- httr::GET(gs_urls$wfs, query = query, httr::timeout(120))
  httr::stop_for_status(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# --- Conta o total de feições de uma camada (respeitando o filtro) ----------
gs_contar <- function(camada, filtro = NULL) {
  p <- gs_requisitar_pagina(camada, count = 1, filtro = filtro)
  if (is.null(p$numberMatched)) {
    length(p$features)
  } else {
    as.integer(p$numberMatched)
  }
}

# --- Escolhe um atributo estável para ordenar a paginação -------------------
gs_detectar_ordenacao <- function(features) {
  if (length(features) == 0) return(NULL)
  props <- names(features[[1]]$properties)
  preferidas <- c("cd_identificador", "cd_equipamento", "id", "codigo")
  for (p in preferidas) if (p %in% props) return(p)
  if (length(props) > 0) props[1] else NULL
}

# --- Baixa uma camada inteira e salva GeoJSON (+ CSV opcional) --------------
gs_baixar_camada <- function(camada, filtro = NULL, dir = gs_pasta_dados(),
                             csv = TRUE, tamanho_pagina = gs_tamanho_pagina,
                             verbose = TRUE) {
  nome     <- gs_nome_completo(camada)
  base     <- gsub("^geoportal:", "", nome)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  geo_path <- file.path(dir, paste0(base, ".geojson"))
  csv_path <- file.path(dir, paste0(base, ".csv"))

  total <- gs_contar(camada, filtro)
  if (total == 0) {
    if (verbose) message("  [", base, "] nada encontrado — nenhum arquivo criado.")
    return(invisible(list(camada = base, total = 0, geojson = NULL, csv = NULL)))
  }

  if (verbose) cat("  [", base, "] baixando", total, "feições...\n", sep = "")

  feicoes <- list()
  startIndex <- 0
  sortBy <- NULL
  repeat {
    p <- gs_requisitar_pagina(camada, count = tamanho_pagina,
                              startIndex = startIndex, filtro = filtro,
                              sortBy = sortBy)
    feicoes <- c(feicoes, p$features)
    obtidas <- length(feicoes)
    if (verbose && obtidas < total) {
      cat("    ... ", obtidas, "/", total, "\n", sep = "")
    }
    if (obtidas >= total || length(p$features) == 0) break

    # Paginação por offset exige ordenação estável quando a camada não tem
    # chave primária. Detectamos um atributo na primeira página e reutilizamos.
    if (is.null(sortBy)) sortBy <- gs_detectar_ordenacao(p$features)
    if (is.null(sortBy)) {
      # Sem atributo de ordenação, buscamos tudo de uma vez (camadas pequenas).
      p <- gs_requisitar_pagina(camada, count = total, filtro = filtro)
      feicoes <- p$features
      break
    }
    startIndex <- obtidas
  }

  fc <- list(
    type          = "FeatureCollection",
    totalFeatures = length(feicoes),
    numberMatched = total,
    features      = feicoes,
    crs           = list(type = "name",
                         properties = list(name = paste0("urn:ogc:def:crs:EPSG::", gs_epsg$oficial)))
  )
  writeLines(jsonlite::toJSON(fc, auto_unbox = TRUE, digits = NA, null = "null"),
             geo_path, useBytes = TRUE)

  csv_salvo <- NULL
  if (csv) {
    csv_salvo <- gs_escrever_csv(geo_path, csv_path)
  }

  if (verbose) cat("    ok:", basename(geo_path),
                   if (!is.null(csv_salvo)) paste0("e ", basename(csv_salvo)), "\n")

  invisible(list(camada = base, total = total, geojson = geo_path, csv = csv_salvo))
}

# --- Converte o GeoJSON baixado em CSV com latitude/longitude ----------------
gs_escrever_csv <- function(geo_path, csv_path) {
  cam <- sf::st_read(geo_path, quiet = TRUE)
  df  <- sf::st_drop_geometry(cam)

  tipo_geom <- class(sf::st_geometry(cam))[1]
  if (identical(tipo_geom, "sfc_POINT")) {
    xy <- sf::st_coordinates(sf::st_transform(cam, gs_epsg$wgs84))
    df$latitude  <- xy[, "Y"]
    df$longitude <- xy[, "X"]
  } else {
    df$geometria_wkt <- sf::st_as_text(sf::st_geometry(cam))
  }

  readr::write_csv(df, csv_path)
  csv_path
}

# --- Baixa várias camadas de uma vez -----------------------------------------
# `camadas` pode ser um vetor de nomes ou um data.frame vindo de
# gs_catalogo_equipamentos(). Devolve um resumo do que foi baixado.
gs_baixar_camadas <- function(camadas, filtro = NULL, dir = gs_pasta_dados(),
                              csv = TRUE, verbose = TRUE) {
  nomes <- if (is.data.frame(camadas)) camadas$camada else as.character(camadas)
  resumo <- lapply(nomes, function(cam) {
    gs_baixar_camada(cam, filtro = filtro, dir = dir, csv = csv, verbose = verbose)
  })
  do.call(rbind, lapply(resumo, function(r) {
    data.frame(
      camada  = r$camada,
      total   = r$total,
      geojson = if (is.null(r$geojson)) NA else r$geojson,
      csv     = if (is.null(r$csv)) NA else r$csv,
      stringsAsFactors = FALSE
    )
  }))
}

# --- Baixa TODOS os equipamentos públicos (o grande garimpo) -----------------
gs_baixar_todos_equipamentos <- function(dir = gs_pasta_dados(), csv = TRUE,
                                         verbose = TRUE) {
  catalogo <- gs_camadas_equipamentos()
  if (verbose) {
    cat("Encontrei", nrow(catalogo), "camadas de equipamentos públicos.\n")
    cat("Começando o baixador...\n")
  }
  gs_baixar_camadas(catalogo, dir = dir, csv = csv, verbose = verbose)
}

# --- Baixa equipamentos de um único tema -------------------------------------
# Ex.: gs_baixar_servicos("saude") baixa UBS, hospitais, pronto-socorros etc.
gs_baixar_servicos <- function(tema, dir = gs_pasta_dados(), csv = TRUE,
                               verbose = TRUE) {
  catalogo <- gs_catalogo_equipamentos()
  tema <- tolower(tema)
  sel <- catalogo[grepl(tema, catalogo$tema, ignore.case = TRUE) |
                  grepl(tema, catalogo$titulo, ignore.case = TRUE), , drop = FALSE]
  if (nrow(sel) == 0) {
    stop("Nenhum equipamento encontrado para o tema '", tema,
         "'. Dica: use gs_catalogo_equipamentos() para ver os temas disponíveis.")
  }
  if (verbose) cat("Encontrei", nrow(sel), "camadas para o tema '", tema, "'.\n", sep = "")
  gs_baixar_camadas(sel, dir = dir, csv = csv, verbose = verbose)
}