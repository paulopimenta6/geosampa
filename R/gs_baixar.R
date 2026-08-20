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
                                  sortBy = NULL, resultType = NULL) {
  query <- list(
    service = "WFS",
    version = "2.0.0",
    request = "GetFeature",
    typeNames = gs_nome_completo(camada),
    outputFormat = "application/json",
    srsName = paste0("EPSG:", gs_epsg$oficial),
    count = count
  )
  if (startIndex > 0) query$startIndex <- startIndex
  if (!is.null(filtro)) query$cql_filter <- filtro
  if (!is.null(sortBy)) query$sortBy <- sortBy
  if (!is.null(resultType)) query$resultType <- resultType

  resp <- gs_http_get(gs_urls$wfs, query = query)
  httr::stop_for_status(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (grepl("^\\s*<", txt)) {
    xml <- xml2::read_xml(txt)
    raiz <- xml2::xml_root(xml)
    if (grepl("Exception", xml2::xml_name(raiz), ignore.case = TRUE)) {
      mensagem <- paste(xml2::xml_text(xml2::xml_find_all(
        xml, ".//*[local-name()='ExceptionText']"
      )), collapse = " ")
      stop("O WFS devolveu uma exceção: ", mensagem)
    }
    nome_raiz <- xml2::xml_name(raiz)
    if (is.null(resultType) || !identical(resultType, "hits") ||
        !grepl("FeatureCollection$", nome_raiz)) {
      stop("O WFS devolveu XML inesperado para uma página de dados: ", nome_raiz)
    }
    return(list(
      numberMatched = xml2::xml_attr(raiz, "numberMatched"),
      totalFeatures = xml2::xml_attr(raiz, "numberOfFeatures"),
      features = list()
    ))
  }
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

gs_nome_saida_camada <- function(camada, filtro = NULL) {
  base <- gs_nome_base(camada)
  if (is.null(filtro)) return(base)
  paste0(
    base, "__filtro_", substr(gs_slug(filtro, padrao = "consulta", max_chars = 32), 1, 32),
    "_", gs_hash_curto(filtro)
  )
}

# --- Conta o total de feições de uma camada (respeitando o filtro) ----------
gs_contar <- function(camada, filtro = NULL) {
  p <- gs_requisitar_pagina(
    camada, count = 1, filtro = filtro, resultType = "hits"
  )
  total <- p$numberMatched
  if (is.null(total)) total <- p$totalFeatures
  total_chr <- if (is.null(total) || length(total) != 1) {
    NA_character_
  } else {
    trimws(as.character(total))
  }
  if (is.na(total_chr) || identical(tolower(total_chr), "unknown")) {
    return(NA_integer_)
  }
  total_num <- suppressWarnings(as.numeric(total_chr))
  if (is.na(total_num) || !is.finite(total_num) || total_num < 0 ||
      total_num != floor(total_num)) {
    stop("O WFS devolveu `numberMatched` inválido para a camada '",
         gs_nome_base(camada), "': ", as.character(total), ".")
  }
  as.integer(total_num)
}

# --- Escolhe um atributo estável para ordenar a paginação -------------------
gs_detectar_ordenacao <- function(features) {
  if (length(features) == 0) return(NULL)
  props <- lapply(features, function(feature) names(feature$properties))
  if (any(lengths(props) == 0)) return(NULL)
  props <- Reduce(intersect, props)
  if (length(props) == 0) return(NULL)

  preferidas <- c("cd_identificador", "cd_equipamento", "id", "codigo")
  preferidas <- props[match(preferidas, tolower(props), nomatch = 0)]
  candidatas <- unique(c(preferidas, props))

  for (p in candidatas) {
    valores <- vapply(features, function(feature) {
      valor <- feature$properties[[p]]
      if (length(valor) != 1 || !is.atomic(valor) || is.na(valor) ||
          !nzchar(as.character(valor))) {
        return(NA_character_)
      }
      paste0(typeof(valor), ":", as.character(valor))
    }, character(1))
    if (!anyNA(valores) && !anyDuplicated(valores)) return(p)
  }
  NULL
}

gs_feicoes_pagina <- function(pagina) {
  if (is.null(pagina$features)) return(list())
  if (!is.list(pagina$features)) {
    stop("Resposta WFS inválida: `features` não é uma lista.")
  }
  pagina$features
}

gs_validar_feicoes_baixadas <- function(feicoes, total_esperado = NA_integer_,
                                         chave_ordenacao = NULL) {
  ids <- vapply(feicoes, function(feature) {
    id <- feature$id
    if (length(id) != 1 || is.na(id) || !nzchar(as.character(id))) {
      NA_character_
    } else {
      as.character(id)
    }
  }, character(1))
  ids_presentes <- ids[!is.na(ids)]
  if (anyDuplicated(ids_presentes)) {
    repetido <- unique(ids_presentes[duplicated(ids_presentes)])[1]
    stop("Download WFS inválido: ID de feição duplicado ('", repetido, "').")
  }

  if (!is.null(chave_ordenacao)) {
    valores <- vapply(feicoes, function(feature) {
      valor <- feature$properties[[chave_ordenacao]]
      if (length(valor) != 1 || !is.atomic(valor) || is.na(valor) ||
          !nzchar(as.character(valor))) {
        NA_character_
      } else {
        paste0(typeof(valor), ":", as.character(valor))
      }
    }, character(1))
    if (anyNA(valores) || anyDuplicated(valores)) {
      stop("A chave de ordenação '", chave_ordenacao,
           "' não é única em todas as feições baixadas.")
    }
  }

  if (!is.na(total_esperado) && length(feicoes) != total_esperado) {
    stop("Download WFS incompleto: esperadas ", total_esperado,
         " feições, mas foram recebidas ", length(feicoes), ".")
  }
  invisible(TRUE)
}

# --- Baixa uma camada inteira e salva GeoJSON (+ CSV opcional) --------------
gs_baixar_camada <- function(camada, filtro = NULL, dir = gs_pasta_dados(),
                              csv = TRUE, tamanho_pagina = gs_tamanho_pagina,
                              verbose = TRUE) {
  if (length(camada) != 1 || is.na(camada) || !nzchar(as.character(camada))) {
    stop("`camada` deve ser um nome escalar não vazio.")
  }
  tamanho_pagina <- as.integer(gs_validar_numero_escalar(
    tamanho_pagina, "tamanho_pagina", minimo = 1, inteiro = TRUE
  ))
  if (!is.logical(csv) || length(csv) != 1 || is.na(csv) ||
      !is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("`csv` e `verbose` devem ser TRUE ou FALSE.")
  }
  nome     <- gs_nome_completo(camada)
  base     <- gs_nome_base(nome)
  base_saida <- gs_nome_saida_camada(base, filtro)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  geo_path <- file.path(dir, paste0(base_saida, ".geojson"))
  csv_path <- file.path(dir, paste0(base_saida, ".csv"))

  total_esperado <- gs_contar(camada, filtro)
  if (!is.na(total_esperado) && total_esperado == 0) {
    gs_promover_conjunto_atomico(
      rep(NA_character_, 2), c(geo_path, csv_path)
    )
    if (verbose) message("  [", base, "] nada encontrado — nenhum arquivo criado.")
    return(invisible(list(camada = base, total = 0, geojson = NULL, csv = NULL)))
  }

  if (verbose) {
    total_txt <- if (is.na(total_esperado)) "quantidade desconhecida de" else total_esperado
    cat("  [", base, "] baixando ", total_txt, " feições...\n", sep = "")
  }

  # A amostra serve apenas para descobrir uma chave realmente única. Quando
  # houver paginação, a página zero é requisitada novamente já ordenada.
  amostra <- gs_feicoes_pagina(gs_requisitar_pagina(
    camada, count = tamanho_pagina, filtro = filtro
  ))
  if (length(amostra) == 0) {
    gs_validar_feicoes_baixadas(amostra, total_esperado)
    gs_promover_conjunto_atomico(
      rep(NA_character_, 2), c(geo_path, csv_path)
    )
    if (verbose) message("  [", base, "] nada encontrado — nenhum arquivo criado.")
    return(invisible(list(camada = base, total = 0, geojson = NULL, csv = NULL)))
  }

  amostra_completa <- if (is.na(total_esperado)) {
    FALSE
  } else {
    length(amostra) >= total_esperado
  }
  sortBy <- NULL

  if (amostra_completa) {
    feicoes <- amostra
  } else {
    sortBy <- gs_detectar_ordenacao(amostra)
    if (is.null(sortBy)) {
      if (is.na(total_esperado)) {
        stop("O WFS não informou a contagem e a amostra não contém uma chave ",
             "única; não é possível paginar a camada com segurança.")
      }
      # Sem chave, uma única requisição evita paginação instável.
      feicoes <- gs_feicoes_pagina(gs_requisitar_pagina(
        camada, count = total_esperado, filtro = filtro
      ))
    } else {
      feicoes <- list()
      startIndex <- 0L
      repeat {
        pagina <- gs_feicoes_pagina(gs_requisitar_pagina(
          camada, count = tamanho_pagina, startIndex = startIndex,
          filtro = filtro, sortBy = sortBy
        ))
        n_pagina <- length(pagina)
        if (n_pagina == 0) break
        feicoes <- c(feicoes, pagina)
        gs_validar_feicoes_baixadas(
          feicoes, chave_ordenacao = sortBy
        )
        obtidas <- length(feicoes)
        if (verbose && !is.na(total_esperado) && obtidas < total_esperado) {
          cat("    ... ", obtidas, "/", total_esperado, "\n", sep = "")
        }
        if (!is.na(total_esperado) && obtidas >= total_esperado) break
        startIndex <- startIndex + n_pagina
      }
    }
  }

  gs_validar_feicoes_baixadas(feicoes, total_esperado, sortBy)
  total <- length(feicoes)

  fc <- list(
    type          = "FeatureCollection",
    totalFeatures = length(feicoes),
    numberMatched = total,
    features      = feicoes,
    crs           = list(type = "name",
                         properties = list(name = paste0("urn:ogc:def:crs:EPSG::", gs_epsg$oficial)))
  )
  conteudo <- jsonlite::toJSON(fc, auto_unbox = TRUE, digits = NA, null = "null")
  stage <- tempfile(pattern = paste0(".", base_saida, "-"), tmpdir = dir)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  stage_geo <- file.path(stage, basename(geo_path))
  writeLines(conteudo, stage_geo, useBytes = TRUE)

  stage_csv <- NULL
  if (csv) {
    stage_csv <- file.path(stage, basename(csv_path))
    gs_escrever_csv(stage_geo, stage_csv)
  }
  origens <- c(stage_geo, if (!is.null(stage_csv)) stage_csv else NA_character_)
  destinos <- c(geo_path, csv_path)
  gs_promover_conjunto_atomico(origens, destinos)
  csv_salvo <- if (csv) csv_path else NULL

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

  gs_gravar_atomico(csv_path, function(temporario) {
    readr::write_csv(df, temporario)
  })
  csv_path
}

# --- Baixa várias camadas de uma vez -----------------------------------------
# `camadas` pode ser um vetor de nomes ou um data.frame vindo de
# gs_catalogo_equipamentos(). Devolve um resumo do que foi baixado.
gs_baixar_camadas <- function(camadas, filtro = NULL, dir = gs_pasta_dados(),
                               csv = TRUE, verbose = TRUE) {
  nomes <- if (is.data.frame(camadas)) camadas$camada else as.character(camadas)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(pattern = ".gs-lote-", tmpdir = dir)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  resumo <- lapply(nomes, function(cam) {
    gs_baixar_camada(cam, filtro = filtro, dir = stage, csv = csv, verbose = verbose)
  })
  origens <- character(0)
  destinos <- character(0)
  for (i in seq_along(resumo)) {
    base_saida <- gs_nome_saida_camada(nomes[i], filtro)
    destinos <- c(
      destinos,
      file.path(dir, paste0(base_saida, c(".geojson", ".csv")))
    )
    origens <- c(
      origens,
      if (is.null(resumo[[i]]$geojson)) NA_character_ else resumo[[i]]$geojson,
      if (is.null(resumo[[i]]$csv)) NA_character_ else resumo[[i]]$csv
    )
  }
  gs_promover_conjunto_atomico(origens, destinos)
  for (i in seq_along(resumo)) {
    if (is.null(resumo[[i]]$geojson)) {
      next
    } else {
      resumo[[i]]$geojson <- file.path(dir, basename(resumo[[i]]$geojson))
      if (!is.null(resumo[[i]]$csv)) {
        resumo[[i]]$csv <- file.path(dir, basename(resumo[[i]]$csv))
      }
    }
  }
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
