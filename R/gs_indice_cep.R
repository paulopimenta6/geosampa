# ============================================================
# GeoSampa — Índice local de CEP -> coordenadas
# ------------------------------------------------------------
# Constrói, a partir dos CSVs já baixados em data/, uma tabela
# que liga cada CEP às coordenadas (latitude/longitude) dos
# equipamentos públicos que o utilizam. Serve como fonte
# OFFLINE, rápida e gratuita para geocodificar CEPs.
# ============================================================

# --- Lista as camadas disponíveis localmente (nomes base dos CSVs) ---------
gs_camadas_local <- function(dir = gs_caminho_dados()) {
  arquivos <- gs_listar_arquivos_consistente(dir, pattern = "\\.csv$")
  sort(gsub("\\.csv$", "", basename(arquivos)))
}

gs_parar_indice_ausente <- function(mensagem) {
  erro <- simpleError(mensagem, call = NULL)
  class(erro) <- c("gs_indice_ausente", class(erro))
  stop(erro)
}

gs_chave_cache_indice <- function(dir, arquivos) {
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  if (length(arquivos) == 0) return(paste0(dir, "|sem_csv"))
  arquivos <- sort(arquivos)
  info <- file.info(arquivos)
  assinatura <- paste(
    basename(arquivos),
    format(info$size, scientific = FALSE, trim = TRUE),
    format(as.numeric(info$mtime), digits = 17, scientific = FALSE, trim = TRUE),
    sep = ":"
  )
  paste(c(dir, assinatura), collapse = "|")
}

# --- Monta o índice local CEP -> coordenadas --------------------------------
# Varre todos os data/*.csv, mantendo UMA LINHA por ocorrência (um mesmo CEP
# pode aparecer em vários endereços/equipamentos). O resultado é cacheado na
# sessão (options); use force = TRUE para reconstruir.
# Colunas: cep, camada, latitude, longitude, nm_equipamento,
#          nm_bairro_equipamento, tx_endereco_equipamento.
gs_indice_cep_impl <- function(dir = gs_caminho_dados(), force = FALSE) {
  if (!is.logical(force) || length(force) != 1 || is.na(force)) {
    stop("`force` deve ser TRUE ou FALSE.")
  }
  arquivos <- sort(gs_listar_arquivos_consistente(
    dir, pattern = "\\.csv$", full.names = TRUE
  ))
  chave_cache <- gs_chave_cache_indice(dir, arquivos)
  cache <- getOption("gs.indice_cep")
  if (!force && is.data.frame(cache) &&
      identical(attr(cache, "gs.chave_cache"), chave_cache)) {
    return(cache)
  }

  if (length(arquivos) == 0) {
    gs_parar_indice_ausente(paste0(
      "Nenhum arquivo CSV em ", dir,
      ". Baixe as camadas antes com gs_baixar_todos_equipamentos()."
    ))
  }

  linhas <- lapply(arquivos, function(arq) {
    camada <- gsub("\\.csv$", "", basename(arq))
    cabecalho <- gs_ler_cabecalho_csv(arq)
    colunas <- c("cd_cep_equipamento", "latitude", "longitude")
    if (!all(colunas %in% names(cabecalho))) {
      return(NULL)  # camadas sem ponto/CEP (ex.: polígonos) são ignoradas
    }
    tab <- gs_ler_csv_verificado(
      arq,
      col_types = readr::cols(
        cd_cep_equipamento = readr::col_character(),
        latitude = readr::col_double(),
        longitude = readr::col_double(),
        .default = readr::col_guess()
      )
    )
    out <- data.frame(
      camada    = camada,
      cep       = as.character(tab$cd_cep_equipamento),
      latitude  = suppressWarnings(as.numeric(tab$latitude)),
      longitude = suppressWarnings(as.numeric(tab$longitude)),
      stringsAsFactors = FALSE
    )
    for (col in c("nm_equipamento", "nm_bairro_equipamento", "tx_endereco_equipamento")) {
      out[[col]] <- if (col %in% names(tab)) as.character(tab[[col]]) else NA_character_
    }
    out
  })

  idx <- do.call(rbind, Filter(Negate(is.null), linhas))
  if (is.null(idx) || nrow(idx) == 0) {
    gs_parar_indice_ausente(paste0(
      "Nenhum registro com CEP e coordenadas encontrado nos CSVs de ", dir, "."
    ))
  }

  idx$cep <- gsub("\\D", "", idx$cep)
  idx <- idx[!is.na(idx$cep) & nchar(idx$cep) == 8 &
              !is.na(idx$latitude) & is.finite(idx$latitude) &
              idx$latitude >= -90 & idx$latitude <= 90 &
              !is.na(idx$longitude) & is.finite(idx$longitude) &
              idx$longitude >= -180 & idx$longitude <= 180, , drop = FALSE]
  if (nrow(idx) == 0) {
    gs_parar_indice_ausente(paste0(
      "Nenhum registro com CEP e coordenadas válidos encontrado nos CSVs de ",
      dir, "."
    ))
  }
  idx$latitude  <- as.numeric(idx$latitude)
  idx$longitude <- as.numeric(idx$longitude)
  rownames(idx) <- NULL

  # Colunas do plano: n_ocorrencias (registros por CEP) e representante
  # (TRUE para a ocorrência mais próxima da mediana daquele CEP).
  idx$n_ocorrencias <- as.integer(stats::ave(seq_len(nrow(idx)), idx$cep, FUN = length))
  med <- stats::aggregate(cbind(latitude, longitude) ~ cep, data = idx,
                          FUN = stats::median)
  names(med) <- c("cep", "latitude_med", "longitude_med")
  idx <- merge(idx, med, by = "cep", all.x = TRUE)
  lat_m <- 111320
  lon_m <- 111320 * cos(stats::median(idx$latitude) * pi / 180)
  idx$dist_med_m <- sqrt(((idx$latitude - idx$latitude_med) * lat_m)^2 +
                         ((idx$longitude - idx$longitude_med) * lon_m)^2)
  idx$representante <- FALSE
  i_min <- tapply(seq_len(nrow(idx)), idx$cep,
                  function(i) i[which.min(idx$dist_med_m[i])])
  idx$representante[as.integer(i_min)] <- TRUE
  idx$latitude_med  <- NULL
  idx$longitude_med <- NULL
  idx$dist_med_m    <- NULL

  idx <- idx[order(idx$cep), , drop = FALSE]
  rownames(idx) <- NULL

  attr(idx, "gs.chave_cache") <- chave_cache
  options(gs.indice_cep = idx)
  idx
}

gs_indice_cep <- function(dir = gs_caminho_dados(), force = FALSE) {
  executar <- function() gs_indice_cep_impl(dir = dir, force = force)
  if (!dir.exists(dir)) return(executar())
  gs_com_lock(gs_lock_diretorio(dir), executar())
}

# --- Coordenada "representante" de cada CEP (mediana das ocorrências) -------
gs_cep_referencia <- function(indice = NULL, dir = gs_caminho_dados()) {
  if (is.null(indice)) indice <- gs_indice_cep(dir = dir)
  if (is.null(indice) || nrow(indice) == 0) return(data.frame())
  stats::aggregate(
    cbind(latitude, longitude) ~ cep,
    data = indice,
    FUN = stats::median
  )
}
