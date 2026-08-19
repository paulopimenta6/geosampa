# ============================================================
# GeoSampa — Catálogo de camadas (GetCapabilities)
# ------------------------------------------------------------
# Pergunta ao serviço WFS quais camadas existem e organiza
# um "cardápio" de dados. Também localiza as camadas de
# equipamentos públicos (as que têm prefixo equipamento_*).
# ============================================================

# --- Consulta o GetCapabilities e monta o catálogo -------------------------
# O resultado é um data.frame com: camada (nome técnico), titulo, resumo e crs.
gs_camadas_disponiveis <- function(force = FALSE) {
  cache <- file.path(tempdir(), "gs_cap_wfs.xml")

  if (!force && file.exists(cache) &&
      difftime(Sys.time(), file.info(cache)$mtime, units = "hours") < 1) {
    txt <- readLines(cache, warn = FALSE, encoding = "UTF-8")
  } else {
    resp <- httr::GET(gs_urls$wfs, query = list(
      service = "WFS", version = "2.0.0", request = "GetCapabilities"
    ))
    httr::stop_for_status(resp)
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    writeLines(txt, cache)
  }

  xml <- xml2::read_xml(paste(txt, collapse = "\n"))
  ns  <- xml2::xml_ns(xml)
  fts <- xml2::xml_find_all(xml, ".//wfs:FeatureType", ns)

  extrair <- function(p) {
    x <- xml2::xml_find_first(fts, p, ns)
    xml2::xml_text(x)
  }

  data.frame(
    camada = extrair("./wfs:Name"),
    titulo = extrair("./wfs:Title"),
    resumo = extrair("./wfs:Abstract"),
    crs    = extrair("./wfs:DefaultCRS"),
    stringsAsFactors = FALSE
  )
}

# --- Filtra apenas as camadas de equipamentos públicos ----------------------
# As camadas de equipamentos seguem o padrão "equipamento_*" (UBS, CEU, CRAS,
# hospitais, bibliotecas, centros esportivos, feiras livres etc.).
gs_camadas_equipamentos <- function(force = FALSE) {
  catalogo <- gs_camadas_disponiveis(force = force)
  catalogo[grepl("equipamento_", catalogo$camada, ignore.case = TRUE), , drop = FALSE]
}

# --- Busca camadas por palavra-chave no título ou nome ----------------------
# Ex.: gs_buscar_camadas("saude") ou gs_buscar_camadas("educação")
gs_buscar_camadas <- function(termo, force = FALSE) {
  catalogo <- gs_camadas_disponiveis(force = force)
  sel <- grepl(termo, catalogo$camada, ignore.case = TRUE) |
         grepl(termo, catalogo$titulo, ignore.case = TRUE)
  catalogo[sel, , drop = FALSE]
}

# --- Devolve o "cardápio" amigável de equipamentos --------------------------
gs_catalogo_equipamentos <- function(force = FALSE) {
  eq <- gs_camadas_equipamentos(force = force)
  eq$tema <- gsub("^equipamento_([a-z_]+?)(_.*)?$", "\\1", eq$camada)
  eq[order(eq$tema, eq$titulo), c("camada", "tema", "titulo", "resumo")]
}

# --- Tema de uma camada local (mesma regra do catálogo) ---------------------
# "equipamento_saude_ubs_posto_centro" -> "saude"
gs_tema_camada <- function(camada) {
  gsub("^equipamento_([a-z_]+?)(_.*)?$", "\\1", camada)
}

# --- Lista os serviços disponíveis LOCALMENTE, agrupados por tema -----------
# Mostra exatamente o que usar no argumento `camadas` de gs_servicos_proximos().
# Só lista camadas de ponto (com latitude/longitude nos CSVs de data/).
# `termo` filtra por tema ou nome (ex.: gs_listar_servicos("saude")).
gs_listar_servicos <- function(termo = NULL, dir = gs_pasta_dados()) {
  camadas <- gs_camadas_local(dir)
  tem <- vapply(camadas, function(cam) {
    arq <- file.path(dir, paste0(cam, ".csv"))
    tab <- tryCatch(readr::read_csv(arq, n_max = 0, show_col_types = FALSE),
                    error = function(e) NULL)
    !is.null(tab) && all(c("latitude", "longitude") %in% names(tab))
  }, logical(1))
  camadas <- sort(camadas[tem])
  if (length(camadas) == 0) {
    stop("Nenhum serviço de ponto (com latitude/longitude) em ", dir,
         ". Baixe as camadas com gs_baixar_todos_equipamentos().")
  }
  tab <- data.frame(
    tema   = gs_tema_camada(camadas),
    camada = camadas,
    stringsAsFactors = FALSE
  )
  if (!is.null(termo)) {
    tab <- tab[grepl(termo, tab$tema, ignore.case = TRUE) |
               grepl(termo, tab$camada, ignore.case = TRUE), , drop = FALSE]
    if (nrow(tab) == 0) {
      stop("Nenhum serviço local encontrado para '", termo,
           "'. Dica: gs_listar_servicos() mostra todos.")
    }
  }
  tab <- tab[order(tab$tema, tab$camada), , drop = FALSE]
  rownames(tab) <- NULL

  temas <- split(tab$camada, tab$tema)
  cat("🎒 Serviços disponíveis em data/ — ",
      if (is.null(termo)) paste0(length(camadas), " camadas") else
        paste0(nrow(tab), " de ", length(camadas), " camadas"),
      ". Use estes nomes no argumento `camadas`:\n\n", sep = "")
  for (t in names(temas)) {
    cat("── ", t, " ──\n", sep = "")
    cat(paste0("   ", temas[[t]]), sep = "\n")
    cat("\n")
  }
  cat("💡 Também funciona passar só o tema (ex.: camadas = \"saude\") ",
      "ou um pedaço do nome (ex.: \"ubs\").\n", sep = "")
  invisible(tab)
}
