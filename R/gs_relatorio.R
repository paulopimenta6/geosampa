# ============================================================
# GeoSampa - relatórios, métricas e persistência de artefatos
# ============================================================

gs_manifesto_vazio <- function() {
  data.frame(
    id_origem = character(0), categoria = character(0),
    analise = character(0), item = character(0), formato = character(0),
    caminho = character(0), status = character(0), mensagem = character(0),
    stringsAsFactors = FALSE
  )
}

gs_linha_manifesto <- function(id_origem, categoria, analise, item, formato,
                                caminho, status = "ok", mensagem = NA_character_) {
  caminho <- path.expand(as.character(caminho))
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", caminho)) {
    caminho <- file.path(getwd(), caminho)
  }
  data.frame(
    id_origem = as.character(id_origem), categoria = categoria,
    analise = analise, item = item, formato = formato,
    caminho = normalizePath(caminho, winslash = "/", mustWork = FALSE),
    status = status, mensagem = mensagem,
    stringsAsFactors = FALSE
  )
}

gs_escrever_csv_atomico <- function(x, caminho) {
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  gs_gravar_atomico(caminho, function(temporario) {
    readr::write_csv(x, temporario)
  })
}

gs_escrever_texto_atomico <- function(x, caminho) {
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  gs_gravar_atomico(caminho, function(temporario) {
    writeLines(x, temporario, useBytes = TRUE)
  })
}

gs_salvar_plot <- function(plot, caminho, largura = 10, altura = 7, dpi = 200) {
  if (!inherits(plot, "ggplot")) stop("`plot` deve ser um objeto ggplot.")
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  gs_gravar_atomico(caminho, function(temporario) {
    ggplot2::ggsave(
      filename = temporario, plot = plot, width = largura, height = altura,
      dpi = dpi, bg = "white"
    )
  })
}

gs_plot_sf <- function(sf_obj, titulo) {
  ggplot2::ggplot(sf_obj) +
    ggplot2::geom_sf(fill = "#41b6c4", color = "white", linewidth = 0.15,
                     alpha = 0.75) +
    gs_tema_mapa() +
    ggplot2::labs(title = titulo, caption = "Fonte: GeoSampa")
}

gs_unidade_metrica <- function(nome) {
  nome <- tolower(nome)
  chave <- sub(".*[.]", "", nome)
  if (grepl("^(p_?valor|valor_?p|p_?ajustado|p_valor_ajustado)$", chave)) {
    return("probabilidade")
  }
  if (grepl("pct|percent|^cv$", chave)) return("%")
  if (grepl("km2|km_?2", chave)) return("km2")
  if (grepl("m2|m_?2", chave)) return("m2")
  if (grepl("(_m$|^celula_m$|^buffer_m$|^erro_padrao$)", chave)) return("m")
  estatistica_distancia <- grepl("distancia", nome) &&
    chave %in% c("min", "max", "media", "mean", "mediana", "median", "mad",
                 "iqr", "p05", "p25", "p75", "p95", "1st_qu", "3rd_qu",
                 "ic95_media_inf", "ic95_media_sup")
  if (estatistica_distancia) return("m")
  NA_character_
}

gs_metricas_analises <- function(analises, id_origem = NA_character_) {
  linhas <- list()

  adicionar <- function(caminho, valor) {
    partes <- strsplit(caminho, ".", fixed = TRUE)[[1]]
    analise <- partes[1]
    metrica <- if (length(partes) > 1) paste(partes[-1], collapse = ".") else "valor"
    numerico <- NA_real_
    logico <- NA
    texto <- NA_character_
    if (is.logical(valor)) {
      logico <- as.logical(valor)
    } else if (is.numeric(valor)) {
      numerico <- as.numeric(valor)
    } else {
      texto <- as.character(valor)
    }
    linhas[[length(linhas) + 1L]] <<- data.frame(
      id_origem = as.character(id_origem), analise = analise,
      metrica = metrica, valor_numerico = numerico,
      valor_logico = logico, valor_texto = texto,
      unidade = gs_unidade_metrica(caminho), stringsAsFactors = FALSE
    )
  }

  visitar <- function(x, caminho) {
    if (is.null(x) || inherits(x, c("ggplot", "sf", "sfc", "table", "htest",
                                    "listw", "nb", "ppp", "fv"))) return(invisible(NULL))
    if (is.data.frame(x)) {
      if (nrow(x) == 1) {
        for (nm in names(x)) {
          valor <- x[[nm]]
          if (is.atomic(valor) && length(valor) == 1) {
            visitar(valor, paste(caminho, nm, sep = "."))
          }
        }
      }
      return(invisible(NULL))
    }
    if (is.atomic(x)) {
      if (length(x) == 1) {
        adicionar(caminho, x)
      } else if (length(x) <= 100 && !is.null(names(x))) {
        for (i in seq_along(x)) {
          nm <- names(x)[i]
          if (is.na(nm) || !nzchar(nm)) nm <- sprintf("item_%03d", i)
          adicionar(paste(caminho, gs_slug(nm), sep = "."), x[[i]])
        }
      }
      return(invisible(NULL))
    }
    if (is.list(x)) {
      nms <- names(x)
      if (is.null(nms)) nms <- rep("", length(x))
      for (i in seq_along(x)) {
        nm <- nms[i]
        if (!nzchar(nm)) nm <- sprintf("item_%03d", i)
        if (nm %in% c("objeto", "geometry", "geometria")) next
        visitar(x[[i]], paste(caminho, nm, sep = "."))
      }
    }
    invisible(NULL)
  }

  if (!is.null(analises)) {
    nms <- names(analises)
    if (is.null(nms)) nms <- paste0("analise_", seq_along(analises))
    for (i in seq_along(analises)) visitar(analises[[i]], gs_slug(nms[i]))
  }

  if (length(linhas) == 0) {
    return(data.frame(
      id_origem = character(0), analise = character(0), metrica = character(0),
      valor_numerico = numeric(0), valor_logico = logical(0),
      valor_texto = character(0), unidade = character(0),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, linhas)
  rownames(out) <- NULL
  out
}

gs_coletar_artefatos <- function(analises, dir, id_origem = "origem",
                                  salvar_figuras = TRUE, dpi = 200) {
  manifesto <- gs_manifesto_vazio()
  usados <- new.env(parent = emptyenv())

  reservar <- function(pasta, base, extensao) {
    base <- gs_slug(base)
    chave <- paste(pasta, base, extensao, sep = "|")
    n <- if (exists(chave, usados, inherits = FALSE)) get(chave, usados) + 1L else 1L
    assign(chave, n, usados)
    sufixo <- if (n == 1L) "" else sprintf("_%02d", n)
    file.path(pasta, paste0(base, sufixo, ".", extensao))
  }

  registrar <- function(categoria, caminho_item, formato, caminho,
                        status = "ok", mensagem = NA_character_) {
    partes <- strsplit(caminho_item, ".", fixed = TRUE)[[1]]
    manifesto <<- rbind(
      manifesto,
      gs_linha_manifesto(
        id_origem, categoria, partes[1], caminho_item, formato, caminho,
        status, mensagem
      )
    )
  }

  tentar <- function(expr, categoria, item, formato, caminho) {
    erro <- tryCatch({
      force(expr)
      NULL
    }, error = function(e) e)
    if (is.null(erro)) {
      registrar(categoria, item, formato, caminho)
      TRUE
    } else {
      registrar(categoria, item, formato, caminho, "erro", conditionMessage(erro))
      FALSE
    }
  }

  visitar <- function(x, caminho_item) {
    if (is.null(x)) return(invisible(NULL))
    nome <- gsub("[.]", "__", caminho_item)
    if (inherits(x, "ggplot")) {
      if (salvar_figuras) {
        caminho <- reservar(file.path(dir, "figuras"), nome, "png")
        tentar(gs_salvar_plot(x, caminho, dpi = dpi), "figura", caminho_item,
               "png", caminho)
      }
      return(invisible(NULL))
    }
    if (inherits(x, "sf")) {
      caminho_geo <- reservar(file.path(dir, "geometrias"), nome, "geojson")
      tentar({
        dir.create(dirname(caminho_geo), recursive = TRUE, showWarnings = FALSE)
        gs_gravar_atomico(caminho_geo, function(temporario) {
          sf::st_write(x, temporario, delete_dsn = TRUE, quiet = TRUE)
        })
      }, "geometria", caminho_item, "geojson", caminho_geo)
      if (salvar_figuras) {
        caminho_png <- reservar(file.path(dir, "figuras"), nome, "png")
        tentar(gs_salvar_plot(gs_plot_sf(x, caminho_item), caminho_png, dpi = dpi),
               "figura", caminho_item, "png", caminho_png)
      }
      return(invisible(NULL))
    }
    if (inherits(x, "table")) x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (is.data.frame(x)) {
      caminho <- reservar(file.path(dir, "tabelas"), nome, "csv")
      tentar(gs_escrever_csv_atomico(x, caminho), "tabela", caminho_item,
             "csv", caminho)
      return(invisible(NULL))
    }
    if (is.list(x)) {
      nms <- names(x)
      if (is.null(nms)) nms <- rep("", length(x))
      for (i in seq_along(x)) {
        nm <- nms[i]
        if (!nzchar(nm)) nm <- sprintf("item_%03d", i)
        if (nm %in% c("objeto", "geometry", "geometria")) next
        visitar(x[[i]], paste(caminho_item, nm, sep = "."))
      }
    }
    invisible(NULL)
  }

  if (!is.null(analises)) {
    nms <- names(analises)
    if (is.null(nms)) nms <- paste0("analise_", seq_along(analises))
    for (i in seq_along(analises)) visitar(analises[[i]], gs_slug(nms[i]))
  }
  rownames(manifesto) <- NULL
  manifesto
}

gs_metadados_consulta <- function(resultado, id_origem = "origem") {
  ponto <- attr(resultado, "ponto")
  valor_ponto <- function(nome) {
    if (is.list(ponto) && !is.null(ponto[[nome]])) ponto[[nome]][1] else NA
  }
  limite <- attr(resultado, "n_por_camada")
  data.frame(
    id_origem = as.character(id_origem),
    origem = as.character(valor_ponto("origem")),
    latitude = suppressWarnings(as.numeric(valor_ponto("latitude"))),
    longitude = suppressWarnings(as.numeric(valor_ponto("longitude"))),
    raio_m = suppressWarnings(as.numeric(attr(resultado, "raio_m"))[1]),
    tipo_distancia = as.character(attr(resultado, "tipo_distancia"))[1],
    backend_distancia = as.character(attr(resultado, "backend_distancia"))[1],
    n_por_camada = if (is.null(limite)) NA_integer_ else as.integer(limite)[1],
    amostra_truncada = isTRUE(attr(resultado, "amostra_truncada")),
    n_retido = nrow(resultado),
    stringsAsFactors = FALSE
  )
}

gs_amostragem_resultado <- function(resultado, id_origem = "origem") {
  amostragem <- attr(resultado, "amostragem_por_camada")
  if (!is.data.frame(amostragem)) {
    camadas <- if ("camada" %in% names(resultado)) table(resultado$camada) else integer(0)
    amostragem <- data.frame(
      camada = names(camadas), n_disponivel = as.integer(camadas),
      n_retido = as.integer(camadas), n_omitido = 0L,
      stringsAsFactors = FALSE
    )
  }
  amostragem$id_origem <- rep(as.character(id_origem), nrow(amostragem))
  amostragem[, c("id_origem", "camada", "n_disponivel", "n_retido", "n_omitido"),
             drop = FALSE]
}

gs_exportar_resultado <- function(resultado, analises = NULL, dir = "saidas",
                                   id_origem = "origem", salvar_figuras = TRUE,
                                   dpi = 200) {
  if (!is.data.frame(resultado)) stop("`resultado` deve ser um data.frame.")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  manifesto <- gs_manifesto_vazio()

  arq_servicos <- file.path(dir, "servicos_proximos.csv")
  gs_escrever_csv_atomico(sf::st_drop_geometry(resultado), arq_servicos)
  manifesto <- rbind(
    manifesto,
    gs_linha_manifesto(id_origem, "dados", "proximidade", "servicos_proximos",
                       "csv", arq_servicos)
  )

  metadados <- gs_metadados_consulta(resultado, id_origem)
  arq_metadados <- file.path(dir, "metadados_consulta.csv")
  gs_escrever_csv_atomico(metadados, arq_metadados)
  manifesto <- rbind(
    manifesto,
    gs_linha_manifesto(id_origem, "dados", "proximidade",
                       "metadados_consulta", "csv", arq_metadados)
  )

  amostragem <- gs_amostragem_resultado(resultado, id_origem)
  arq_amostragem <- file.path(dir, "amostragem_por_camada.csv")
  gs_escrever_csv_atomico(amostragem, arq_amostragem)
  manifesto <- rbind(
    manifesto,
    gs_linha_manifesto(id_origem, "dados", "proximidade",
                       "amostragem_por_camada", "csv", arq_amostragem)
  )

  if (!is.null(analises)) {
    manifesto <- rbind(
      manifesto,
      gs_coletar_artefatos(
        analises, dir, id_origem = id_origem,
        salvar_figuras = salvar_figuras, dpi = dpi
      )
    )
  }

  metricas <- gs_metricas_analises(analises, id_origem)
  arq_metricas <- file.path(dir, "metricas.csv")
  gs_escrever_csv_atomico(metricas, arq_metricas)
  manifesto <- rbind(
    manifesto,
    gs_linha_manifesto(id_origem, "metrica", "geral", "metricas", "csv",
                       arq_metricas)
  )

  ponto <- attr(resultado, "ponto")
  raio <- attr(resultado, "raio_m")
  interpretacoes <- tryCatch(
    gs_interpretar_analise(analises, resultado, raio),
    error = function(e) list()
  )
  tab_interpretacoes <- if (length(interpretacoes) == 0) {
    data.frame(analise = character(0), interpretacao = character(0))
  } else {
    data.frame(
      analise = names(interpretacoes),
      interpretacao = unlist(interpretacoes, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }
  arq_interpretacoes <- file.path(dir, "interpretacoes.csv")
  gs_escrever_csv_atomico(tab_interpretacoes, arq_interpretacoes)
  manifesto <- rbind(
    manifesto,
    gs_linha_manifesto(id_origem, "metrica", "geral", "interpretacoes",
                       "csv", arq_interpretacoes)
  )

  arq_manifesto <- file.path(dir, "manifesto.csv")
  manifesto <- rbind(
    manifesto,
    gs_linha_manifesto(id_origem, "manifesto", "geral", "manifesto", "csv",
                       arq_manifesto)
  )
  gs_escrever_csv_atomico(manifesto, arq_manifesto)
  message("Artefatos exportados em: ", normalizePath(dir, winslash = "/"))
  invisible(manifesto)
}

gs_md_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

gs_tab_md <- function(df) {
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return("_(sem dados)_")
  escapar <- function(x) {
    x <- gs_md_escape(x)
    x <- gsub("\\|", "\\\\|", x)
    gsub("[\r\n]+", " ", x)
  }
  c(
    paste(escapar(names(df)), collapse = " | "),
    paste(rep("---", ncol(df)), collapse = " | "),
    vapply(seq_len(nrow(df)), function(i) {
      paste(vapply(df[i, , drop = FALSE], function(x) escapar(x)[1],
                   character(1)), collapse = " | ")
    }, character(1))
  )
}

gs_tab_html <- function(df) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Pacote 'htmltools' não instalado para gerar relatório HTML.")
  }
  tags <- htmltools::tags
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(tags$p("(sem dados)"))
  cab <- tags$tr(lapply(names(df), tags$th))
  linhas <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(lapply(df[i, , drop = FALSE], function(x) tags$td(as.character(x))))
  })
  tags$table(class = "tabela", tags$thead(cab), tags$tbody(linhas))
}

gs_valor_metrica_texto <- function(metricas) {
  ifelse(
    !is.na(metricas$valor_numerico),
    format(metricas$valor_numerico, digits = 8, trim = TRUE),
    ifelse(!is.na(metricas$valor_logico), as.character(metricas$valor_logico),
           metricas$valor_texto)
  )
}

gs_falhas_analises <- function(analises) {
  falhas <- lapply(names(analises), function(nm) {
    x <- analises[[nm]]
    if (!is.list(x) || !identical(x$executado, FALSE)) return(NULL)
    mensagem <- if (is.null(x$mensagem) || length(x$mensagem) == 0) {
      "não executada"
    } else {
      as.character(x$mensagem[1])
    }
    data.frame(analise = nm, mensagem = mensagem, stringsAsFactors = FALSE)
  })
  falhas <- Filter(Negate(is.null), falhas)
  if (length(falhas) == 0) {
    return(data.frame(analise = character(0), mensagem = character(0)))
  }
  do.call(rbind, falhas)
}

gs_texto_limitacoes <- function() {
  paste0(
    "As distâncias descrevem o conjunto observado dentro da janela da consulta. ",
    "Intervalos e p-valores dependem das hipóteses de cada método e não ",
    "transformam dados administrativos em amostra aleatória. LISA e Getis-Ord ",
    "são exploratórios e usam ajuste Benjamini-Hochberg quando executados."
  )
}

gs_relatorio_numerico <- function(resultado, analises, arquivo,
                                   id_origem = "origem",
                                   arquivo_metricas = file.path(
                                     dirname(arquivo), "metricas.csv")) {
  metricas <- gs_metricas_analises(analises, id_origem)
  gs_escrever_csv_atomico(metricas, arquivo_metricas)
  ponto <- attr(resultado, "ponto")
  raio <- attr(resultado, "raio_m")
  origem <- if (is.list(ponto) && !is.null(ponto$origem)) ponto$origem else id_origem
  metadados <- gs_metadados_consulta(resultado, id_origem)
  amostragem <- gs_amostragem_resultado(resultado, id_origem)
  interpretacoes <- tryCatch(
    gs_interpretar_analise(analises, resultado, raio),
    error = function(e) list()
  )

  linhas <- c(
    "# Relatório numérico - GeoSampa", "",
    paste0("- Origem: ", gs_md_escape(origem)),
    paste0("- Raio da consulta: ", ifelse(is.null(raio), "não informado", raio), " m"),
    paste0("- Serviços encontrados: ", nrow(resultado)),
    paste0("- Distância: ", gs_md_escape(metadados$tipo_distancia),
           " (", gs_md_escape(metadados$backend_distancia), ")"),
    paste0("- Amostra truncada: ", metadados$amostra_truncada), "",
    "## Interpretação"
  )
  if (length(interpretacoes) == 0) {
    linhas <- c(linhas, "", "Nenhuma interpretação disponível.")
  } else {
    for (nm in names(interpretacoes)) {
      linhas <- c(linhas, "", paste0("### ", gs_md_escape(nm)), "",
                  gs_md_escape(interpretacoes[[nm]]))
    }
  }

  falhas <- gs_falhas_analises(analises)
  if (nrow(falhas) > 0) {
    linhas <- c(linhas, "", "## Análises não executadas", "",
                gs_tab_md(falhas))
  }

  if (nrow(amostragem) > 0) {
    linhas <- c(linhas, "", "## Amostragem por camada", "",
                gs_tab_md(amostragem[, setdiff(names(amostragem), "id_origem")]))
  }

  linhas <- c(linhas, "", "## Métricas", "")
  if (nrow(metricas) == 0) {
    linhas <- c(linhas, "Nenhuma métrica escalar disponível.")
  } else {
    tabela <- metricas[, c("analise", "metrica", "unidade"), drop = FALSE]
    tabela$valor <- gs_valor_metrica_texto(metricas)
    tabela <- tabela[, c("analise", "metrica", "valor", "unidade")]
    linhas <- c(linhas, gs_tab_md(tabela))
  }
  linhas <- c(
    linhas, "", "## Limitações", "",
    gs_texto_limitacoes()
  )
  gs_escrever_texto_atomico(linhas, arquivo)
  invisible(list(relatorio = arquivo, metricas = arquivo_metricas,
                 dados = metricas))
}

gs_caminho_relativo <- function(caminho, base) {
  caminho <- normalizePath(caminho, winslash = "/", mustWork = FALSE)
  base <- normalizePath(base, winslash = "/", mustWork = FALSE)
  prefixo <- paste0(base, "/")
  if (startsWith(caminho, prefixo)) substring(caminho, nchar(prefixo) + 1L) else caminho
}

gs_relatorio_analises <- function(resultado = NULL, cep = NULL,
                                   coordenadas = NULL, camadas = NULL,
                                   raio_m = gs_raio_padrao_m,
                                   n_por_camada = NULL,
                                   tipo_distancia = c("geodesica", "euclidiana",
                                                      "haversine", "manhattan",
                                                      "rede_viaria"),
                                   tipo = NULL,
                                   arquivo = file.path("relatorios", "relatorio_analises.html"),
                                   formato = c("html", "md"),
                                   pop_layer = NULL, densidade_km2 = NULL,
                                   analises = NULL, id_origem = "origem",
                                   manifesto = NULL, dir = gs_caminho_dados()) {
  formato <- match.arg(formato)
  if (is.null(tipo)) {
    tipo <- c("descritivas", "raios_progressivos", "acessibilidade_media",
              "raio_otimo", "nni")
  }
  if (is.null(resultado)) {
    resultado <- gs_servicos_proximos(
      cep = cep, coordenadas = coordenadas, camadas = camadas,
      raio_m = raio_m, n_por_camada = n_por_camada,
      tipo_distancia = tipo_distancia, dir = dir
    )
  }
  if (is.null(analises)) {
    analises <- gs_analise_servicos(
      resultado, tipo = tipo, pop_layer = pop_layer,
      densidade_km2 = densidade_km2, dir = dir
    )
  }
  if (is.null(manifesto)) {
    pasta_artefatos <- file.path(
      dirname(arquivo), paste0(tools::file_path_sans_ext(basename(arquivo)), "_artefatos")
    )
    manifesto <- gs_exportar_resultado(
      resultado, analises, pasta_artefatos, id_origem = id_origem
    )
  }

  ponto <- attr(resultado, "ponto")
  raio <- attr(resultado, "raio_m")
  origem <- if (is.list(ponto) && !is.null(ponto$origem)) ponto$origem else id_origem
  metadados <- gs_metadados_consulta(resultado, id_origem)
  amostragem <- gs_amostragem_resultado(resultado, id_origem)
  interpretacoes <- tryCatch(
    gs_interpretar_analise(analises, resultado, raio),
    error = function(e) list()
  )
  metricas <- gs_metricas_analises(analises, id_origem)
  falhas <- gs_falhas_analises(analises)
  artefatos <- manifesto[manifesto$status == "ok" &
                           manifesto$categoria %in% c("figura", "tabela", "geometria"),
                         , drop = FALSE]
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)

  if (formato == "md") {
    linhas <- c(
      "# Relatório de análises - GeoSampa", "",
      paste0("**Origem:** ", gs_md_escape(origem), "  "),
      paste0("**Raio:** ", raio, " m  "),
      paste0("**Serviços encontrados:** ", nrow(resultado), "  "),
      paste0("**Distância:** ", gs_md_escape(metadados$tipo_distancia),
             " (", gs_md_escape(metadados$backend_distancia), ")  "),
      paste0("**Amostra truncada:** ", metadados$amostra_truncada), "",
      "## Síntese"
    )
    if (length(interpretacoes) == 0) {
      linhas <- c(linhas, "", "Nenhuma interpretação disponível.")
    } else {
      for (nm in names(interpretacoes)) {
        linhas <- c(linhas, "", paste0("### ", gs_md_escape(nm)), "",
                    gs_md_escape(interpretacoes[[nm]]))
      }
    }
    if (nrow(falhas) > 0) {
      linhas <- c(linhas, "", "## Análises não executadas", "",
                  gs_tab_md(falhas))
    }
    if (nrow(metricas) > 0) {
      tab <- metricas[, c("analise", "metrica", "unidade"), drop = FALSE]
      tab$valor <- gs_valor_metrica_texto(metricas)
      linhas <- c(linhas, "", "## Métricas", "",
                  gs_tab_md(tab[, c("analise", "metrica", "valor", "unidade")]))
    }
    if (nrow(amostragem) > 0) {
      linhas <- c(linhas, "", "## Amostragem por camada", "",
                  gs_tab_md(amostragem[, setdiff(names(amostragem), "id_origem")]))
    }
    if (nrow(artefatos) > 0) {
      linhas <- c(linhas, "", "## Artefatos", "")
      for (i in seq_len(nrow(artefatos))) {
        relativo <- gs_caminho_relativo(artefatos$caminho[i], dirname(arquivo))
        if (artefatos$categoria[i] == "figura") {
          linhas <- c(linhas, paste0("### ", gs_md_escape(artefatos$item[i])), "",
                      paste0("![", gs_md_escape(artefatos$item[i]), "](", relativo, ")"), "")
        } else {
          linhas <- c(linhas, paste0("- [", gs_md_escape(artefatos$item[i]),
                                      "](", relativo, ")"))
        }
      }
    }
    linhas <- c(linhas, "", "## Limitações", "", gs_texto_limitacoes())
    gs_escrever_texto_atomico(linhas, arquivo)
  } else {
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      stop("Pacote 'htmltools' não instalado para gerar relatório HTML.")
    }
    tags <- htmltools::tags
    secoes <- list(
      tags$h1("Relatório de análises - GeoSampa"),
      tags$p(sprintf(
        paste0("Origem: %s | Raio: %s m | Serviços: %d | Distância: %s ",
               "(%s) | Amostra truncada: %s"),
        origem, raio, nrow(resultado), metadados$tipo_distancia,
        metadados$backend_distancia, metadados$amostra_truncada
      )),
      tags$h2("Síntese")
    )
    if (length(interpretacoes) == 0) {
      secoes[[length(secoes) + 1L]] <- tags$p("Nenhuma interpretação disponível.")
    } else {
      for (nm in names(interpretacoes)) {
        secoes[[length(secoes) + 1L]] <- tags$h3(nm)
        secoes[[length(secoes) + 1L]] <- tags$p(class = "interpretacao",
                                                interpretacoes[[nm]])
      }
    }
    if (nrow(falhas) > 0) {
      secoes[[length(secoes) + 1L]] <- tags$h2("Análises não executadas")
      secoes[[length(secoes) + 1L]] <- gs_tab_html(falhas)
    }
    if (nrow(metricas) > 0) {
      tab <- metricas[, c("analise", "metrica", "unidade"), drop = FALSE]
      tab$valor <- gs_valor_metrica_texto(metricas)
      secoes[[length(secoes) + 1L]] <- tags$h2("Métricas")
      secoes[[length(secoes) + 1L]] <- gs_tab_html(
        tab[, c("analise", "metrica", "valor", "unidade")]
      )
    }
    if (nrow(amostragem) > 0) {
      secoes[[length(secoes) + 1L]] <- tags$h2("Amostragem por camada")
      secoes[[length(secoes) + 1L]] <- gs_tab_html(
        amostragem[, setdiff(names(amostragem), "id_origem")]
      )
    }
    if (nrow(artefatos) > 0) {
      secoes[[length(secoes) + 1L]] <- tags$h2("Artefatos")
      for (i in seq_len(nrow(artefatos))) {
        relativo <- gs_caminho_relativo(artefatos$caminho[i], dirname(arquivo))
        secoes[[length(secoes) + 1L]] <- tags$h3(artefatos$item[i])
        secoes[[length(secoes) + 1L]] <- if (artefatos$categoria[i] == "figura") {
          tags$div(class = "figura", tags$img(src = relativo, alt = artefatos$item[i]))
        } else {
          tags$p(tags$a(href = relativo, "Abrir arquivo"))
        }
      }
    }
    secoes[[length(secoes) + 1L]] <- tags$h2("Limitações")
    secoes[[length(secoes) + 1L]] <- tags$p(gs_texto_limitacoes())
    doc <- tags$html(
      tags$head(
        tags$meta(charset = "utf-8"), tags$title("Relatório GeoSampa"),
        tags$style(htmltools::HTML(paste0(
          "body{font-family:sans-serif;margin:2em;max-width:1100px}",
          "table.tabela{border-collapse:collapse;margin:1em 0}",
          "table.tabela th,table.tabela td{border:1px solid #ccc;padding:6px 10px;font-size:.9em}",
          "table.tabela th{background:#f0f0f0}",
          ".figura img{max-width:100%;border:1px solid #ddd}",
          ".interpretacao{background:#f2f7fc;border-left:4px solid #2c7fb8;padding:8px 12px}"
        )))
      ),
      tags$body(secoes)
    )
    gs_gravar_atomico(arquivo, function(temporario) {
      writeLines(c("<!DOCTYPE html>", as.character(doc)), temporario,
                 useBytes = TRUE)
    })
  }
  message("Relatório salvo em: ", arquivo)
  invisible(arquivo)
}

gs_salvar_analises <- function(resultado, analises, dir,
                                id_origem = "origem",
                                formato_relatorio = c("md", "html", "ambos", "nenhum"),
                                salvar_mapa = TRUE, largura = 10, altura = 8,
                                dpi = 200, sobrescrever = FALSE) {
  formato_relatorio <- match.arg(formato_relatorio)
  if (!is.logical(sobrescrever) || length(sobrescrever) != 1 || is.na(sobrescrever)) {
    stop("`sobrescrever` deve ser TRUE ou FALSE.")
  }
  if (dir.exists(dir) && length(list.files(dir, all.files = TRUE, no.. = TRUE)) > 0 &&
      !sobrescrever) {
    stop("Diretório de saída já contém arquivos: ", dir,
         ". Use `sobrescrever = TRUE` ou escolha outro diretório.")
  }
  if (dir.exists(dir) && sobrescrever) {
    unlink(dir, recursive = TRUE, force = TRUE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  manifesto <- gs_exportar_resultado(
    resultado, analises, dir, id_origem = id_origem,
    salvar_figuras = TRUE, dpi = dpi
  )

  if (salvar_mapa && nrow(resultado) > 0) {
    caminho_mapa <- file.path(dir, "figuras", "mapa_servicos.png")
    erro <- tryCatch({
      gs_mapa_servicos(
        resultado, interativo = FALSE, salvar = caminho_mapa,
        largura = largura, altura = altura, dpi = dpi
      )
      NULL
    }, error = function(e) e)
    manifesto <- rbind(
      manifesto,
      gs_linha_manifesto(
        id_origem, "figura", "proximidade", "mapa_servicos", "png",
        caminho_mapa, if (is.null(erro)) "ok" else "erro",
        if (is.null(erro)) NA_character_ else conditionMessage(erro)
      )
    )
  }

  numerico <- gs_relatorio_numerico(
    resultado, analises, file.path(dir, "relatorio_numerico.md"),
    id_origem = id_origem, arquivo_metricas = file.path(dir, "metricas.csv")
  )
  manifesto <- rbind(
    manifesto,
    gs_linha_manifesto(id_origem, "relatorio", "geral", "relatorio_numerico",
                       "md", numerico$relatorio)
  )

  relatorios <- character(0)
  formatos <- switch(
    formato_relatorio,
    ambos = c("md", "html"), nenhum = character(0), formato_relatorio
  )
  for (formato in formatos) {
    caminho <- file.path(dir, paste0("relatorio_analises.", formato))
    gs_relatorio_analises(
      resultado = resultado, analises = analises, arquivo = caminho,
      formato = formato, id_origem = id_origem, manifesto = manifesto
    )
    relatorios <- c(relatorios, caminho)
    manifesto <- rbind(
      manifesto,
      gs_linha_manifesto(id_origem, "relatorio", "geral", "relatorio_analises",
                         formato, caminho)
    )
  }
  gs_escrever_csv_atomico(manifesto, file.path(dir, "manifesto.csv"))
  invisible(list(
    pasta = normalizePath(dir, winslash = "/"), manifesto = manifesto,
    metricas = numerico$dados, relatorios = relatorios
  ))
}
