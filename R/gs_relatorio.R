# ============================================================
# GeoSampa — Relatório e exportação dos resultados
# ------------------------------------------------------------
# Gera um relatório consolidado (HTML auto-contido ou Markdown)
# com as análises escolhidas, e exporta tabelas/polígonos em
# CSV/GeoJSON. Não exige rmarkdown/pandoc: o HTML embute as
# figuras em base64 (dependência: htmltools + base64enc).
# ============================================================

tags <- htmltools::tags

# --- Exporta os resultados de uma análise para CSV/GeoJSON --------------------
# `resultado` é o data.frame de gs_servicos_proximos(); `analises` é a lista
# devolvida por gs_analise_servicos(). Devolve os caminhos criados.
gs_exportar_resultado <- function(resultado, analises = NULL, dir = "saidas") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  caminhos <- character(0)
  arq_csv <- file.path(dir, "servicos_proximos.csv")
  readr::write_csv(resultado, arq_csv)
  caminhos <- c(caminhos, arq_csv)

  if (!is.null(analises)) {
    for (nm in names(analises)) {
      a <- analises[[nm]]
      if (is.null(a)) next
      if (is.data.frame(a)) {
        arq <- file.path(dir, paste0("analise_", nm, ".csv"))
        readr::write_csv(a, arq)
        caminhos <- c(caminhos, arq)
      } else if (inherits(a, "sf")) {
        arq <- file.path(dir, paste0("analise_", nm, ".geojson"))
        sf::st_write(a, arq, delete_dsn = TRUE, quiet = TRUE)
        caminhos <- c(caminhos, arq)
      } else if (is.list(a)) {
        for (sn in names(a)) {
          sub <- a[[sn]]
          if (is.data.frame(sub)) {
            arq <- file.path(dir, paste0("analise_", nm, "_", sn, ".csv"))
            readr::write_csv(sub, arq)
            caminhos <- c(caminhos, arq)
          } else if (inherits(sub, "sf")) {
            arq <- file.path(dir, paste0("analise_", nm, "_", sn, ".geojson"))
            sf::st_write(sub, arq, delete_dsn = TRUE, quiet = TRUE)
            caminhos <- c(caminhos, arq)
          }
        }
      }
    }
  }
  message("Arquivos exportados em: ", dir)
  invisible(caminhos)
}

# --- Helpers internos ----------------------------------------------------------
gs_salvar_fig_tmp <- function(p, nome) {
  arq <- tempfile(pattern = paste0("gs_", gsub("[^A-Za-z0-9]", "_", nome), "_"),
                  fileext = ".png")
  ggplot2::ggsave(arq, plot = p, width = 10, height = 8, dpi = 200)
  arq
}

gs_fig_b64 <- function(p, nome) {
  arq <- gs_salvar_fig_tmp(p, nome)
  on.exit(unlink(arq), add = TRUE)
  paste0("data:image/png;base64,",
         base64enc::base64encode(readBin(arq, "raw", n = file.info(arq)$size)))
}

gs_plot_sf <- function(sf_obj, titulo) {
  ggplot2::ggplot(sf_obj) +
    ggplot2::geom_sf(fill = "#41b6c4", color = "white", linewidth = 0.15,
                     alpha = 0.7) +
    gs_tema_mapa() +
    ggplot2::labs(title = titulo)
}

gs_tab_html <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(tags$p("(sem dados)"))
  cab <- tags$tr(lapply(names(df), tags$th))
  linhas <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(lapply(df[i, , drop = FALSE],
                   function(x) tags$td(as.character(x))))
  })
  htmltools::tags$table(class = "tabela",
                        htmltools::tags$thead(cab),
                        htmltools::tags$tbody(linhas))
}

gs_tab_md <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return("_(sem dados)_")
  c(
    paste(names(df), collapse = " | "),
    paste(rep("---", ncol(df)), collapse = " | "),
    vapply(seq_len(nrow(df)), function(i) {
      paste(vapply(df[i, , drop = FALSE], function(x) as.character(x),
                   character(1)), collapse = " | ")
    }, character(1))
  )
}

# --- Gera o relatório consolidado ----------------------------------------------
# `tipo`: mesmas opções de gs_analise_servicos(). Se não informado, usa um
# conjunto padrão de análises leves.
# `formato`: "html" (auto-contido, figuras embutidas) ou "md" (Markdown).
# Para `cobertura_populacional`, aceita `pop_layer` (sf) ou `densidade_km2`.
gs_relatorio_analises <- function(resultado = NULL, cep = NULL, coordenadas = NULL,
                                  camadas = NULL, raio_m = gs_raio_padrao_m,
                                  n_por_camada = NULL,
                                  tipo_distancia = c("geodesica", "euclidiana",
                                                     "haversine", "manhattan",
                                                     "rede_viaria"),
                                  tipo = NULL,
                                  arquivo = file.path("relatorios", "relatorio_analises.html"),
                                  formato = c("html", "md"),
                                  pop_layer = NULL, densidade_km2 = NULL) {
  formato <- match.arg(formato)
  if (is.null(tipo)) {
    tipo <- c("descritivas", "raios_progressivos", "acessibilidade_media",
              "raio_otimo", "nni")
  }
  if (is.null(resultado)) {
    resultado <- gs_servicos_proximos(
      cep = cep, coordenadas = coordenadas, camadas = camadas,
      raio_m = raio_m, n_por_camada = n_por_camada,
      tipo_distancia = tipo_distancia
    )
  }
  ponto <- attr(resultado, "ponto")
  raio <- attr(resultado, "raio_m")
  if (is.null(raio)) raio <- raio_m

  analises <- gs_analise_servicos(resultado, tipo = setdiff(tipo, "cobertura_populacional"))
  if ("cobertura_populacional" %in% tipo) {
    analises$cobertura_populacional <-
      gs_analise_cobertura_populacional(resultado, ponto, raio,
                                        pop_layer = pop_layer,
                                        densidade_km2 = densidade_km2)
  }

  # Pasta de figuras (usada no formato Markdown)
  pasta_figuras <- NULL
  if (formato == "md") {
    pasta_figuras <- file.path(dirname(arquivo), "assets")
    dir.create(pasta_figuras, showWarnings = FALSE, recursive = TRUE)
  }

  # Monta a lista de "seções" (título, parágrafos, tabelas e figuras)
  secoes <- list()
  secoes[[length(secoes) + 1]] <- if (formato == "html") tags$h1("Relatório de análises — GeoSampa") else "# Relatório de análises — GeoSampa"
  secoes[[length(secoes) + 1]] <- if (formato == "html") {
    tags$p(sprintf("Ponto: %s | Raio: %s m | Serviços encontrados: %d | Fonte: Prefeitura de São Paulo / GeoSampa",
                   ponto$origem, raio, nrow(resultado)))
  } else {
    sprintf("_Ponto: %s | Raio: %s m | Serviços encontrados: %d | Fonte: Prefeitura de São Paulo / GeoSampa_",
            ponto$origem, raio, nrow(resultado))
  }

  for (nm in names(analises)) {
    a <- analises[[nm]]
    secoes[[length(secoes) + 1]] <-
      if (formato == "html") tags$h2(nm) else paste0("\n## ", nm, "\n")
    if (is.null(a)) next
    if (is.data.frame(a)) {
      secoes <- c(secoes, if (formato == "html") gs_tab_html(a) else gs_tab_md(a))
    } else if (inherits(a, "sf")) {
      secoes <- c(secoes, gs_secao_figura(a, nm, formato, pasta_figuras))
    } else if (inherits(a, "ggplot")) {
      secoes <- c(secoes, gs_secao_figura(a, nm, formato, pasta_figuras))
    } else if (is.list(a)) {
      for (sn in names(a)) {
        sub <- a[[sn]]
        if (is.data.frame(sub)) {
          secoes[[length(secoes) + 1]] <-
            if (formato == "html") tags$h3(sn) else paste0("\n### ", sn, "\n")
          secoes <- c(secoes, if (formato == "html") gs_tab_html(sub) else gs_tab_md(sub))
        } else if (inherits(sub, "sf")) {
          secoes <- c(secoes, gs_secao_figura(sub, paste0(nm, "_", sn), formato, pasta_figuras))
        } else if (inherits(sub, "ggplot")) {
          secoes <- c(secoes, gs_secao_figura(sub, paste0(nm, "_", sn), formato, pasta_figuras))
        } else if (inherits(sub, "table")) {
          df <- data.frame(item = names(sub), valor = as.vector(sub),
                           stringsAsFactors = FALSE)
          secoes <- c(secoes, if (formato == "html") gs_tab_html(df) else gs_tab_md(df))
        } else if (is.character(sub) && length(sub) == 1) {
          secoes <- c(secoes, if (formato == "html") tags$p(sub) else sub)
        } else if (is.atomic(sub) && length(sub) == 1 && !is.factor(sub)) {
          df <- data.frame(item = sn, valor = as.character(sub),
                           stringsAsFactors = FALSE)
          secoes <- c(secoes, if (formato == "html") gs_tab_html(df) else gs_tab_md(df))
        }
      }
    }
    # Parágrafo de interpretação automática (leitura dos resultados)
    interp <- gs_interpretar_analise(analises, resultado, raio)[[nm]]
    if (!is.null(interp)) {
      secoes[[length(secoes) + 1]] <-
        if (formato == "html") {
          tags$p(class = "interpretacao", interp)
        } else {
          paste0("\n> ", interp, "\n")
        }
    }
  }

  if (formato == "html") {
    doc <- htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$meta(charset = "utf-8"),
        htmltools::tags$title("Relatório de análises — GeoSampa"),
        htmltools::tags$style(paste0(
          "body{font-family:sans-serif;margin:2em;max-width:1000px}",
          "table.tabela{border-collapse:collapse;margin:1em 0}",
          "table.tabela th,table.tabela td{border:1px solid #ccc;padding:6px 10px;font-size:0.9em}",
          "table.tabela th{background:#f0f0f0}",
          ".figura img{max-width:100%;border:1px solid #ddd;margin:.5em 0}",
          "h2{border-bottom:2px solid #2c7fb8;padding-bottom:4px;margin-top:2em}",
          "h3{color:#2c7fb8}",
          "p.interpretacao{background:#f2f7fc;border-left:4px solid #2c7fb8;padding:8px 12px;border-radius:3px;color:#333;line-height:1.5}")
        )
      ),
      htmltools::tags$body(secoes)
    )
    dir.create(dirname(arquivo), showWarnings = FALSE, recursive = TRUE)
    htmltools::save_html(doc, file = arquivo)
  } else {
    texto <- vapply(secoes, function(s) {
      if (is.character(s)) paste(s, collapse = "\n") else ""
    }, character(1))
    writeLines(texto, arquivo, useBytes = TRUE)
  }
  message("Relatório salvo em: ", arquivo)
  invisible(arquivo)
}

# --- Anexa uma figura (ggplot/sf) à lista de seções ----------------------------
# No HTML embute em base64; no MD salva PNG em assets/ e referencia por link.
gs_secao_figura <- function(obj, nome, formato, pasta_figuras = NULL) {
  if (inherits(obj, "sf")) obj <- gs_plot_sf(obj, nome)
  if (formato == "html") {
    list(htmltools::tags$div(class = "figura",
                             htmltools::tags$img(src = gs_fig_b64(obj, nome),
                                                 alt = nome)))
  } else {
    dir.create(pasta_figuras, showWarnings = FALSE, recursive = TRUE)
    arq <- file.path(pasta_figuras, paste0(nome, ".png"))
    ggplot2::ggsave(arq, plot = obj, width = 10, height = 8, dpi = 200)
    paste0("\n![", nome, "](assets/", basename(arq), ")\n")
  }
}
