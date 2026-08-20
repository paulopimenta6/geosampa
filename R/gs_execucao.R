# ============================================================
# GeoSampa - execução reproduzível para múltiplas origens
# ============================================================

gs_ids_unicos <- function(ids) {
  usados <- new.env(parent = emptyenv())
  vapply(ids, function(id) {
    base <- gs_slug(id, padrao = "origem")
    n <- if (exists(base, usados, inherits = FALSE)) get(base, usados) + 1L else 1L
    assign(base, n, usados)
    if (n == 1L) base else paste0(base, "__", sprintf("%02d", n))
  }, character(1), USE.NAMES = FALSE)
}

gs_normalizar_origens <- function(cep = NULL, coordenadas = NULL, ids = NULL) {
  partes <- list()
  ids_sugeridos <- character(0)

  if (!is.null(cep)) {
    if (!is.atomic(cep) || length(cep) == 0) {
      stop("`cep` deve ser um vetor com um ou mais CEPs.")
    }
    ceps <- vapply(cep, gs_normalizar_cep, character(1))
    nomes <- names(cep)
    if (is.null(nomes)) nomes <- rep("", length(ceps))
    nomes[is.na(nomes)] <- ""
    sugeridos <- ifelse(nzchar(nomes), nomes, paste0("cep_", ceps))
    partes[[length(partes) + 1L]] <- data.frame(
      tipo_origem = "cep", cep = vapply(ceps, gs_cep_mascarado, character(1)),
      latitude = NA_real_, longitude = NA_real_,
      rotulo = paste("CEP", vapply(ceps, gs_cep_mascarado, character(1))),
      stringsAsFactors = FALSE
    )
    ids_sugeridos <- c(ids_sugeridos, sugeridos)
  }

  if (!is.null(coordenadas)) {
    tab <- NULL
    if (is.numeric(coordenadas) && is.null(dim(coordenadas)) &&
        length(coordenadas) == 2) {
      tab <- data.frame(latitude = coordenadas[1], longitude = coordenadas[2])
    } else if (is.matrix(coordenadas) || is.data.frame(coordenadas)) {
      tab <- as.data.frame(coordenadas, stringsAsFactors = FALSE)
      nomes_lower <- tolower(names(tab))
      i_lat <- match(TRUE, nomes_lower %in% c("latitude", "lat"), nomatch = 0)
      i_lon <- match(TRUE, nomes_lower %in% c("longitude", "lon", "lng"), nomatch = 0)
      if (i_lat == 0 || i_lon == 0) {
        if (ncol(tab) < 2) {
          stop("`coordenadas` precisa de colunas latitude e longitude.")
        }
        i_lat <- 1L
        i_lon <- 2L
      }
      para_numero <- function(x) {
        if (is.factor(x)) x <- as.character(x)
        suppressWarnings(as.numeric(x))
      }
      tab$latitude <- para_numero(tab[[i_lat]])
      tab$longitude <- para_numero(tab[[i_lon]])
    } else {
      stop(paste0(
        "`coordenadas` deve ser c(latitude, longitude), uma matriz ou um ",
        "data.frame com latitude e longitude."
      ))
    }
    if (nrow(tab) == 0) stop("`coordenadas` não contém linhas.")
    for (i in seq_len(nrow(tab))) {
      validada <- gs_validar_coordenadas(c(tab$latitude[i], tab$longitude[i]))
      tab$latitude[i] <- validada[1]
      tab$longitude[i] <- validada[2]
    }
    nomes_lower <- tolower(names(tab))
    i_id <- match(TRUE, nomes_lower %in% c("id", "id_origem", "nome"), nomatch = 0)
    sugeridos <- if (i_id > 0) as.character(tab[[i_id]]) else {
      rn <- rownames(tab)
      if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(tab))))) rn else
        sprintf("coord_%03d", seq_len(nrow(tab)))
    }
    sugeridos[is.na(sugeridos) | !nzchar(sugeridos)] <-
      sprintf("coord_%03d", which(is.na(sugeridos) | !nzchar(sugeridos)))
    partes[[length(partes) + 1L]] <- data.frame(
      tipo_origem = "coordenadas", cep = NA_character_,
      latitude = tab$latitude, longitude = tab$longitude,
      rotulo = sprintf("Coordenadas %.6f, %.6f", tab$latitude, tab$longitude),
      stringsAsFactors = FALSE
    )
    ids_sugeridos <- c(ids_sugeridos, sugeridos)
  }

  if (length(partes) == 0) {
    stop("Informe pelo menos um CEP ou conjunto de coordenadas.")
  }
  origens <- do.call(rbind, partes)
  rownames(origens) <- NULL
  if (!is.null(ids)) {
    if (length(ids) != nrow(origens) || anyNA(ids) || any(!nzchar(as.character(ids)))) {
      stop("`ids` deve ter um identificador não vazio para cada origem.")
    }
    ids_sugeridos <- as.character(ids)
  }
  origens$id_origem <- gs_ids_unicos(ids_sugeridos)
  origens$status <- "pendente"
  origens$mensagem <- NA_character_
  origens[, c("id_origem", "tipo_origem", "cep", "latitude", "longitude",
              "rotulo", "status", "mensagem")]
}

gs_resumo_origem <- function(resultado, id_origem) {
  distancias <- gs_distancias_resultado(resultado)
  distancias <- distancias[is.finite(distancias)]
  if (length(distancias) == 0) {
    return(data.frame(
      id_origem = id_origem, n_servicos = 0L, n_camadas = 0L,
      distancia_min_m = NA_real_, p25_m = NA_real_, mediana_m = NA_real_,
      media_m = NA_real_, p75_m = NA_real_, p90_m = NA_real_,
      distancia_max_m = NA_real_, stringsAsFactors = FALSE
    ))
  }
  q <- stats::quantile(distancias, c(0.25, 0.5, 0.75, 0.9), names = FALSE)
  data.frame(
    id_origem = id_origem, n_servicos = length(distancias),
    n_camadas = length(unique(resultado$camada)),
    distancia_min_m = min(distancias), p25_m = q[1], mediana_m = q[2],
    media_m = mean(distancias), p75_m = q[3], p90_m = q[4],
    distancia_max_m = max(distancias), stringsAsFactors = FALSE
  )
}

gs_plots_comparacao <- function(comparacao, servicos) {
  if (!is.data.frame(comparacao) || nrow(comparacao) == 0) return(list())
  comparacao$id_origem <- factor(
    comparacao$id_origem,
    levels = comparacao$id_origem[order(comparacao$n_servicos)]
  )
  contagens <- ggplot2::ggplot(
    comparacao, ggplot2::aes(x = id_origem, y = n_servicos)
  ) +
    ggplot2::geom_col(fill = "#2c7fb8", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = n_servicos), hjust = -0.15,
                       size = 3.8) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    gs_tema_mapa() +
    ggplot2::labs(
      title = "Serviços encontrados por origem",
      x = NULL, y = "Número de serviços",
      caption = "Mesmo raio e seleção de camadas para todas as origens"
    )
  plots <- list(contagens = contagens)

  if (is.data.frame(servicos) && nrow(servicos) > 0) {
    servicos$id_origem <- factor(servicos$id_origem,
                                  levels = levels(comparacao$id_origem))
    coluna_distancia <- if ("distancia_m_exata" %in% names(servicos)) {
      "distancia_m_exata"
    } else {
      "distancia_m"
    }
    plots$distancias <- ggplot2::ggplot(
      servicos, ggplot2::aes(x = id_origem, y = .data[[coluna_distancia]])
    ) +
      ggplot2::geom_boxplot(fill = "#41b6c4", outlier.alpha = 0.35) +
      ggplot2::stat_summary(fun = stats::median, geom = "point", shape = 18,
                            size = 3, color = "#d7301f") +
      ggplot2::coord_flip() +
      gs_tema_mapa() +
      ggplot2::labs(
        title = "Distribuição das distâncias por origem",
        x = NULL, y = "Distância ao serviço (m)",
        caption = "Losango vermelho: mediana"
      )
  }
  plots
}

gs_relatorio_lote <- function(origens, comparacao, arquivo, figuras = character(0)) {
  linhas <- c(
    "# Relatório comparativo de múltiplas origens - GeoSampa", "",
    paste0("Origens processadas: ", nrow(origens)), "",
    "## Situação das origens", "", gs_tab_md(origens), "",
    "## Comparação numérica", "", gs_tab_md(comparacao), ""
  )
  if (length(figuras) > 0) {
    linhas <- c(linhas, "## Figuras", "")
    for (figura in figuras) {
      relativo <- gs_caminho_relativo(figura, dirname(arquivo))
      linhas <- c(linhas, paste0("![", tools::file_path_sans_ext(basename(figura)),
                                  "](", relativo, ")"), "")
    }
  }
  linhas <- c(
    linhas, "## Interpretação", "",
    paste0(
      "As comparações são descritivas: cada linha representa o conjunto de ",
      "serviços registrado para uma origem sob os mesmos filtros. Diferenças ",
      "de contagem e distância não devem ser interpretadas como efeitos causais ",
      "nem como testes de hipótese entre bairros."
    )
  )
  gs_escrever_texto_atomico(linhas, arquivo)
  invisible(arquivo)
}

gs_analisar_locais <- function(
    cep = NULL, coordenadas = NULL, ids = NULL, camadas = NULL,
    raio_m = gs_raio_padrao_m, n_por_camada = NULL,
    tipo_distancia = c("geodesica", "euclidiana", "haversine", "manhattan",
                       "rede_viaria"),
    tipo = c("descritivas", "vizinho_mais_proximo", "acessibilidade_media",
             "raios_progressivos", "raio_otimo", "cobertura_buffer", "nni",
             "moran", "getis_ord", "lisa"),
    dir = gs_caminho_dados(), dir_saida = file.path(gs_raiz(), "saidas"),
    nome_execucao = NULL,
    formato_relatorio = c("md", "html", "ambos", "nenhum"),
    salvar_mapa = TRUE, continuar_em_erro = TRUE, sobrescrever = FALSE,
    pop_layer = NULL, densidade_km2 = NULL, dpi = 200) {
  tipo_distancia <- match.arg(tipo_distancia)
  formato_relatorio <- match.arg(formato_relatorio)
  raio_m <- gs_validar_numero_escalar(
    raio_m, "raio_m", minimo = 0, minimo_inclusivo = FALSE
  )
  if (!is.logical(continuar_em_erro) || length(continuar_em_erro) != 1 ||
      is.na(continuar_em_erro)) {
    stop("`continuar_em_erro` deve ser TRUE ou FALSE.")
  }
  origens <- gs_normalizar_origens(cep, coordenadas, ids)
  nome <- if (is.null(nome_execucao)) {
    paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "_analise_locais")
  } else {
    gs_slug(nome_execucao, padrao = "analise_locais")
  }
  pasta <- file.path(dir_saida, nome)
  if (dir.exists(pasta) && length(list.files(pasta, all.files = TRUE, no.. = TRUE)) > 0 &&
      !sobrescrever) {
    stop("A execução já existe: ", pasta,
         ". Use `sobrescrever = TRUE` ou outro `nome_execucao`.")
  }
  if (dir.exists(pasta) && sobrescrever) {
    unlink(pasta, recursive = TRUE, force = TRUE)
  }
  dir.create(file.path(pasta, "origens"), recursive = TRUE, showWarnings = FALSE)

  resultados <- vector("list", nrow(origens))
  names(resultados) <- origens$id_origem
  servicos_lista <- list()
  metricas_lista <- list()
  comparacoes <- list()
  manifestos <- list()

  for (i in seq_len(nrow(origens))) {
    origem <- origens[i, , drop = FALSE]
    id <- origem$id_origem
    message("[", i, "/", nrow(origens), "] Analisando ", id, "...")
    pasta_origem <- file.path(pasta, "origens", id)
    erro <- tryCatch({
      proximos <- if (origem$tipo_origem == "cep") {
        gs_servicos_proximos(
          cep = origem$cep, camadas = camadas, raio_m = raio_m,
          n_por_camada = n_por_camada, tipo_distancia = tipo_distancia,
          dir = dir
        )
      } else {
        gs_servicos_proximos(
          coordenadas = c(origem$latitude, origem$longitude), camadas = camadas,
          raio_m = raio_m, n_por_camada = n_por_camada,
          tipo_distancia = tipo_distancia, dir = dir
        )
      }
      ponto <- attr(proximos, "ponto")
      origens$latitude[i] <- ponto$latitude
      origens$longitude[i] <- ponto$longitude
      origens$rotulo[i] <- ponto$origem

      analises <- gs_analise_servicos(
        proximos, tipo = tipo, dir = dir, pop_layer = pop_layer,
        densidade_km2 = densidade_km2
      )
      falhas_analises <- gs_falhas_analises(analises)
      servicos <- proximos
      servicos$id_origem <- rep(id, nrow(servicos))
      servicos$tipo_origem <- rep(origem$tipo_origem, nrow(servicos))
      metricas_origem <- gs_metricas_analises(analises, id)
      comparacao_origem <- gs_resumo_origem(proximos, id)
      interpretacoes <- gs_interpretar_analise(analises, proximos, raio_m)
      resultado_origem <- list(
        servicos = proximos, analises = analises,
        interpretacoes = interpretacoes
      )
      arquivos <- gs_salvar_analises(
        proximos, analises, pasta_origem, id_origem = id,
        formato_relatorio = formato_relatorio, salvar_mapa = salvar_mapa,
        dpi = dpi, sobrescrever = FALSE
      )
      resultado_origem$arquivos <- arquivos

      servicos_lista[[id]] <- servicos
      metricas_lista[[id]] <- metricas_origem
      comparacoes[[id]] <- comparacao_origem
      manifestos[[id]] <- arquivos$manifesto
      resultados[[id]] <- resultado_origem
      origens$status[i] <- if (nrow(proximos) == 0) {
        "sem_servicos"
      } else if (nrow(falhas_analises) > 0) {
        "parcial"
      } else {
        "ok"
      }
      if (nrow(falhas_analises) > 0) {
        origens$mensagem[i] <- paste0(
          "Análises não executadas: ",
          paste(falhas_analises$analise, collapse = ", ")
        )
      }
      NULL
    }, error = function(e) e)

    if (!is.null(erro)) {
      if (dir.exists(pasta_origem)) unlink(pasta_origem, recursive = TRUE, force = TRUE)
      origens$status[i] <- "erro"
      origens$mensagem[i] <- conditionMessage(erro)
      resultados[[id]] <- list(erro = conditionMessage(erro))
      dir.create(pasta_origem, recursive = TRUE, showWarnings = FALSE)
      gs_escrever_texto_atomico(conditionMessage(erro),
                                file.path(pasta_origem, "erro.txt"))
      if (!continuar_em_erro) stop(erro)
    }
  }

  servicos <- if (length(servicos_lista) == 0) {
    data.frame(id_origem = character(0), stringsAsFactors = FALSE)
  } else {
    out <- do.call(rbind, servicos_lista)
    rownames(out) <- NULL
    out
  }
  metricas <- if (length(metricas_lista) == 0) {
    gs_metricas_analises(NULL)
  } else {
    out <- do.call(rbind, metricas_lista)
    rownames(out) <- NULL
    out
  }
  comparacao <- if (length(comparacoes) == 0) {
    data.frame(id_origem = character(0), n_servicos = integer(0))
  } else {
    out <- do.call(rbind, comparacoes)
    rownames(out) <- NULL
    out
  }
  manifesto <- if (length(manifestos) == 0) gs_manifesto_vazio() else {
    out <- do.call(rbind, manifestos)
    rownames(out) <- NULL
    out
  }

  gs_escrever_csv_atomico(origens, file.path(pasta, "origens.csv"))
  gs_escrever_csv_atomico(servicos, file.path(pasta, "servicos.csv"))
  gs_escrever_csv_atomico(metricas, file.path(pasta, "metricas.csv"))
  gs_escrever_csv_atomico(comparacao, file.path(pasta, "comparacao_origens.csv"))

  plots <- gs_plots_comparacao(comparacao, servicos)
  figuras <- character(0)
  if (length(plots) > 0) {
    for (nm in names(plots)) {
      caminho <- file.path(pasta, "figuras", paste0("comparacao_", nm, ".png"))
      gs_salvar_plot(plots[[nm]], caminho, dpi = dpi)
      figuras <- c(figuras, caminho)
      manifesto <- rbind(
        manifesto,
        gs_linha_manifesto("lote", "figura", "comparacao", nm, "png", caminho)
      )
    }
  }
  relatorio_lote <- gs_relatorio_lote(
    origens, comparacao, file.path(pasta, "relatorio_lote.md"), figuras
  )
  manifesto <- rbind(
    manifesto,
    gs_linha_manifesto("lote", "relatorio", "comparacao", "relatorio_lote",
                       "md", relatorio_lote)
  )
  arq_resultado <- file.path(pasta, "resultado_lote.rds")

  saida <- list(
    pasta = normalizePath(pasta, winslash = "/"), parametros = list(
      camadas = camadas, raio_m = raio_m, n_por_camada = n_por_camada,
      tipo_distancia = tipo_distancia, tipo = tipo
    ),
    origens = origens, resultados = resultados, servicos = servicos,
    metricas = metricas, comparacao = comparacao, manifesto = manifesto,
    relatorio = relatorio_lote
  )
  class(saida) <- c("gs_lote", "list")
  gs_gravar_atomico(arq_resultado, function(temporario) {
    saveRDS(saida, temporario, version = 3)
  })
  gs_escrever_csv_atomico(manifesto, file.path(pasta, "manifesto.csv"))
  invisible(saida)
}

print.gs_lote <- function(x, ...) {
  cat("Execução GeoSampa em lote\n")
  cat("Pasta:", x$pasta, "\n")
  cat("Origens:", nrow(x$origens), "| OK:", sum(x$origens$status == "ok"),
      "| Parciais:", sum(x$origens$status == "parcial"),
      "| Sem serviços:", sum(x$origens$status == "sem_servicos"),
      "| Erros:", sum(x$origens$status == "erro"), "\n")
  invisible(x)
}
