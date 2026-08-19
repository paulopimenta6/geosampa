# ============================================================
# GeoSampa — Análises estatísticas e espaciais
# ------------------------------------------------------------
# A partir dos serviços próximos (gs_servicos_proximos), oferece
# análises descritivas e espaciais, com o tipo escolhido pelo
# usuário: descritivas, vizinho mais próximo, Voronoi/Thiessen,
# densidade de kernel, raios progressivos, autocorrelação
# espacial (Moran's I, requer spdep) e rede viária (OSRM, requer
# pacote osrm). As duas últimas avisam quando o pacote não está
# instalado, em vez de falharem.
# ============================================================

# --- Utilitários internos -----------------------------------------------------
gs_dominio_consulta <- function(resultado, ponto = NULL, raio_m = NULL) {
  if (is.null(ponto)) ponto <- attr(resultado, "ponto")
  if (is.null(raio_m)) raio_m <- attr(resultado, "raio_m")

  raio_m <- suppressWarnings(as.numeric(raio_m)[1])
  if (length(raio_m) == 0 || !is.finite(raio_m) || raio_m <= 0) {
    return(list(executado = FALSE,
                mensagem = "Raio da consulta ausente ou inválido."))
  }

  ponto_sf <- NULL
  if (is.list(ponto) && !is.null(ponto$sf)) {
    ponto_sf <- ponto$sf
  } else if (inherits(ponto, c("sf", "sfc"))) {
    ponto_sf <- ponto
  } else if (is.list(ponto) &&
             all(c("longitude", "latitude") %in% names(ponto))) {
    xy <- suppressWarnings(as.numeric(c(ponto$longitude[1], ponto$latitude[1])))
    if (length(xy) == 2 && all(is.finite(xy))) {
      ponto_sf <- sf::st_sfc(sf::st_point(xy), crs = gs_epsg$wgs84)
    }
  }

  if (inherits(ponto_sf, "sf")) ponto_sf <- sf::st_geometry(ponto_sf)
  if (!inherits(ponto_sf, "sfc") || length(ponto_sf) != 1 ||
      is.na(sf::st_crs(ponto_sf)) || any(sf::st_is_empty(ponto_sf))) {
    return(list(executado = FALSE,
                mensagem = "Ponto da consulta ausente ou inválido."))
  }
  if (!identical(as.character(sf::st_geometry_type(ponto_sf))[1], "POINT")) {
    return(list(executado = FALSE,
                mensagem = "A geometria da consulta deve ser um único ponto."))
  }

  ponto_utm <- tryCatch(
    sf::st_transform(ponto_sf, gs_epsg$oficial),
    error = function(e) NULL
  )
  if (is.null(ponto_utm)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar o ponto da consulta."))
  }
  dominio <- tryCatch(sf::st_buffer(ponto_utm, raio_m),
                      error = function(e) NULL)
  if (is.null(dominio) || any(sf::st_is_empty(dominio))) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível construir o buffer da consulta."))
  }

  list(executado = TRUE, ponto = ponto_utm, dominio = dominio,
       raio_m = raio_m)
}

gs_com_seed <- function(seed, expr) {
  if (is.null(seed)) return(force(expr))
  seed <- suppressWarnings(as.integer(seed)[1])
  if (!is.finite(seed)) stop("`seed` deve ser um número inteiro finito.")

  tinha_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (tinha_seed) seed_anterior <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (tinha_seed) {
      assign(".Random.seed", seed_anterior, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

gs_resumo_distancias <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x <- suppressWarnings(as.numeric(x))
  validos <- is.finite(x)
  x_ok <- x[validos]
  n <- length(x_ok)
  nomes <- c("n", "n_ausentes", "min", "p05", "p25", "mediana",
             "media", "p75", "p95", "max", "mad", "iqr", "sd", "cv",
             "erro_padrao", "ic95_media_inf", "ic95_media_sup")
  out <- stats::setNames(rep(NA_real_, length(nomes)), nomes)
  out[c("n", "n_ausentes")] <- c(n, sum(!validos))
  if (n == 0) return(out)

  qs <- stats::quantile(x_ok, c(0.05, 0.25, 0.5, 0.75, 0.95),
                        names = FALSE, type = 7)
  media <- mean(x_ok)
  desvio <- if (n > 1) stats::sd(x_ok) else NA_real_
  erro <- if (n > 1 && is.finite(desvio)) desvio / sqrt(n) else NA_real_
  margem <- if (is.finite(erro)) stats::qt(0.975, df = n - 1) * erro else NA_real_
  out[c("min", "p05", "p25", "mediana", "media", "p75", "p95", "max",
        "mad", "iqr", "sd", "cv", "erro_padrao", "ic95_media_inf",
        "ic95_media_sup")] <- c(
          min(x_ok), qs[1], qs[2], qs[3], media, qs[4], qs[5], max(x_ok),
          stats::mad(x_ok, center = qs[3], constant = 1.4826), qs[4] - qs[2],
          desvio, if (is.finite(desvio) && media != 0) 100 * desvio / media else NA_real_,
          erro, media - margem, media + margem
        )
  out
}

# --- Descritivas: contagens e distribuição das distâncias --------------------
gs_analise_descritivas <- function(resultado) {
  if (!is.data.frame(resultado) || !"distancia_m" %in% names(resultado)) {
    return(list(executado = FALSE,
                mensagem = "Resultado sem a coluna numérica 'distancia_m'."))
  }
  medidas <- gs_resumo_distancias(resultado$distancia_m)
  if (medidas[["n"]] == 0) {
    return(list(executado = FALSE,
                mensagem = "Nenhuma distância válida para a análise descritiva."))
  }

  distancia <- resultado$distancia_m
  if (is.factor(distancia)) distancia <- as.character(distancia)
  distancia <- suppressWarnings(as.numeric(distancia))
  validos <- is.finite(distancia)
  dados_plot <- resultado[validos, , drop = FALSE]
  dados_plot$distancia_m <- distancia[validos]
  if (!"camada" %in% names(dados_plot)) dados_plot$camada <- "geral"
  dados_plot$camada <- as.character(dados_plot$camada)
  dados_plot$camada[is.na(dados_plot$camada) | !nzchar(dados_plot$camada)] <-
    "(sem camada)"

  n_por_camada <- as.data.frame(table(dados_plot$camada))
  names(n_por_camada) <- c("camada", "n")
  rownames(n_por_camada) <- NULL

  n_por_tipo <- NULL
  if ("tipo_servico" %in% names(dados_plot) &&
      !all(is.na(dados_plot$tipo_servico))) {
    n_por_tipo <- as.data.frame(table(dados_plot$tipo_servico))
    names(n_por_tipo) <- c("tipo_servico", "n")
    rownames(n_por_tipo) <- NULL
  }

  mediana <- medidas[["mediana"]]
  media <- medidas[["media"]]
  resumo_basico <- summary(dados_plot$distancia_m)
  resumo_estendido <- c(
    resumo_basico,
    P05 = medidas[["p05"]], MAD = medidas[["mad"]],
    IQR = medidas[["iqr"]], P95 = medidas[["p95"]],
    `IC95 média (inf.)` = medidas[["ic95_media_inf"]],
    `IC95 média (sup.)` = medidas[["ic95_media_sup"]]
  )

  list(
    executado        = TRUE,
    n_total          = nrow(resultado),
    n_por_camada     = n_por_camada,
    n_por_tipo       = n_por_tipo,
    resumo_distancia = resumo_estendido,
    estatisticas_distancia = as.data.frame(as.list(medidas),
                                           check.names = FALSE),
    metodo_ic = if (medidas[["n"]] > 1) {
      paste0(
        "IC descritivo de 95% t de Student para a média, sob hipótese i.i.d.; ",
        "não representa incerteza censitária dos dados administrativos"
      )
    } else {
      "IC não calculado: menos de duas observações"
    },
    histograma = ggplot2::ggplot(dados_plot, ggplot2::aes(x = distancia_m)) +
      ggplot2::geom_histogram(bins = 20, fill = "#2c7fb8", color = "white") +
      ggplot2::geom_vline(xintercept = mediana, color = "#d7301f",
                          linetype = "dashed", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = media, color = "#0570b0",
                          linewidth = 0.8) +
      ggplot2::labs(x = "Distância (m)", y = "Nº de serviços",
                    title = "Distribuição das distâncias",
                    caption = "Vermelho tracejado: mediana | Azul: média") +
      ggplot2::theme_minimal(),
    boxplot = ggplot2::ggplot(dados_plot,
                              ggplot2::aes(x = camada, y = distancia_m)) +
      ggplot2::geom_boxplot(fill = "#41b6c4") +
      ggplot2::stat_summary(fun = stats::median, geom = "point",
                            shape = 18, size = 3, color = "#d7301f") +
      ggplot2::labs(x = NULL, y = "Distância (m)",
                    title = "Distâncias por camada",
                    caption = "Losango vermelho: mediana") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  )
}

# --- Vizinho mais próximo: menor distância (geral e por camada) ---------------
gs_analise_vizinho <- function(resultado) {
  if (!is.data.frame(resultado) || nrow(resultado) == 0 ||
      !all(c("camada", "distancia_m") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "Nenhum serviço válido para identificar o vizinho mais próximo."))
  }
  distancia <- suppressWarnings(as.numeric(resultado$distancia_m))
  resultado <- resultado[is.finite(distancia), , drop = FALSE]
  resultado$distancia_m <- distancia[is.finite(distancia)]
  if (nrow(resultado) == 0) {
    return(list(executado = FALSE,
                mensagem = "Nenhuma distância finita para identificar o vizinho mais próximo."))
  }
  por_camada <- do.call(rbind, lapply(split(resultado, resultado$camada), function(d) {
    i <- which.min(d$distancia_m)
    data.frame(camada = d$camada[i], nome = d$nome[i],
               distancia_m = d$distancia_m[i], stringsAsFactors = FALSE)
  }))
  rownames(por_camada) <- NULL
  i <- which.min(resultado$distancia_m)
  list(
    executado              = TRUE,
    vizinho_mais_proximo = resultado[i, , drop = FALSE],
    por_camada           = por_camada
  )
}

# --- Voronoi/Thiessen: áreas de influência dos serviços -----------------------
# Devolve um sf de polígonos (em EPSG:4326) recortado pelo buffer da consulta.
# O cálculo é feito na projeção oficial (metros), sobre um único MULTIPOINT.
gs_analise_voronoi <- function(resultado, ponto = NULL, raio_m = NULL) {
  dominio <- gs_dominio_consulta(resultado, ponto, raio_m)
  if (!isTRUE(dominio$executado)) return(dominio)
  if (!is.data.frame(resultado) ||
      !all(c("longitude", "latitude") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "Resultado sem coordenadas para calcular Voronoi."))
  }

  lon <- suppressWarnings(as.numeric(resultado$longitude))
  lat <- suppressWarnings(as.numeric(resultado$latitude))
  validos <- is.finite(lon) & is.finite(lat)
  if (!any(validos)) {
    return(list(executado = FALSE,
                mensagem = "Nenhuma coordenada válida para calcular Voronoi."))
  }
  dados <- resultado[validos, , drop = FALSE]
  dados$longitude <- lon[validos]
  dados$latitude <- lat[validos]
  pts_utm <- tryCatch(
    sf::st_transform(
      sf::st_as_sf(dados, coords = c("longitude", "latitude"),
                   crs = gs_epsg$wgs84),
      gs_epsg$oficial
    ),
    error = function(e) NULL
  )
  if (is.null(pts_utm)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar os pontos para calcular Voronoi."))
  }
  dentro <- lengths(sf::st_intersects(pts_utm, dominio$dominio)) > 0
  pts_utm <- pts_utm[dentro, , drop = FALSE]
  if (nrow(pts_utm) == 0) {
    return(list(executado = FALSE,
                mensagem = "Nenhum ponto está dentro do domínio da consulta."))
  }

  coords <- sf::st_coordinates(pts_utm)
  chave <- paste(format(coords[, 1], digits = 15, scientific = FALSE,
                        trim = TRUE),
                 format(coords[, 2], digits = 15, scientific = FALSE,
                        trim = TRUE), sep = "|")
  grupos <- split(seq_len(nrow(pts_utm)), factor(chave, levels = unique(chave)))
  primeiro <- vapply(grupos, `[`, integer(1), 1)
  agrega_texto <- function(x, separador) {
    x <- unique(as.character(x[!is.na(x) & nzchar(as.character(x))]))
    if (length(x) == 0) NA_character_ else paste(x, collapse = separador)
  }
  valor_coluna <- function(nome, i) {
    if (nome %in% names(pts_utm)) pts_utm[[nome]][i] else rep(NA, length(i))
  }
  atributos <- do.call(rbind, lapply(grupos, function(i) {
    distancias <- suppressWarnings(as.numeric(valor_coluna("distancia_m", i)))
    data.frame(
      camada = agrega_texto(valor_coluna("camada", i), ", "),
      nome = agrega_texto(valor_coluna("nome", i), "; "),
      distancia_m = if (any(is.finite(distancias))) min(distancias, na.rm = TRUE) else NA_real_,
      n_servicos = length(i),
      stringsAsFactors = FALSE
    )
  }))
  sites <- sf::st_sf(site_id = seq_along(grupos), atributos,
                     geometry = sf::st_geometry(pts_utm)[primeiro])
  if (nrow(sites) < 2) {
    return(list(executado = FALSE,
                mensagem = paste0(
                  "São necessárias ao menos 2 coordenadas distintas para ",
                  "construir o Voronoi."
                ),
                n_deduplicados = nrow(pts_utm) - nrow(sites)))
  }

  multiponto <- sf::st_union(sf::st_geometry(sites))
  envelope <- sf::st_as_sfc(sf::st_bbox(dominio$dominio))
  geometrias <- tryCatch(
    sf::st_collection_extract(
      sf::st_voronoi(multiponto, envelope = envelope), "POLYGON"
    ),
    error = function(e) NULL
  )
  if (is.null(geometrias) || length(geometrias) == 0) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível construir as células de Voronoi."))
  }
  celulas <- sf::st_sf(cell_id = seq_along(geometrias),
                       geometry = geometrias)
  celulas <- tryCatch(
    suppressWarnings(sf::st_intersection(
      celulas, sf::st_sf(.dominio = 1L, geometry = dominio$dominio)
    )),
    error = function(e) NULL
  )
  if (is.null(celulas)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao recortar as células pelo buffer da consulta."))
  }

  areas <- suppressWarnings(as.numeric(sf::st_area(celulas)))
  celulas <- celulas[!sf::st_is_empty(celulas) & is.finite(areas) & areas > 0,
                     , drop = FALSE]
  if (nrow(celulas) == 0) {
    return(list(executado = FALSE,
                mensagem = "O recorte não produziu células de Voronoi válidas."))
  }
  hits <- sf::st_intersects(celulas, sites)
  associacao <- vapply(seq_len(nrow(celulas)), function(i) {
    if (length(hits[[i]]) == 1) return(hits[[i]])
    sf::st_nearest_feature(sf::st_point_on_surface(celulas[i, , drop = FALSE]),
                           sites)
  }, integer(1))
  atributos_saida <- sf::st_drop_geometry(sites)[
    match(associacao, sites$site_id),
    c("camada", "nome", "distancia_m", "n_servicos"), drop = FALSE
  ]
  pol <- sf::st_sf(atributos_saida, geometry = sf::st_geometry(celulas))
  pol <- pol[order(associacao), , drop = FALSE]
  pol <- sf::st_transform(pol, gs_epsg$wgs84)
  attr(pol, "raio_m") <- dominio$raio_m
  attr(pol, "n_deduplicados") <- nrow(pts_utm) - nrow(sites)
  pol
}

# --- Densidade de kernel dos serviços -----------------------------------------
gs_analise_kde <- function(resultado, ponto) {
  if (!is.data.frame(resultado) || nrow(resultado) < 3 ||
      !all(c("longitude", "latitude") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "São necessários ao menos 3 pontos para estimar a densidade."))
  }
  pts <- tryCatch(
    sf::st_transform(
      sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                   crs = gs_epsg$wgs84),
      gs_epsg$oficial
    ),
    error = function(e) NULL
  )
  if (is.null(pts)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar os pontos para a KDE."))
  }
  xy <- sf::st_coordinates(pts)
  if (length(unique(xy[, 1])) < 2 || length(unique(xy[, 2])) < 2) {
    return(list(executado = FALSE,
                mensagem = "A KDE exige variação nas duas coordenadas espaciais."))
  }
  dados <- data.frame(x_m = xy[, 1], y_m = xy[, 2])
  ggplot2::ggplot(dados, ggplot2::aes(x = x_m, y = y_m)) +
    ggplot2::stat_density_2d(
      ggplot2::aes(fill = ggplot2::after_stat(density)),
      geom = "raster", contour = FALSE, alpha = 0.82
    ) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::geom_point(color = "#d7301f", size = 1) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Densidade de kernel dos serviços",
      subtitle = sprintf("Ponto: %s | projeção EPSG:%s", ponto$origem,
                         gs_epsg$oficial),
      x = "Coordenada leste (m)", y = "Coordenada norte (m)",
      fill = "Densidade"
    )
}

# --- Raios progressivos: oportunidades acumuladas ------------------------------
# Devolve a contagem acumulada por raio (tabela) e a curva correspondente.
gs_analise_raios <- function(resultado, ponto, raios = c(500, 1000, 2000)) {
  raios <- suppressWarnings(as.numeric(raios))
  if (length(raios) == 0 || any(!is.finite(raios)) || any(raios <= 0)) {
    return(list(executado = FALSE,
                mensagem = "Os raios progressivos devem ser números positivos."))
  }
  distancias <- suppressWarnings(as.numeric(resultado$distancia_m))
  distancias <- distancias[is.finite(distancias)]
  if (length(distancias) == 0) {
    return(list(executado = FALSE,
                mensagem = "Nenhuma distância válida para os raios progressivos."))
  }
  raios <- sort(unique(raios))
  contagem <- data.frame(
    raio_m = raios,
    n_servicos = vapply(raios, function(r) sum(distancias <= r),
                         integer(1))
  )
  grafico <- ggplot2::ggplot(contagem, ggplot2::aes(x = raio_m, y = n_servicos)) +
    ggplot2::geom_area(alpha = 0.15, fill = "#2c7fb8") +
    ggplot2::geom_line(color = "#2c7fb8", linewidth = 1) +
    ggplot2::geom_point(color = "#2c7fb8", size = 2.5) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::labs(x = "Raio de busca (m)", y = "Nº de serviços alcançados",
                  title = "Oportunidades acumuladas por raio",
                  subtitle = sprintf("Ponto: %s", ponto$origem)) +
    ggplot2::theme_minimal()
  list(executado = TRUE, contagem = contagem, grafico = grafico)
}

# --- Autocorrelação espacial (Moran's I) — requer spdep ------------------------
# A versão padrão (sobre_grade = TRUE) aplica Moran's I às CONTAGENS de
# serviços por célula hexagonal (via gs_grade_hex), a aplicação estatisticamente
# correta para pontos. A versão alternativa (sobre_grade = FALSE) aplica às
# distâncias radiais e é mantida apenas como DIAGNÓSTICO, com ressalva: essa
# variável tem gradiente espacial construído (distância a um único ponto), o que
# tende a indicar agrupamento por construção.
# Nota de interpretação: para diagnósticos finos, agregue por unidade espacial
# (ex.: distritos) — veja `moran_distrital`.
gs_analise_moran <- function(resultado, celula_m = gs_celula_hex_m,
                             sobre_grade = TRUE, nsim = 999, seed = 123,
                             ponto = NULL, raio_m = NULL) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    return(list(
      executado = FALSE,
      mensagem = "Pacote 'spdep' não instalado. Instale com: install.packages('spdep')"
    ))
  }
  nsim <- suppressWarnings(as.integer(nsim)[1])
  seed_valido <- is.null(seed) ||
    (length(seed) > 0 && is.finite(suppressWarnings(as.numeric(seed)[1])))
  if (!is.finite(nsim) || nsim < 1 || !seed_valido) {
    return(list(executado = FALSE,
                mensagem = "`nsim` e `seed` devem ser números inteiros válidos."))
  }

  if (sobre_grade) {
    grade <- gs_grade_hex(resultado, celula_m, ponto = ponto, raio_m = raio_m)
    if (!inherits(grade, "sf")) return(grade)
    if (nrow(grade) < 4) {
      return(list(executado = FALSE,
                  mensagem = "Menos de 4 células no domínio para Moran's I."))
    }
    x <- as.numeric(grade$n_servicos)
    variancia <- stats::var(x)
    if (!is.finite(variancia) || variancia <= 0) {
      return(list(executado = FALSE,
                  mensagem = "Contagens sem variância — Moran's I não é definido."))
    }
    nb <- tryCatch(spdep::poly2nb(sf::st_geometry(grade), queen = TRUE),
                   error = function(e) NULL)
    graus <- if (is.null(nb)) integer(0) else spdep::card(nb)
    if (length(graus) != nrow(grade) || sum(graus) == 0 || any(graus == 0)) {
      return(list(executado = FALSE,
                  mensagem = "Vizinhança da grade inválida ou com células isoladas."))
    }
    lw <- tryCatch(spdep::nb2listw(nb, style = "W", zero.policy = FALSE),
                   error = function(e) NULL)
    if (is.null(lw)) {
      return(list(executado = FALSE,
                  mensagem = "Não foi possível construir os pesos espaciais da grade."))
    }
    teste <- tryCatch(
      gs_com_seed(seed, spdep::moran.mc(
        x, lw, nsim = nsim, zero.policy = FALSE, alternative = "two.sided"
      )),
      error = function(e) NULL
    )
    if (is.null(teste) || !is.finite(unname(teste$statistic)) ||
        !is.finite(teste$p.value)) {
      return(list(executado = FALSE,
                  mensagem = "Falha ao executar Moran's I (configuração de vizinhança inválida)."))
    }
    moran_i <- unname(teste$statistic)
    valor_p <- teste$p.value
    interpretacao <- if (valor_p < 0.05 && moran_i > 0) {
      "Há autocorrelação espacial positiva: células vizinhas tendem a ter contagens de serviços semelhantes (agrupamento)."
    } else if (valor_p < 0.05 && moran_i < 0) {
      "Há autocorrelação espacial negativa: células vizinhas tendem a ter contagens de serviços distintas (dispersão)."
    } else {
      "Não há evidência de autocorrelação espacial significativa na contagem de serviços por célula."
    }
    return(list(
      executado = TRUE, metodo = "grade_hex", celula_m = celula_m,
      moran_i = round(moran_i, 4), valor_p = round(valor_p, 4),
      n_celulas = nrow(grade),
      n_celulas_ocupadas = sum(x > 0),
      metodo_p = "Monte Carlo bilateral", nsim = nsim, seed = seed,
      interpretacao = interpretacao,
      objeto = teste
    ))
  }

  # --- Versão DIAGNÓSTICA: Moran sobre a distância radial ----------------------
  if (!is.data.frame(resultado) ||
      !all(c("longitude", "latitude", "distancia_m") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "Resultado sem coordenadas ou distâncias para Moran's I."))
  }
  lon <- suppressWarnings(as.numeric(resultado$longitude))
  lat <- suppressWarnings(as.numeric(resultado$latitude))
  distancia <- suppressWarnings(as.numeric(resultado$distancia_m))
  validos <- is.finite(lon) & is.finite(lat) & is.finite(distancia)
  d <- resultado[validos, , drop = FALSE]
  d$longitude <- lon[validos]
  d$latitude <- lat[validos]
  d$distancia_m <- distancia[validos]
  chave <- sprintf("%.12f|%.12f", d$longitude, d$latitude)
  dup   <- duplicated(chave)
  d     <- d[!dup, , drop = FALSE]
  n_dup <- sum(dup)
  if (nrow(d) < 4) {
    return(list(executado = FALSE,
                mensagem = "Menos de 4 pontos distintos — número insuficiente para Moran's I."))
  }
  variancia <- stats::var(d$distancia_m)
  if (!is.finite(variancia) || variancia <= 0) {
    return(list(executado = FALSE,
                mensagem = "Distâncias sem variância — Moran's I não é definido."))
  }
  coords <- tryCatch(
    sf::st_coordinates(sf::st_transform(
      sf::st_as_sf(d, coords = c("longitude", "latitude"),
                   crs = gs_epsg$wgs84),
      gs_epsg$oficial
    )),
    error = function(e) NULL
  )
  if (is.null(coords)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar os pontos para Moran's I."))
  }
  nb <- tryCatch(
    spdep::knn2nb(spdep::knearneigh(coords, k = min(5, nrow(d) - 1)),
                  sym = TRUE),
    error = function(e) NULL
  )
  graus <- if (is.null(nb)) integer(0) else spdep::card(nb)
  if (length(graus) != nrow(d) || sum(graus) == 0 || any(graus == 0)) {
    return(list(executado = FALSE,
                mensagem = "Vizinhança inválida para Moran's I."))
  }
  ncomp <- spdep::n.comp.nb(nb)
  avisos <- character(0)
  if (ncomp$nc > 1) {
    avisos <- c(avisos, sprintf(
      "Vizinhança com %d sub-grafo(s) desconexo(s); os resultados locais nesses sub-grafos não são confiáveis.",
      ncomp$nc))
  }
  lw <- tryCatch(spdep::nb2listw(nb, style = "W", zero.policy = FALSE),
                 error = function(e) NULL)
  if (is.null(lw)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível construir os pesos espaciais."))
  }
  teste <- tryCatch(
    gs_com_seed(seed, spdep::moran.mc(
      d$distancia_m, lw, nsim = nsim, zero.policy = FALSE,
      alternative = "two.sided"
    )),
    error = function(e) NULL
  )
  if (is.null(teste) || !is.finite(unname(teste$statistic)) ||
      !is.finite(teste$p.value)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao executar Moran's I (configuração de vizinhança inválida)."))
  }
  moran_i <- unname(teste$statistic)
  valor_p <- teste$p.value
  interpretacao <- if (valor_p < 0.05 && moran_i > 0) {
    "Há autocorrelação espacial positiva na distância radial."
  } else if (valor_p < 0.05 && moran_i < 0) {
    "Há autocorrelação espacial negativa na distância radial."
  } else {
    "Não há evidência de autocorrelação espacial significativa na distância radial."
  }
  list(
    executado = TRUE,
    metodo    = "distancia_radial",
    moran_i   = round(moran_i, 4),
    valor_p   = round(valor_p, 4),
    n_pontos  = nrow(d),
    n_deduplicados = n_dup,
    metodo_p = "Monte Carlo bilateral", nsim = nsim, seed = seed,
    avisos    = c(avisos, paste0(
      "Moran aplicado à distância radial tem gradiente espacial construído ",
      "(distância a um único ponto) e tende a indicar agrupamento por ",
      "construção; use sobre_grade = TRUE (padrão) ou moran_distrital para ",
      "diagnóstico confiável.")),
    interpretacao = interpretacao,
    objeto    = teste
  )
}

# --- Distâncias por rede viária (OSRM) — requer pacote osrm --------------------
# Usa o servidor público de demonstração do OSRM; cobertura e limites se
# aplicam. Compara a distância rodoviária com a distância em linha reta.
gs_analise_rede <- function(resultado, ponto) {
  if (!requireNamespace("osrm", quietly = TRUE)) {
    return(list(
      executado = FALSE,
      mensagem = "Pacote 'osrm' não instalado. Instale com: install.packages('osrm')"
    ))
  }
  if (!is.data.frame(resultado) || nrow(resultado) == 0 ||
      !all(c("longitude", "latitude", "camada", "nome") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "Nenhum destino válido para consultar a rede viária."))
  }
  opcoes_anteriores <- options(
    osrm.server = gs_osrm_server(), osrm.profile = gs_osrm_profile()
  )
  on.exit(options(opcoes_anteriores), add = TRUE)
  origem <- gs_osrm_input("origem", ponto$longitude, ponto$latitude)
  indices <- split(seq_len(nrow(resultado)),
                   ceiling(seq_len(nrow(resultado)) / 99))
  if (nrow(resultado) > 200) {
    message("  rede viária: calculando distâncias para ", nrow(resultado),
            " destinos via OSRM (pode demorar)...")
  }
  dist_rede_m <- tryCatch(unlist(lapply(indices, function(i) {
    destinos <- gs_osrm_input(
      as.character(i), resultado$longitude[i], resultado$latitude[i]
    )
    tab <- gs_consultar_tabela_osrm(origem, destinos)
    gs_osrm_dist_m(tab$distances[1, ])
  }), use.names = FALSE), error = function(e) NULL)
  if (is.null(dist_rede_m) || length(dist_rede_m) != nrow(resultado)) {
    return(list(
      executado = FALSE,
      servidor  = gs_osrm_server(),
      mensagem = paste0(
        "Falha ao consultar o servidor OSRM (", gs_osrm_server(),
        "). O servidor demo pode estar fora do ar ou sem cobertura para a ",
        "região. Configure outro com options(osrm.server = 'http://...') ",
        "ou options(gs.osrm_server = 'http://...').")
    ))
  }
  origem_sf <- sf::st_sfc(
    sf::st_point(c(ponto$longitude, ponto$latitude)), crs = gs_epsg$wgs84
  )
  destinos_sf <- sf::st_as_sf(
    resultado, coords = c("longitude", "latitude"), crs = gs_epsg$wgs84
  )
  dist_reta_m <- as.numeric(sf::st_distance(origem_sf, destinos_sf))
  out <- data.frame(
    camada            = resultado$camada,
    nome              = resultado$nome,
    distancia_reta_m  = round(dist_reta_m, 1),
    distancia_rede_m  = dist_rede_m,
    razao_rede_reta   = round(dist_rede_m / pmax(dist_reta_m, 1), 2),
    stringsAsFactors  = FALSE
  )
  list(executado = TRUE, resultado = out)
}

# ============================================================
# Análises novas — acessibilidade, cobertura e estatística espacial avançada
# ============================================================

# --- Acessibilidade: resumo das distâncias (geral, por camada, por tipo) -----
# Medidas robustas para distribuições assimétricas (comuns em distâncias):
# mediana (destaque), P25/P75, IQR e CV além de média/desvio-padrão.
gs_analise_acessibilidade <- function(resultado) {
  if (!is.data.frame(resultado) || !"distancia_m" %in% names(resultado)) {
    return(list(executado = FALSE,
                mensagem = "Resultado sem a coluna numérica 'distancia_m'."))
  }
  medidas <- gs_resumo_distancias(resultado$distancia_m)
  if (medidas[["n"]] == 0) {
    return(list(executado = FALSE,
                mensagem = "Nenhuma distância válida para analisar acessibilidade."))
  }
  distancia <- resultado$distancia_m
  if (is.factor(distancia)) distancia <- as.character(distancia)
  distancia <- suppressWarnings(as.numeric(distancia))
  validos <- is.finite(distancia)
  dados <- resultado[validos, , drop = FALSE]
  dados$distancia_m <- distancia[validos]
  if (!"camada" %in% names(dados)) dados$camada <- "geral"
  dados$camada <- as.character(dados$camada)
  dados$camada[is.na(dados$camada) | !nzchar(dados$camada)] <- "(sem camada)"

  geral <- as.data.frame(as.list(medidas), check.names = FALSE)
  por_camada <- do.call(rbind, lapply(split(dados, dados$camada), function(d) {
    data.frame(camada = d$camada[1],
               as.list(gs_resumo_distancias(d$distancia_m)),
               check.names = FALSE, row.names = NULL)
  }))
  rownames(por_camada) <- NULL
  por_tipo <- NULL
  if ("tipo_servico" %in% names(dados) && !all(is.na(dados$tipo_servico))) {
    dados_tipo <- dados[!is.na(dados$tipo_servico) &
                          nzchar(as.character(dados$tipo_servico)), , drop = FALSE]
    por_tipo <- do.call(rbind, lapply(split(dados_tipo, dados_tipo$tipo_servico), function(d) {
      data.frame(tipo_servico = as.character(d$tipo_servico[1]),
                 as.list(gs_resumo_distancias(d$distancia_m)),
                 check.names = FALSE, row.names = NULL)
    }))
    if (!is.null(por_tipo)) rownames(por_tipo) <- NULL
  }
  qs <- unname(medidas[c("p25", "mediana", "p75")])
  grafico_ecdf <- ggplot2::ggplot(dados, ggplot2::aes(x = distancia_m)) +
    ggplot2::stat_ecdf(geom = "step", color = "#2c7fb8", linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = qs, linetype = "dashed",
                        color = "grey55") +
    ggplot2::labs(x = "Distância (m)", y = "Proporção acumulada de serviços",
                  title = "Curva acumulada das distâncias (ECDF)",
                  subtitle = "Linhas tracejadas: P25, mediana e P75") +
    ggplot2::theme_minimal()
  list(executado = TRUE, geral = geral, por_camada = por_camada,
       por_tipo = por_tipo,
       metodo_ic = if (medidas[["n"]] > 1) {
         paste0(
           "IC descritivo de 95% t de Student para a média, sob hipótese i.i.d.; ",
           "não representa incerteza censitária dos dados administrativos"
         )
       } else {
         "IC não calculado: menos de duas observações"
       },
       grafico_ecdf = grafico_ecdf)
}

# --- Cobertura por buffer: área coberta pelos buffers dos serviços ------------
# Calcula, por camada (e no geral), a área coberta pela união de buffers de
# `raio_buffer_m`, sempre limitada pelo buffer usado na consulta.
gs_analise_cobertura <- function(resultado, ponto = NULL,
                                 raio_buffer_m = gs_raio_buffer_m,
                                 raio_m = NULL) {
  dominio <- gs_dominio_consulta(resultado, ponto, raio_m)
  if (!isTRUE(dominio$executado)) return(dominio)
  raio_buffer_m <- suppressWarnings(as.numeric(raio_buffer_m)[1])
  if (!is.finite(raio_buffer_m) || raio_buffer_m <= 0) {
    return(list(executado = FALSE,
                mensagem = "O raio dos buffers de cobertura deve ser positivo."))
  }
  if (!is.data.frame(resultado) || nrow(resultado) == 0 ||
      !all(c("longitude", "latitude", "camada") %in% names(resultado))) {
    return(list(executado = FALSE, mensagem = "Nenhum ponto para calcular cobertura."))
  }

  lon <- suppressWarnings(as.numeric(resultado$longitude))
  lat <- suppressWarnings(as.numeric(resultado$latitude))
  validos <- is.finite(lon) & is.finite(lat)
  dados <- resultado[validos, , drop = FALSE]
  dados$longitude <- lon[validos]
  dados$latitude <- lat[validos]
  if (nrow(dados) == 0) {
    return(list(executado = FALSE, mensagem = "Nenhuma coordenada válida para cobertura."))
  }
  dados$camada <- as.character(dados$camada)
  dados$camada[is.na(dados$camada) | !nzchar(dados$camada)] <- "(sem camada)"
  pts_utm <- tryCatch(
    sf::st_transform(sf::st_as_sf(dados, coords = c("longitude", "latitude"),
                                  crs = gs_epsg$wgs84),
                     gs_epsg$oficial),
    error = function(e) NULL
  )
  if (is.null(pts_utm)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar os pontos para cobertura."))
  }
  dentro <- lengths(sf::st_intersects(pts_utm, dominio$dominio)) > 0
  pts_utm <- pts_utm[dentro, , drop = FALSE]
  if (nrow(pts_utm) == 0) {
    return(list(executado = FALSE,
                mensagem = "Nenhum serviço está dentro do domínio da consulta."))
  }

  area_dominio <- as.numeric(sf::st_area(dominio$dominio))
  calcula_area <- function(x) {
    uniao <- sf::st_union(sf::st_buffer(sf::st_geometry(x), raio_buffer_m))
    recorte <- suppressWarnings(sf::st_intersection(uniao, dominio$dominio))
    if (length(recorte) == 0 || all(sf::st_is_empty(recorte))) return(0)
    sum(as.numeric(sf::st_area(recorte)), na.rm = TRUE)
  }
  por_camada <- lapply(split(seq_len(nrow(pts_utm)), pts_utm$camada), function(i) {
    area <- calcula_area(pts_utm[i, , drop = FALSE])
    data.frame(camada = pts_utm$camada[i[1]], n = length(i),
               area_coberta_km2 = round(area / 1e6, 2),
               area_no_dominio_km2 = round(area / 1e6, 2),
               pct_dominio = round(100 * area / area_dominio, 2),
               area_no_hull_km2 = round(area / 1e6, 2),
               pct_hull = round(100 * area / area_dominio, 2))
  })
  por_camada <- do.call(rbind, por_camada)
  rownames(por_camada) <- NULL
  area_todas <- calcula_area(pts_utm)
  list(
    executado = TRUE,
    raio_buffer_m = raio_buffer_m,
    raio_consulta_m = dominio$raio_m,
    area_consulta_km2 = round(area_dominio / 1e6, 2),
    area_hull_km2 = round(area_dominio / 1e6, 2),
    area_coberta_km2 = round(area_todas / 1e6, 2),
    pct_cobertura = round(100 * area_todas / area_dominio, 2),
    por_camada = por_camada
  )
}

# --- Raio ótimo: percentis da distribuição das distâncias ----------------------
# O menor raio que "alcança" X% dos serviços (quantis da distância), com o
# gráfico ECDF que permite ver visualmente o percentil correspondente a cada
# raio.
gs_analise_raio_otimo <- function(resultado, p = c(0.5, 0.75, 0.9, 0.95)) {
  p <- suppressWarnings(as.numeric(p))
  distancias <- suppressWarnings(as.numeric(resultado$distancia_m))
  distancias <- distancias[is.finite(distancias)]
  if (length(distancias) == 0) {
    return(list(executado = FALSE,
                mensagem = "Nenhuma distância válida para estimar raios de cobertura."))
  }
  if (length(p) == 0 || any(!is.finite(p)) || any(p <= 0 | p >= 1)) {
    return(list(executado = FALSE,
                mensagem = "Os percentis devem estar estritamente entre 0 e 1."))
  }
  p <- sort(unique(p))
  q <- stats::quantile(distancias, probs = p)
  percentis <- data.frame(percentil = names(q), raio_m = unname(round(q, 0)))
  dados <- data.frame(distancia_m = distancias)
  grafico <- ggplot2::ggplot(dados, ggplot2::aes(x = distancia_m)) +
    ggplot2::stat_ecdf(geom = "step", color = "#2c7fb8", linewidth = 0.9) +
    ggplot2::geom_hline(yintercept = as.numeric(p), linetype = "dashed",
                        color = "grey50") +
    ggplot2::geom_vline(xintercept = unname(q), linetype = "dotted",
                        color = "#d7301f") +
    ggplot2::labs(x = "Distância (m)", y = "Proporção acumulada de serviços",
                  title = "Raio necessário para alcançar X% dos serviços",
                  subtitle = "Vermelho pontilhado: percentis P50, P75, P90 e P95") +
    ggplot2::theme_minimal()
  list(executado = TRUE, percentis = percentis, grafico = grafico)
}

# --- Índice de Vizinho Mais Próximo (NNI) --------------------------------------
# Razão entre a distância média observada ao vizinho mais próximo e a esperada
# para um padrão aleatório. R < 1 → agrupado; R > 1 → disperso.
gs_analise_nni <- function(resultado, ponto = NULL, raio_m = NULL,
                           nsim = 199, seed = 123) {
  dominio <- gs_dominio_consulta(resultado, ponto, raio_m)
  if (!isTRUE(dominio$executado)) return(dominio)
  nsim <- suppressWarnings(as.integer(nsim)[1])
  seed_valido <- is.null(seed) ||
    (length(seed) > 0 && is.finite(suppressWarnings(as.numeric(seed)[1])))
  if (!is.finite(nsim) || nsim < 0 || !seed_valido) {
    return(list(executado = FALSE,
                mensagem = "`nsim` deve ser não negativo e `seed`, um inteiro válido."))
  }
  if (!is.data.frame(resultado) ||
      !all(c("longitude", "latitude") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "Resultado sem coordenadas para calcular o NNI."))
  }
  lon <- suppressWarnings(as.numeric(resultado$longitude))
  lat <- suppressWarnings(as.numeric(resultado$latitude))
  validos <- is.finite(lon) & is.finite(lat)
  dados <- resultado[validos, , drop = FALSE]
  dados$longitude <- lon[validos]
  dados$latitude <- lat[validos]
  if (nrow(dados) < 2) {
    return(list(executado = FALSE,
                mensagem = "São necessários ao menos 2 pontos válidos para o NNI."))
  }
  pts_utm <- tryCatch(
    sf::st_transform(sf::st_as_sf(dados, coords = c("longitude", "latitude"),
                                  crs = gs_epsg$wgs84),
                     gs_epsg$oficial),
    error = function(e) NULL
  )
  if (is.null(pts_utm)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar os pontos para o NNI."))
  }
  dentro <- lengths(sf::st_intersects(pts_utm, dominio$dominio)) > 0
  n_fora <- sum(!dentro)
  pts_utm <- pts_utm[dentro, , drop = FALSE]
  coords <- sf::st_coordinates(pts_utm)
  duplicados <- duplicated(as.data.frame(coords))
  n_dup <- sum(duplicados)
  coords <- coords[!duplicados, , drop = FALSE]
  n <- nrow(coords)
  if (n < 2) {
    return(list(executado = FALSE,
                mensagem = "Menos de 2 localizações distintas no domínio para o NNI.",
                n_deduplicados = n_dup))
  }

  distancias <- as.matrix(stats::dist(coords))
  diag(distancias) <- Inf
  obs <- mean(apply(distancias, 1, min))
  area <- as.numeric(sf::st_area(dominio$dominio))
  if (!is.finite(obs) || !is.finite(area) || area <= 0) {
    return(list(executado = FALSE,
                mensagem = "Distâncias ou área do domínio inválidas para o NNI."))
  }
  esperado <- 0.5 * sqrt(area / n)
  se <- 0.26136 * sqrt(area / n^2)
  R <- obs / esperado
  z <- if (is.finite(se) && se > 0) (obs - esperado) / se else NA_real_
  pvalor <- if (is.finite(z)) 2 * stats::pnorm(-abs(z)) else NA_real_
  metodo_p <- "aproximação normal"
  esperado_mc <- NA_real_

  if (nsim >= 19 && n <= 500) {
    simulados <- tryCatch(gs_com_seed(seed, replicate(nsim, {
      raios <- dominio$raio_m * sqrt(stats::runif(n))
      angulos <- stats::runif(n, 0, 2 * pi)
      xy <- cbind(raios * cos(angulos), raios * sin(angulos))
      dm <- as.matrix(stats::dist(xy))
      diag(dm) <- Inf
      mean(apply(dm, 1, min))
    })), error = function(e) NULL)
    if (!is.null(simulados) && all(is.finite(simulados))) {
      esperado_mc <- mean(simulados)
      desvio_mc <- stats::sd(simulados)
      if (is.finite(desvio_mc) && desvio_mc > 0) {
        z <- (obs - esperado_mc) / desvio_mc
      }
      p_inferior <- (1 + sum(simulados <= obs)) / (nsim + 1)
      p_superior <- (1 + sum(simulados >= obs)) / (nsim + 1)
      pvalor <- min(1, 2 * min(p_inferior, p_superior))
      metodo_p <- "Monte Carlo bilateral no buffer circular"
    }
  }
  referencia <- if (is.finite(esperado_mc)) esperado_mc else esperado
  interpretacao <- if (is.finite(pvalor) && pvalor < 0.05 && obs < referencia) {
    "Agrupado"
  } else if (is.finite(pvalor) && pvalor < 0.05 && obs > referencia) {
    "Disperso (uniforme)"
  } else {
    "Compatível com aleatoriedade espacial"
  }
  avisos <- character(0)
  if (n_dup > 0) {
    avisos <- c(avisos, sprintf(
      "%d ocorrência(s) co-localizada(s) foram removidas para o padrão pontual simples.",
      n_dup))
  }
  if (n_fora > 0) {
    avisos <- c(avisos, sprintf(
      "%d ponto(s) fora do buffer da consulta foram ignorados.", n_fora))
  }
  if (!identical(metodo_p, "Monte Carlo bilateral no buffer circular")) {
    avisos <- c(avisos,
                "P-valor analítico sem correção explícita do efeito de borda.")
  }
  list(
    executado = TRUE, n = n,
    n_deduplicados = n_dup,
    distancia_observada_m = round(obs, 1),
    distancia_esperada_m = round(esperado, 1),
    distancia_esperada_mc_m = round(esperado_mc, 1),
    indice_nni = round(R, 3), z = round(z, 2),
    valor_p = round(pvalor, 4), area_km2 = round(area / 1e6, 2),
    metodo_p = metodo_p, nsim = if (grepl("Monte Carlo", metodo_p)) nsim else 0L,
    seed = seed,
    interpretacao = interpretacao,
    avisos = avisos
  )
}

# --- Grade hexagonal de contagens (apoio para LISA / Getis-Ord) ----------------
gs_grade_hex <- function(resultado, celula_m = gs_celula_hex_m,
                         ponto = NULL, raio_m = NULL) {
  dominio <- gs_dominio_consulta(resultado, ponto, raio_m)
  if (!isTRUE(dominio$executado)) return(dominio)
  celula_m <- suppressWarnings(as.numeric(celula_m)[1])
  if (!is.finite(celula_m) || celula_m <= 0) {
    return(list(executado = FALSE,
                mensagem = "O tamanho da célula deve ser positivo."))
  }

  grade <- tryCatch(
    sf::st_make_grid(dominio$dominio, cellsize = celula_m, square = FALSE),
    error = function(e) NULL
  )
  if (is.null(grade) || length(grade) == 0) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível construir a grade hexagonal."))
  }
  grade_sf <- sf::st_sf(celula_id = seq_along(grade), geometry = grade)
  toca <- lengths(sf::st_intersects(grade_sf, dominio$dominio)) > 0
  grade_sf <- grade_sf[toca, , drop = FALSE]
  grade_sf <- tryCatch(
    suppressWarnings(sf::st_intersection(
      grade_sf, sf::st_sf(.dominio = 1L, geometry = dominio$dominio)
    )),
    error = function(e) NULL
  )
  if (is.null(grade_sf)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível recortar a grade pelo domínio."))
  }
  areas <- suppressWarnings(as.numeric(sf::st_area(grade_sf)))
  grade_sf <- grade_sf[!sf::st_is_empty(grade_sf) & is.finite(areas) & areas > 0,
                       , drop = FALSE]
  grade_sf <- grade_sf[order(grade_sf$celula_id), , drop = FALSE]
  if (nrow(grade_sf) == 0) {
    return(list(executado = FALSE,
                mensagem = "O domínio não contém células hexagonais válidas."))
  }
  grade_sf$n_servicos <- integer(nrow(grade_sf))
  grade_sf$camadas <- rep(NA_character_, nrow(grade_sf))

  if (!is.data.frame(resultado) || nrow(resultado) == 0) {
    attr(grade_sf, "raio_m") <- dominio$raio_m
    return(grade_sf)
  }
  if (!all(c("longitude", "latitude") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "Resultado sem coordenadas para preencher a grade."))
  }
  lon <- suppressWarnings(as.numeric(resultado$longitude))
  lat <- suppressWarnings(as.numeric(resultado$latitude))
  validos <- is.finite(lon) & is.finite(lat)
  dados <- resultado[validos, , drop = FALSE]
  dados$longitude <- lon[validos]
  dados$latitude <- lat[validos]
  if (nrow(dados) == 0) {
    attr(grade_sf, "raio_m") <- dominio$raio_m
    return(grade_sf)
  }
  pts_utm <- tryCatch(
    sf::st_transform(sf::st_as_sf(dados, coords = c("longitude", "latitude"),
                                  crs = gs_epsg$wgs84),
                     gs_epsg$oficial),
    error = function(e) NULL
  )
  if (is.null(pts_utm)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar os pontos da grade."))
  }
  candidatas <- sf::st_intersects(pts_utm, grade_sf)
  atribuicao <- vapply(candidatas, function(i) {
    if (length(i) == 0) NA_integer_ else i[1]
  }, integer(1))
  dentro <- !is.na(atribuicao)
  grade_sf$n_servicos <- tabulate(atribuicao[dentro], nbins = nrow(grade_sf))
  camadas <- if ("camada" %in% names(pts_utm)) {
    as.character(pts_utm$camada)
  } else {
    rep(NA_character_, nrow(pts_utm))
  }
  grade_sf$camadas <- vapply(seq_len(nrow(grade_sf)), function(i) {
    valores <- sort(unique(camadas[dentro & atribuicao == i]))
    valores <- valores[!is.na(valores) & nzchar(valores)]
    if (length(valores) == 0) NA_character_ else paste(valores, collapse = ", ")
  }, character(1))
  attr(grade_sf, "raio_m") <- dominio$raio_m
  grade_sf
}

# --- Mapa simples de contagens por célula --------------------------------------
gs_mapa_grade <- function(grade, titulo) {
  ggplot2::ggplot(grade) +
    ggplot2::geom_sf(ggplot2::aes(fill = n_servicos), color = "white",
                     linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(na.value = "grey90") +
    gs_tema_mapa() +
    ggplot2::labs(title = titulo, fill = "Nº de serviços")
}

# --- Mapa de classes (pontos quentes/frios ou LISA) ----------------------------
gs_mapa_grade_classe <- function(grade, titulo) {
  cores <- c("ponto quente" = "#d7301f", "ponto frio" = "#0570b0",
              "alto-alto" = "#d7301f", "baixo-baixo" = "#0570b0",
              "alto-baixo" = "#fc8d59", "baixo-alto" = "#74add1",
              "não significativo" = "grey85")
  ggplot2::ggplot(grade) +
    ggplot2::geom_sf(ggplot2::aes(fill = classe), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = cores) +
    gs_tema_mapa() +
    ggplot2::labs(title = titulo, fill = "Classe")
}

gs_classificar_lisa <- function(valor_centrado, lag_espacial, p_ajustado,
                                alpha = 0.05) {
  significativo <- is.finite(p_ajustado) & p_ajustado < alpha &
    is.finite(valor_centrado) & is.finite(lag_espacial) &
    valor_centrado != 0 & lag_espacial != 0
  classe <- rep("não significativo", length(valor_centrado))
  classe[significativo & valor_centrado > 0 & lag_espacial > 0] <- "alto-alto"
  classe[significativo & valor_centrado < 0 & lag_espacial < 0] <- "baixo-baixo"
  classe[significativo & valor_centrado > 0 & lag_espacial < 0] <- "alto-baixo"
  classe[significativo & valor_centrado < 0 & lag_espacial > 0] <- "baixo-alto"
  classe
}

# --- Getis-Ord (G* local) — aglomerados quentes/frios — requer spdep ------------
gs_analise_getis_ord <- function(resultado, celula_m = gs_celula_hex_m,
                                 ponto = NULL, raio_m = NULL) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = "Pacote 'spdep' não instalado. Instale com: install.packages('spdep')"))
  }
  grade <- gs_grade_hex(resultado, celula_m, ponto = ponto, raio_m = raio_m)
  if (!inherits(grade, "sf")) return(grade)
  if (nrow(grade) < 4) {
    return(list(executado = FALSE,
                mensagem = "Menos de 4 células no domínio para Getis-Ord."))
  }
  x <- as.numeric(grade$n_servicos)
  variancia <- stats::var(x)
  if (!is.finite(variancia) || variancia <= 0) {
    return(list(executado = FALSE,
                mensagem = "Contagens sem variância — Getis-Ord G* não é definido."))
  }
  nb <- tryCatch(spdep::poly2nb(sf::st_geometry(grade), queen = TRUE),
                 error = function(e) NULL)
  graus <- if (is.null(nb)) integer(0) else spdep::card(nb)
  if (length(graus) != nrow(grade) || sum(graus) == 0 || any(graus == 0)) {
    return(list(executado = FALSE,
                mensagem = "Vizinhança da grade inválida ou com células isoladas."))
  }
  nb_gstar <- spdep::include.self(nb)
  lw <- tryCatch(spdep::nb2listw(nb_gstar, style = "B", zero.policy = FALSE),
                 error = function(e) NULL)
  if (is.null(lw)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível construir os pesos de Getis-Ord."))
  }
  gi <- tryCatch(spdep::localG(x, lw, zero.policy = FALSE),
                 error = function(e) NULL)
  if (is.null(gi)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao executar Getis-Ord (pouca variância nas células)."))
  }
  internos <- attr(gi, "internals")
  grade$gi <- if (!is.null(internos) && "G*i" %in% colnames(internos)) {
    as.numeric(internos[, "G*i"])
  } else {
    rep(NA_real_, nrow(grade))
  }
  grade$gi_z <- as.numeric(gi)
  if (!any(is.finite(grade$gi_z))) {
    return(list(executado = FALSE,
                mensagem = "Getis-Ord G* sem variância local definida."))
  }
  grade$p_valor <- ifelse(is.finite(grade$gi_z),
                           2 * stats::pnorm(-abs(grade$gi_z)), 1)
  grade$p_ajustado <- stats::p.adjust(grade$p_valor, method = "BH")
  grade$p_valor_ajustado <- grade$p_ajustado
  grade$classe <- ifelse(grade$p_ajustado < 0.05 & grade$gi_z > 0,
                          "ponto quente",
                   ifelse(grade$p_ajustado < 0.05 & grade$gi_z < 0,
                          "ponto frio", "não significativo"))
  list(executado = TRUE, celula_m = celula_m,
       grade = grade, inclui_self = TRUE, correcao_p = "BH",
       avisos = "P-valores bilaterais ajustados por Benjamini-Hochberg; a análise permanece exploratória.",
       mapa = gs_mapa_grade_classe(grade, "Getis-Ord (G*) por célula"))
}

# --- LISA (Moran local) — aglomerados alto-alto / baixo-baixo — requer spdep ----
gs_analise_lisa <- function(resultado, celula_m = gs_celula_hex_m,
                            ponto = NULL, raio_m = NULL) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = "Pacote 'spdep' não instalado. Instale com: install.packages('spdep')"))
  }
  grade <- gs_grade_hex(resultado, celula_m, ponto = ponto, raio_m = raio_m)
  if (!inherits(grade, "sf")) return(grade)
  if (nrow(grade) < 4) {
    return(list(executado = FALSE,
                mensagem = "Menos de 4 células no domínio para LISA."))
  }
  x <- as.numeric(grade$n_servicos)
  variancia <- stats::var(x)
  if (!is.finite(variancia) || variancia <= 0) {
    return(list(executado = FALSE,
                mensagem = "Contagens sem variância — LISA não é definido."))
  }
  nb <- tryCatch(spdep::poly2nb(sf::st_geometry(grade), queen = TRUE),
                 error = function(e) NULL)
  graus <- if (is.null(nb)) integer(0) else spdep::card(nb)
  if (length(graus) != nrow(grade) || sum(graus) == 0 || any(graus == 0)) {
    return(list(executado = FALSE,
                mensagem = "Vizinhança da grade inválida ou com células isoladas."))
  }
  lw <- tryCatch(spdep::nb2listw(nb, style = "W", zero.policy = FALSE),
                 error = function(e) NULL)
  if (is.null(lw)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível construir os pesos do LISA."))
  }
  lm <- tryCatch(spdep::localmoran(x, lw, zero.policy = FALSE,
                                   alternative = "two.sided"),
                 error = function(e) NULL)
  if (is.null(lm)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao executar LISA (pouca variância nas células)."))
  }
  coluna_p <- grep("^Pr\\(", colnames(lm), value = TRUE)[1]
  if (is.na(coluna_p)) {
    return(list(executado = FALSE,
                mensagem = "LISA não retornou p-valores locais."))
  }
  grade$lisa_i <- as.numeric(lm[, "Ii"])
  grade$lisa_z <- as.numeric(lm[, "Z.Ii"])
  grade$p_valor <- as.numeric(lm[, coluna_p])
  grade$p_valor[!is.finite(grade$p_valor)] <- 1
  grade$p_ajustado <- stats::p.adjust(grade$p_valor, method = "BH")
  grade$p_valor_ajustado <- grade$p_ajustado
  grade$valor_centrado <- x - mean(x)
  grade$lag_espacial <- tryCatch(as.numeric(spdep::lag.listw(
    lw, grade$valor_centrado, zero.policy = FALSE
  )), error = function(e) NULL)
  if (is.null(grade$lag_espacial) ||
      !any(is.finite(grade$lag_espacial))) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível calcular o lag espacial do LISA."))
  }
  grade$classe <- gs_classificar_lisa(
    grade$valor_centrado, grade$lag_espacial, grade$p_ajustado
  )
  list(executado = TRUE, celula_m = celula_m,
       grade = grade, correcao_p = "BH",
       avisos = "P-valores bilaterais ajustados por Benjamini-Hochberg; a análise permanece exploratória.",
       mapa = gs_mapa_grade_classe(grade, "LISA por célula"))
}

# --- Função K de Ripley — multiescala — requer spatstat -------------------------
gs_analise_ripley_k <- function(resultado, rmax_m = NULL, ponto = NULL,
                                raio_m = NULL) {
  if (!requireNamespace("spatstat.geom", quietly = TRUE) ||
      !requireNamespace("spatstat.explore", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = paste0(
                  "Pacotes 'spatstat.geom' e 'spatstat.explore' não instalados. ",
                  "Instale com: install.packages('spatstat')"
                )))
  }
  dominio <- gs_dominio_consulta(resultado, ponto, raio_m)
  if (!isTRUE(dominio$executado)) return(dominio)
  if (!is.data.frame(resultado) || nrow(resultado) < 4 ||
      !all(c("longitude", "latitude") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "São necessários ao menos 4 pontos para a função K."))
  }
  pts_sf <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                          crs = gs_epsg$wgs84)
  pts_utm <- sf::st_transform(pts_sf, gs_epsg$oficial)
  coords <- sf::st_coordinates(pts_utm)
  centro <- sf::st_coordinates(dominio$ponto)[1, ]
  win <- spatstat.geom::disc(
    radius = dominio$raio_m, centre = c(centro[1], centro[2])
  )
  dentro <- spatstat.geom::inside.owin(coords[, 1], coords[, 2], w = win)
  coords <- coords[dentro, , drop = FALSE]
  if (nrow(coords) < 4) {
    return(list(executado = FALSE,
                mensagem = "Menos de 4 pontos dentro do domínio da consulta."))
  }
  dup <- duplicated(as.data.frame(coords))
  if (any(dup)) {
    message("  ripley_k: removidos ", sum(dup), " pontos duplicados.")
    coords <- coords[!dup, , drop = FALSE]
  }
  if (nrow(coords) < 4) {
    return(list(executado = FALSE, mensagem = "Menos de 4 pontos para a função K."))
  }
  ppp <- spatstat.geom::ppp(x = coords[, 1], y = coords[, 2], window = win)
  if (!is.null(rmax_m)) {
    rmax_m <- suppressWarnings(as.numeric(rmax_m)[1])
    if (!is.finite(rmax_m) || rmax_m <= 0 || rmax_m > dominio$raio_m) {
      return(list(executado = FALSE,
                  mensagem = "`rmax_m` deve estar entre 0 e o raio da consulta."))
    }
  }
  K <- spatstat.explore::Kest(
    ppp, correction = c("border", "isotropic"), rmax = rmax_m
  )
  df <- as.data.frame(K)
  coluna <- if ("iso" %in% names(df)) "iso" else "border"
  df$l_menos_r <- sqrt(pmax(df[[coluna]], 0) / pi) - df$r
  grafico <- ggplot2::ggplot(df, ggplot2::aes(x = r, y = l_menos_r)) +
    ggplot2::geom_hline(yintercept = 0, color = "#d7301f", linetype = "dashed") +
    ggplot2::geom_line(color = "#0570b0", linewidth = 0.9) +
    ggplot2::labs(x = "Distância r (m)", y = "L(r) - r (m)",
                  title = "Função K de Ripley transformada",
                  subtitle = "Valores positivos sugerem agrupamento; negativos, dispersão") +
    ggplot2::theme_minimal()
  list(executado = TRUE, n = ppp$n, raio_dominio_m = dominio$raio_m,
       correcao = coluna, curva = df, objeto = K, grafico = grafico,
       avisos = paste0(
         "Diagnóstico exploratório multiescala; sem envelope de simulação, ",
         "a curva não constitui teste formal em cada distância."
       ))
}

# --- KDE com banda estimada (Silverman) ----------------------------------------
gs_analise_kde_banda <- function(resultado, ponto) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = "Pacote 'MASS' não instalado para estimar a banda da KDE."))
  }
  if (!is.data.frame(resultado) || nrow(resultado) < 3 ||
      !all(c("longitude", "latitude") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "São necessários ao menos 3 pontos para a KDE."))
  }
  pts <- tryCatch(sf::st_transform(
    sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                 crs = gs_epsg$wgs84), gs_epsg$oficial
  ), error = function(e) NULL)
  if (is.null(pts)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar os pontos para a KDE."))
  }
  xy <- sf::st_coordinates(pts)
  bx <- MASS::bandwidth.nrd(xy[, 1])
  by <- MASS::bandwidth.nrd(xy[, 2])
  if (!all(is.finite(c(bx, by))) || bx <= 0 || by <= 0) {
    return(list(executado = FALSE,
                mensagem = "Variação espacial insuficiente para estimar a banda da KDE."))
  }
  dados <- data.frame(x_m = xy[, 1], y_m = xy[, 2])
  ggplot2::ggplot(dados, ggplot2::aes(x = x_m, y = y_m)) +
    ggplot2::stat_density_2d(ggplot2::aes(fill = ggplot2::after_stat(density)),
                             geom = "raster", contour = FALSE, alpha = 0.8,
                             h = c(bx, by)) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::geom_point(color = "#d7301f", size = 1) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Densidade de kernel (banda estimada)",
      subtitle = sprintf("Ponto: %s | banda x = %.0f m; banda y = %.0f m",
                          ponto$origem, bx, by),
      x = "Coordenada leste (m)", y = "Coordenada norte (m)",
      fill = "Densidade"
    )
}

# --- Baixa (uma vez) a camada de distritos do GeoSampa -------------------------
gs_baixar_distritos <- function(dir = gs_pasta_dados(), force = FALSE) {
  cam <- gs_camadas_apoio$distritos
  path <- file.path(dir, paste0(cam, ".geojson"))
  if (!file.exists(path) || force) {
    gs_baixar_camada(cam, dir = dir, csv = TRUE, verbose = TRUE)
  }
  distritos <- sf::st_read(path, quiet = TRUE)
  # Geometrias do GeoSampa podem ter auto-interseções; corrige antes do uso.
  sf::st_make_valid(distritos)
}

# --- Distribuição por distrito no domínio efetivamente observado -------------
gs_distritos_no_dominio <- function(resultado, dir = gs_pasta_dados(),
                                    ponto = NULL, raio_m = NULL) {
  dominio <- gs_dominio_consulta(resultado, ponto, raio_m)
  if (!isTRUE(dominio$executado)) return(dominio)
  distritos <- tryCatch(gs_baixar_distritos(dir), error = function(e) NULL)
  if (is.null(distritos) || !inherits(distritos, "sf") ||
      !"nm_distrito_municipal" %in% names(distritos)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível carregar a camada válida de distritos."))
  }
  if (!is.data.frame(resultado) ||
      !all(c("longitude", "latitude") %in% names(resultado))) {
    return(list(executado = FALSE,
                mensagem = "Resultado sem coordenadas para o cruzamento distrital."))
  }
  distritos <- tryCatch(
    sf::st_make_valid(sf::st_transform(distritos, gs_epsg$oficial)),
    error = function(e) NULL
  )
  if (is.null(distritos)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar os distritos."))
  }
  distritos$.area_original_m2 <- as.numeric(sf::st_area(distritos))
  dist <- tryCatch(
    suppressWarnings(sf::st_intersection(
      distritos, sf::st_sf(.dominio = 1L, geometry = dominio$dominio)
    )),
    error = function(e) NULL
  )
  if (is.null(dist) || nrow(dist) == 0) {
    return(list(executado = FALSE,
                mensagem = "Nenhum distrito intersecta o domínio da consulta."))
  }
  area <- as.numeric(sf::st_area(dist))
  validos_dist <- is.finite(area) & area > 0 & !sf::st_is_empty(dist)
  dist <- dist[validos_dist, , drop = FALSE]
  area <- area[validos_dist]
  dist$area_observada_km2 <- area / 1e6
  dist$area_km2 <- dist$area_observada_km2
  dist$fracao_area_observada <- pmin(
    area / pmax(dist$.area_original_m2, 1), 1
  )

  lon <- suppressWarnings(as.numeric(resultado$longitude))
  lat <- suppressWarnings(as.numeric(resultado$latitude))
  validos <- is.finite(lon) & is.finite(lat)
  dados <- resultado[validos, , drop = FALSE]
  dados$longitude <- lon[validos]
  dados$latitude <- lat[validos]
  pts <- sf::st_transform(
    sf::st_as_sf(dados, coords = c("longitude", "latitude"),
                 crs = gs_epsg$wgs84),
    gs_epsg$oficial
  )
  candidatos <- sf::st_intersects(pts, dist)
  atribuicao <- vapply(candidatos, function(i) {
    if (length(i) == 0) return(NA_integer_)
    i[order(as.character(dist$nm_distrito_municipal[i]))][1]
  }, integer(1))
  dist$n_servicos <- tabulate(atribuicao[!is.na(atribuicao)], nbins = nrow(dist))
  dist$densidade_por_km2 <- dist$n_servicos / pmax(dist$area_observada_km2, 1e-9)
  dist$.area_original_m2 <- NULL
  dist$.dominio <- NULL
  list(
    executado = TRUE, distritos = dist,
    n_sem_distrito = sum(is.na(atribuicao)), raio_m = dominio$raio_m
  )
}

gs_analise_por_distrito <- function(resultado, dir = gs_pasta_dados(),
                                    ponto = NULL, raio_m = NULL) {
  cruzamento <- gs_distritos_no_dominio(resultado, dir, ponto, raio_m)
  if (!isTRUE(cruzamento$executado)) return(cruzamento)
  dist <- cruzamento$distritos
  mapa <- ggplot2::ggplot(dist) +
    ggplot2::geom_sf(ggplot2::aes(fill = densidade_por_km2), color = "white",
                     linewidth = 0.15) +
    ggplot2::scale_fill_viridis_c(na.value = "grey90") +
    gs_tema_mapa() +
    ggplot2::labs(
      title = "Densidade de serviços por distrito no domínio da consulta",
      fill = "Serviços/km²",
      caption = "Áreas distritais recortadas pelo raio da consulta"
    )
  list(executado = TRUE, raio_m = cruzamento$raio_m,
       n_sem_distrito = cruzamento$n_sem_distrito,
       por_distrito = sf::st_drop_geometry(dist), mapa = mapa)
}

# --- Moran's I da densidade nos distritos observados --------------------------
gs_analise_moran_distrital <- function(resultado, dir = gs_pasta_dados(),
                                       nsim = 999, seed = 123,
                                       ponto = NULL, raio_m = NULL) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = "Pacote 'spdep' não instalado. Instale com: install.packages('spdep')"))
  }
  nsim <- suppressWarnings(as.integer(nsim)[1])
  seed_valido <- is.null(seed) ||
    (length(seed) > 0 && is.finite(suppressWarnings(as.numeric(seed)[1])))
  if (!is.finite(nsim) || nsim < 1 || !seed_valido) {
    return(list(executado = FALSE,
                mensagem = "`nsim` e `seed` devem ser números inteiros válidos."))
  }
  cruzamento <- gs_distritos_no_dominio(resultado, dir, ponto, raio_m)
  if (!isTRUE(cruzamento$executado)) return(cruzamento)
  dist <- cruzamento$distritos
  x <- dist$densidade_por_km2
  variancia <- stats::var(x)
  if (nrow(dist) < 4 || !is.finite(variancia) || variancia <= 0) {
    return(list(executado = FALSE,
                mensagem = paste0(
                  "Menos de 4 distritos observados ou sem variação de densidade ",
                  "— insuficiente para Moran distrital."
                )))
  }
  nb <- tryCatch(spdep::poly2nb(sf::st_geometry(dist), queen = TRUE),
                 error = function(e) NULL)
  graus <- if (is.null(nb)) integer(0) else spdep::card(nb)
  if (length(graus) != nrow(dist) || sum(graus) == 0 || any(graus == 0)) {
    return(list(executado = FALSE,
                mensagem = "Vizinhança distrital inválida ou com distritos isolados."))
  }
  lw <- tryCatch(spdep::nb2listw(nb, style = "W", zero.policy = FALSE),
                 error = function(e) NULL)
  if (is.null(lw)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível construir os pesos distritais."))
  }
  teste <- tryCatch(gs_com_seed(seed, spdep::moran.mc(
    x, lw, nsim = nsim, zero.policy = FALSE,
    alternative = "two.sided"
  )), error = function(e) NULL)
  if (is.null(teste) || !is.finite(unname(teste$statistic)) ||
      !is.finite(teste$p.value)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao executar Moran distrital (vizinhança inválida)."))
  }
  lm <- tryCatch(spdep::localmoran(
    x, lw, zero.policy = FALSE, alternative = "two.sided"
  ), error = function(e) NULL)
  if (is.null(lm)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao executar LISA distrital."))
  }
  coluna_p <- grep("^Pr\\(", colnames(lm), value = TRUE)[1]
  if (is.na(coluna_p)) {
    return(list(executado = FALSE,
                mensagem = "LISA distrital não retornou p-valores locais."))
  }
  dist$lisa_i <- as.numeric(lm[, "Ii"])
  dist$lisa_z <- as.numeric(lm[, "Z.Ii"])
  dist$p_valor <- as.numeric(lm[, coluna_p])
  dist$p_valor[!is.finite(dist$p_valor)] <- 1
  dist$p_ajustado <- stats::p.adjust(dist$p_valor, method = "BH")
  dist$p_valor_ajustado <- dist$p_ajustado
  dist$valor_centrado <- x - mean(x)
  dist$lag_espacial <- tryCatch(as.numeric(spdep::lag.listw(
    lw, dist$valor_centrado, zero.policy = FALSE
  )), error = function(e) NULL)
  if (is.null(dist$lag_espacial) || !any(is.finite(dist$lag_espacial))) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível calcular o lag espacial distrital."))
  }
  dist$classe <- gs_classificar_lisa(
    dist$valor_centrado, dist$lag_espacial, dist$p_ajustado
  )
  mapa <- ggplot2::ggplot(dist) +
    ggplot2::geom_sf(ggplot2::aes(fill = classe), color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(
      values = c("alto-alto" = "#d7301f", "baixo-baixo" = "#0570b0",
                 "alto-baixo" = "#fc8d59", "baixo-alto" = "#74add1",
                 "não significativo" = "grey85")) +
    gs_tema_mapa() +
    ggplot2::labs(
      title = "Moran local da densidade distrital no domínio", fill = "Classe"
    )
  list(executado = TRUE,
       moran_i = unname(teste$statistic),
       valor_p = teste$p.value,
       metodo_p = "Monte Carlo bilateral", nsim = nsim, seed = seed,
       variavel = "densidade_por_km2", correcao_p_local = "BH",
       n_distritos = nrow(dist), raio_m = cruzamento$raio_m,
       por_distrito = sf::st_drop_geometry(dist),
       mapa = mapa, objeto = teste)
}

# --- Cobertura populacional: população dentro do raio de busca ------------------
# Fontes: (a) `pop_layer` — objeto sf com coluna `populacao` e geometria
# poligonal (ex.: setores censitários do IBGE unidos com os dados do Censo);
# (b) `densidade_km2` — densidade média estimada (hab/km²) para estimativa.
gs_analise_cobertura_populacional <- function(resultado, ponto = NULL,
                                               raio_m = NULL,
                                               pop_layer = NULL,
                                               densidade_km2 = NULL) {
  if (is.null(pop_layer) && is.null(densidade_km2)) {
    return(list(executado = FALSE,
      mensagem = paste0(
        "Nenhuma fonte de população informada. Forneça `pop_layer` ",
         "(objeto sf com coluna `populacao`, ex.: setores censitários do IBGE) ",
         "ou `densidade_km2` (hab/km²) para estimar a população no raio.")))
  }
  dominio <- gs_dominio_consulta(resultado, ponto, raio_m)
  if (!isTRUE(dominio$executado)) return(dominio)
  area_km2 <- as.numeric(sf::st_area(dominio$dominio)) / 1e6
  if (!is.null(densidade_km2)) {
    densidade_km2 <- suppressWarnings(as.numeric(densidade_km2)[1])
    if (!is.finite(densidade_km2) || densidade_km2 < 0) {
      return(list(executado = FALSE,
                  mensagem = "`densidade_km2` deve ser um número não negativo."))
    }
    return(list(executado = TRUE, metodo = "densidade",
                raio_m = dominio$raio_m,
                area_km2 = round(area_km2, 2), densidade_km2 = densidade_km2,
                populacao_estimada = round(densidade_km2 * area_km2)))
  }
  if (!inherits(pop_layer, "sf")) {
    return(list(executado = FALSE,
                mensagem = "`pop_layer` deve ser um objeto sf."))
  }
  if (!"populacao" %in% names(pop_layer)) {
    return(list(executado = FALSE,
                mensagem = "`pop_layer` precisa de uma coluna chamada 'populacao'."))
  }
  pop_t <- tryCatch(
    sf::st_make_valid(sf::st_transform(pop_layer, gs_epsg$oficial)),
    error = function(e) NULL
  )
  if (is.null(pop_t) || nrow(pop_t) == 0) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível projetar a camada populacional."))
  }
  pop_t$.gs_pop_id <- seq_len(nrow(pop_t))
  area_orig <- suppressWarnings(as.numeric(sf::st_area(pop_t)))
  geometrias_validas <- !sf::st_is_empty(pop_t) & is.finite(area_orig) & area_orig > 0
  pop_t <- pop_t[geometrias_validas, , drop = FALSE]
  area_orig <- area_orig[geometrias_validas]
  if (nrow(pop_t) == 0) {
    return(list(executado = FALSE,
                mensagem = "A camada populacional não possui geometrias com área válida."))
  }
  inter <- tryCatch(
    suppressWarnings(sf::st_intersection(
      pop_t, sf::st_sf(.dominio = 1L, geometry = dominio$dominio)
    )),
    error = function(e) NULL
  )
  if (is.null(inter)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao cruzar população e buffer da consulta."))
  }
  if (nrow(inter) == 0) {
    return(list(executado = TRUE, metodo = "pop_layer",
                raio_m = dominio$raio_m,
                area_km2 = round(area_km2, 2), populacao_atendida = 0,
                n_unidades = 0, n_populacao_na = 0))
  }
  inter$area_orig_m2 <- area_orig[match(inter$.gs_pop_id, pop_t$.gs_pop_id)]
  inter$area_piece_m2 <- suppressWarnings(as.numeric(sf::st_area(inter)))
  fracao <- pmin(pmax(inter$area_piece_m2 / inter$area_orig_m2, 0), 1)
  fracao[!is.finite(fracao)] <- 0
  populacao <- suppressWarnings(as.numeric(as.character(inter$populacao)))
  populacao_valida <- is.finite(populacao)
  pop <- sum(ifelse(populacao_valida, populacao * fracao, 0), na.rm = TRUE)
  n_pop_na <- length(unique(inter$.gs_pop_id[!populacao_valida]))
  list(executado = TRUE, metodo = "pop_layer", raio_m = dominio$raio_m,
       area_km2 = round(area_km2, 2),
       area_intersectada_km2 = round(sum(inter$area_piece_m2, na.rm = TRUE) / 1e6, 2),
       populacao_atendida = round(pop),
       n_unidades = length(unique(inter$.gs_pop_id)),
       n_populacao_na = n_pop_na)
}

# --- Função principal: executa as análises escolhidas -------------------------
# Aceita vários tipos de uma vez (ex.: c("descritivas", "voronoi")) e devolve
# uma lista nomeada. Se `resultado` não for informado, calcula-o a partir dos
# demais argumentos (mesmos de gs_servicos_proximos).
gs_analise_servicos <- function(resultado = NULL, cep = NULL, coordenadas = NULL,
                                 camadas = NULL, raio_m = gs_raio_padrao_m,
                                 n_por_camada = NULL,
                                tipo_distancia = c("geodesica", "euclidiana",
                                                   "haversine", "manhattan",
                                                   "rede_viaria"),
                                tipo = c("descritivas", "vizinho_mais_proximo",
                                         "voronoi", "kde", "kde_banda",
                                         "raios_progressivos", "moran",
                                         "moran_distrital", "rede_viaria",
                                         "acessibilidade_media", "cobertura_buffer",
                                          "raio_otimo", "nni", "getis_ord",
                                          "lisa", "ripley_k", "por_distrito",
                                          "cobertura_populacional"),
                                 dir = gs_caminho_dados(), pop_layer = NULL,
                                 densidade_km2 = NULL) {
  tipo <- match.arg(tipo, several.ok = TRUE)
  if (is.null(resultado)) {
    resultado <- gs_servicos_proximos(
      cep = cep, coordenadas = coordenadas, camadas = camadas,
      raio_m = raio_m, n_por_camada = n_por_camada,
      tipo_distancia = tipo_distancia, dir = dir
    )
  }
  ponto <- attr(resultado, "ponto")
  if (is.null(ponto)) {
    stop("`resultado` não tem o atributo 'ponto'. Use a saída de gs_servicos_proximos().")
  }
  raio <- attr(resultado, "raio_m")
  if (is.null(raio)) raio <- raio_m

  segura <- function(expr, nome) {
    tryCatch(
      force(expr),
      error = function(e) list(
        executado = FALSE,
        mensagem = paste0("Falha na análise '", nome, "': ", conditionMessage(e))
      )
    )
  }
  saida <- list()
  if ("descritivas" %in% tipo) saida$descritivas <-
    segura(gs_analise_descritivas(resultado), "descritivas")
  if ("vizinho_mais_proximo" %in% tipo) saida$vizinho_mais_proximo <-
    segura(gs_analise_vizinho(resultado), "vizinho_mais_proximo")
  if ("voronoi" %in% tipo) saida$voronoi <-
    segura(gs_analise_voronoi(resultado, ponto, raio), "voronoi")
  if ("kde" %in% tipo) saida$kde <-
    segura(gs_analise_kde(resultado, ponto), "kde")
  if ("kde_banda" %in% tipo) saida$kde_banda <-
    segura(gs_analise_kde_banda(resultado, ponto), "kde_banda")
  if ("raios_progressivos" %in% tipo) saida$raios_progressivos <-
    segura(gs_analise_raios(resultado, ponto), "raios_progressivos")
  if ("moran" %in% tipo) saida$moran <-
    segura(gs_analise_moran(resultado, ponto = ponto, raio_m = raio), "moran")
  if ("rede_viaria" %in% tipo) saida$rede_viaria <-
    segura(gs_analise_rede(resultado, ponto), "rede_viaria")
  if ("acessibilidade_media" %in% tipo) saida$acessibilidade_media <-
    segura(gs_analise_acessibilidade(resultado), "acessibilidade_media")
  if ("cobertura_buffer" %in% tipo) saida$cobertura_buffer <-
    segura(gs_analise_cobertura(resultado, ponto, raio_m = raio), "cobertura_buffer")
  if ("raio_otimo" %in% tipo) saida$raio_otimo <-
    segura(gs_analise_raio_otimo(resultado), "raio_otimo")
  if ("nni" %in% tipo) saida$nni <-
    segura(gs_analise_nni(resultado, ponto, raio), "nni")
  if ("getis_ord" %in% tipo) saida$getis_ord <-
    segura(gs_analise_getis_ord(resultado, ponto = ponto, raio_m = raio), "getis_ord")
  if ("lisa" %in% tipo) saida$lisa <-
    segura(gs_analise_lisa(resultado, ponto = ponto, raio_m = raio), "lisa")
  if ("ripley_k" %in% tipo) saida$ripley_k <-
    segura(gs_analise_ripley_k(resultado, ponto = ponto, raio_m = raio), "ripley_k")
  if ("por_distrito" %in% tipo) saida$por_distrito <-
    segura(gs_analise_por_distrito(resultado, dir = dir), "por_distrito")
  if ("moran_distrital" %in% tipo) saida$moran_distrital <-
    segura(gs_analise_moran_distrital(resultado, dir = dir), "moran_distrital")
  if ("cobertura_populacional" %in% tipo) saida$cobertura_populacional <-
    segura(gs_analise_cobertura_populacional(
      resultado, ponto, raio, pop_layer = pop_layer,
      densidade_km2 = densidade_km2
    ), "cobertura_populacional")
  saida
}

# --- Interpretação automática das análises (usada no relatório) ----------------
# Gera, para cada análise disponível em `analises`, um parágrafo curto em
# português com a leitura dos principais resultados — deixando as análises
# "corretas e bem explicadas". `resultado` é o data.frame de
# gs_servicos_proximos(); `raio_m` é o raio de busca usado.
gs_interpretar_analise <- function(analises, resultado, raio_m) {
  out <- list()
  if (is.null(analises) || is.null(resultado)) return(out)
  fmt <- function(...) sprintf(...)
  executada <- function(x) {
    !is.null(x) && !(is.list(x) && identical(x[["executado"]], FALSE))
  }
  d <- resultado$distancia_m

  if (!is.null(analises$descritivas) &&
      !identical(analises$descritivas$executado, FALSE)) {
    r <- analises$descritivas$estatisticas_distancia
    if (is.null(r)) r <- as.data.frame(as.list(gs_resumo_distancias(d)))
    ic <- if (isTRUE(is.finite(r$ic95_media_inf) &&
                     is.finite(r$ic95_media_sup))) {
      fmt(" (IC95%% %.0f–%.0f m)", r$ic95_media_inf, r$ic95_media_sup)
    } else {
      ""
    }
    out$descritivas <- fmt(
      "Foram encontrados %d serviço(s) num raio de %d m. As distâncias vão de %.0f m a %.0f m, com mediana de %.0f m, MAD de %.0f m e IQR de %.0f m (P25 %.0f m; P75 %.0f m). A média é %.0f m%s.",
      analises$descritivas$n_total, raio_m,
      r$min, r$max, r$mediana, r$mad, r$iqr, r$p25, r$p75, r$media, ic)
  }

  if (!is.null(analises$vizinho_mais_proximo)) {
    v <- analises$vizinho_mais_proximo$vizinho_mais_proximo
    if (!is.null(v) && nrow(v) > 0) {
      out$vizinho_mais_proximo <- fmt(
        "O serviço mais próximo é '%s' (camada %s), a %.0f m do ponto de interesse.",
        v$nome[1], v$camada[1], v$distancia_m[1])
    }
  }

  if (!is.null(analises$acessibilidade_media) &&
      !identical(analises$acessibilidade_media$executado, FALSE) &&
      !is.null(analises$acessibilidade_media$geral)) {
    g <- analises$acessibilidade_media$geral
    ic <- if (isTRUE(is.finite(g$ic95_media_inf) &&
                     is.finite(g$ic95_media_sup))) {
      fmt("; IC95%% %.0f–%.0f m", g$ic95_media_inf, g$ic95_media_sup)
    } else {
      ""
    }
    out$acessibilidade_media <- fmt(
      "Acessibilidade: mediana de %.0f m, MAD de %.0f m, P25 de %.0f m e P75 de %.0f m (IQR = %.0f m). A média é %.0f m com desvio-padrão de %.0f m (CV = %.1f%%%s).",
      g$mediana, g$mad, g$p25, g$p75, g$iqr, g$media, g$sd, g$cv, ic)
  }

  if (!is.null(analises$raio_otimo) && !is.null(analises$raio_otimo$percentis)) {
    p <- analises$raio_otimo$percentis
    out$raio_otimo <- fmt(
      "Para alcançar %s dos serviços é preciso um raio de %s, respectivamente.",
      paste0(p$percentil, collapse = ", "),
      paste0(round(p$raio_m, 0), " m", collapse = ", "))
  }

  if (!is.null(analises$raios_progressivos) &&
      !is.null(analises$raios_progressivos$contagem)) {
    cg <- analises$raios_progressivos$contagem
    ult <- cg[nrow(cg), ]
    out$raios_progressivos <- fmt(
      "Com um raio de %d m são alcançados %d serviço(s). O acréscimo de oportunidades conforme o raio cresce foi: %s.",
      ult$raio_m, ult$n_servicos,
      paste0("+", diff(c(0, cg$n_servicos)), " aos ", cg$raio_m, " m",
             collapse = "; "))
  }

  if (!is.null(analises$cobertura_buffer) &&
      isTRUE(analises$cobertura_buffer$executado)) {
    cb <- analises$cobertura_buffer
    out$cobertura_buffer <- fmt(
      "Os buffers de %d m ao redor dos serviços cobrem %.1f%% da área do buffer da consulta (%.2f de %.2f km²).",
      cb$raio_buffer_m, cb$pct_cobertura, cb$area_coberta_km2,
      cb$area_consulta_km2)
  }

  if (!is.null(analises$nni) && isTRUE(analises$nni$executado)) {
    n <- analises$nni
    out$nni <- fmt(
      "Índice de Vizinho Mais Próximo: R = %.2f (%s) com z = %.2f e p = %.3f. A distância média observada ao vizinho mais próximo é %.1f m (esperada: %.1f m).",
      n$indice_nni, n$interpretacao, n$z, n$valor_p,
      n$distancia_observada_m, n$distancia_esperada_m)
  }

  # Nota: usa [[ ]] (não $) para evitar partial matching — "$moran" casaria
  # também com "moran_distrital" e "$kde" com "kde_banda".
  if (!is.null(analises[["moran"]]) && isTRUE(analises[["moran"]]$executado)) {
    m <- analises[["moran"]]
    out$moran <- fmt(
      "Moran's I (método: %s): I = %.4f, p = %.4f. %s",
      m$metodo, m$moran_i, m$valor_p, m$interpretacao)
  }

  if (!is.null(analises[["moran_distrital"]]) &&
      isTRUE(analises[["moran_distrital"]]$executado)) {
    md <- analises[["moran_distrital"]]
    n_lisa <- sum(md$por_distrito$classe %in%
                    c("alto-alto", "baixo-baixo", "alto-baixo", "baixo-alto"),
                  na.rm = TRUE)
    out$moran_distrital <- fmt(
      "Moran's I agregado por distrito: I = %.4f, p = %.4f. %d distrito(s) foram sinalizados em quadrantes LISA após ajuste BH.",
      md$moran_i, md$valor_p, n_lisa)
  }

  if (!is.null(analises[["getis_ord"]]) && isTRUE(analises[["getis_ord"]]$executado)) {
    g <- analises[["getis_ord"]]
    out$getis_ord <- fmt(
      "Getis-Ord G* em grade hexagonal de %d m: %d célula(s) de ponto quente e %d de ponto frio após ajuste BH bilateral. Trate como exploratório.",
      g$celula_m, sum(g$grade$classe == "ponto quente"),
      sum(g$grade$classe == "ponto frio"))
  }

  if (!is.null(analises[["lisa"]]) && isTRUE(analises[["lisa"]]$executado)) {
    l <- analises[["lisa"]]
    out$lisa <- fmt(
      "LISA (Moran local) em grade hexagonal de %d m: %d 'alto-alto', %d 'baixo-baixo', %d 'alto-baixo' e %d 'baixo-alto' após ajuste BH. Trate como exploratório.",
      l$celula_m, sum(l$grade$classe == "alto-alto"),
      sum(l$grade$classe == "baixo-baixo"),
      sum(l$grade$classe == "alto-baixo"),
      sum(l$grade$classe == "baixo-alto"))
  }

  if (!is.null(analises[["por_distrito"]]) && isTRUE(analises[["por_distrito"]]$executado)) {
    pd <- analises[["por_distrito"]]$por_distrito
    if (nrow(pd) > 0) {
      top <- pd[which.max(pd$n_servicos), , drop = FALSE]
      coluna_distrito <- intersect(c("distrito", "nm_distrito_municipal"),
                                   names(top))[1]
      nome_distrito <- if (length(coluna_distrito) == 1 &&
                           !is.na(coluna_distrito)) {
        as.character(top[[coluna_distrito]][1])
      } else {
        "(não informado)"
      }
      out$por_distrito <- fmt(
        "Serviços distribuídos em %d distrito(s) da cidade. O distrito com mais serviços é %s (%d serviço(s), densidade de %.2f por km²).",
        sum(pd$n_servicos > 0), nome_distrito, top$n_servicos,
        top$densidade_por_km2)
    }
  }

  if (!is.null(analises[["cobertura_populacional"]]) &&
      isTRUE(analises[["cobertura_populacional"]]$executado)) {
    cp <- analises[["cobertura_populacional"]]
    out$cobertura_populacional <- if (identical(cp$metodo, "densidade")) {
      fmt("População estimada dentro do raio de %d m: ~%.0f habitantes (área de %.2f km² a %.0f hab/km²).",
          cp$raio_m, cp$populacao_estimada, cp$area_km2, cp$densidade_km2)
    } else {
      fmt("População atendida dentro do raio de %d m: ~%.0f habitantes (área de %.2f km²).",
          cp$raio_m, cp$populacao_atendida, cp$area_km2)
    }
  }

  if (!is.null(analises[["rede_viaria"]]) && isTRUE(analises[["rede_viaria"]]$executado)) {
    rv <- analises[["rede_viaria"]]$resultado
    out$rede_viaria <- fmt(
      "Em média, a distância por rede viária é %.2f× a distância em linha reta (mediana de %.2f×) para %d serviço(s).",
      round(mean(rv$razao_rede_reta), 2),
      round(stats::median(rv$razao_rede_reta), 2), nrow(rv))
  }

  if (executada(analises[["kde"]])) {
    out$kde <- paste0(
      "O mapa de densidade de kernel mostra as áreas de maior concentração de ",
      "serviços: quanto mais quente a cor, maior a concentração local.")
  }

  if (executada(analises[["kde_banda"]])) {
    out$kde_banda <- paste0(
      "O mapa de densidade de kernel com banda estimada (Silverman) mostra as ",
      "áreas de maior concentração de serviços, com suavização ajustada aos dados.")
  }

  if (executada(analises[["voronoi"]])) {
    out$voronoi <- paste0(
      "Os polígonos de Voronoi (Thiessen) delimitam, para cada serviço, a área ",
      "em que ele é o mais próximo — uma medida simples de zona de influência.")
  }

  if (!is.null(analises$ripley_k) && isTRUE(analises$ripley_k$executado)) {
    out$ripley_k <- paste0(
      "A função K de Ripley compara a agregação observada com o padrão aleatório ",
      "em múltiplas escalas: quando a curva observada fica acima da esperada, ",
      "há agrupamento naquela escala.")
  }

  out
}
