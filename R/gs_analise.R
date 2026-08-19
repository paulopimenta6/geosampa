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

# --- Descritivas: contagens e distribuição das distâncias --------------------
gs_analise_descritivas <- function(resultado) {
  n_por_camada <- as.data.frame(table(resultado$camada))
  names(n_por_camada) <- c("camada", "n")
  rownames(n_por_camada) <- NULL

  n_por_tipo <- NULL
  if ("tipo_servico" %in% names(resultado) && !all(is.na(resultado$tipo_servico))) {
    n_por_tipo <- as.data.frame(table(resultado$tipo_servico))
    names(n_por_tipo) <- c("tipo_servico", "n")
    rownames(n_por_tipo) <- NULL
  }

  mediana <- stats::median(resultado$distancia_m)
  media   <- mean(resultado$distancia_m)

  list(
    n_total          = nrow(resultado),
    n_por_camada     = n_por_camada,
    n_por_tipo       = n_por_tipo,
    resumo_distancia = summary(resultado$distancia_m),
    histograma = ggplot2::ggplot(resultado, ggplot2::aes(x = distancia_m)) +
      ggplot2::geom_histogram(bins = 20, fill = "#2c7fb8", color = "white") +
      ggplot2::geom_vline(xintercept = mediana, color = "#d7301f",
                          linetype = "dashed", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = media, color = "#0570b0",
                          linewidth = 0.8) +
      ggplot2::labs(x = "Distância (m)", y = "Nº de serviços",
                    title = "Distribuição das distâncias",
                    caption = "Vermelho tracejado: mediana | Azul: média") +
      ggplot2::theme_minimal(),
    boxplot = ggplot2::ggplot(resultado, ggplot2::aes(x = camada, y = distancia_m)) +
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
  por_camada <- do.call(rbind, lapply(split(resultado, resultado$camada), function(d) {
    i <- which.min(d$distancia_m)
    data.frame(camada = d$camada[i], nome = d$nome[i],
               distancia_m = d$distancia_m[i], stringsAsFactors = FALSE)
  }))
  rownames(por_camada) <- NULL
  i <- which.min(resultado$distancia_m)
  list(
    vizinho_mais_proximo = resultado[i, , drop = FALSE],
    por_camada           = por_camada
  )
}

# --- Voronoi/Thiessen: áreas de influência dos serviços -----------------------
# Devolve um sf de polígonos (em EPSG:4326) recortado pela bbox ao redor do
# ponto. O cálculo é feito na projeção oficial (metros) — st_voronoi não
# funciona sobre coordenadas geográficas.
gs_analise_voronoi <- function(resultado, ponto) {
  pts <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                      crs = gs_epsg$wgs84)
  pts_utm <- sf::st_transform(pts, gs_epsg$oficial)
  ponto_utm <- sf::st_transform(ponto$sf, gs_epsg$oficial)
  bbox <- sf::st_bbox(sf::st_buffer(ponto_utm, 5000))
  envelope <- sf::st_as_sfc(bbox)
  vor <- sf::st_voronoi(sf::st_geometry(pts_utm), envelope = envelope)
  pol <- sf::st_collection_extract(vor, "POLYGON")
  pol <- sf::st_sf(geometry = pol)
  pol <- sf::st_intersection(pol, envelope)
  pol <- sf::st_transform(pol, gs_epsg$wgs84)
  pol$camada       <- pts$camada
  pol$nome         <- pts$nome
  pol$distancia_m  <- pts$distancia_m
  pol
}

# --- Densidade de kernel dos serviços -----------------------------------------
gs_analise_kde <- function(resultado, ponto) {
  ggplot2::ggplot(resultado, ggplot2::aes(x = longitude, y = latitude)) +
    ggplot2::stat_density_2d(ggplot2::aes(fill = ggplot2::after_stat(density)),
                             geom = "raster", contour = FALSE, alpha = 0.8) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::geom_point(color = "#d7301f", size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Densidade de kernel dos serviços",
      subtitle = sprintf("Ponto: %s", ponto$origem)
    )
}

# --- Raios progressivos: oportunidades acumuladas ------------------------------
# Devolve a contagem acumulada por raio (tabela) e a curva correspondente.
gs_analise_raios <- function(resultado, ponto, raios = c(500, 1000, 2000)) {
  contagem <- data.frame(
    raio_m = raios,
    n_servicos = vapply(raios, function(r) sum(resultado$distancia_m <= r),
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
  list(contagem = contagem, grafico = grafico)
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
                             sobre_grade = TRUE) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    return(list(
      executado = FALSE,
      mensagem = "Pacote 'spdep' não instalado. Instale com: install.packages('spdep')"
    ))
  }

  if (sobre_grade) {
    grade <- gs_grade_hex(resultado, celula_m)
    ocupadas <- grade[!is.na(grade$camadas), , drop = FALSE]
    if (nrow(ocupadas) < 4) {
      return(list(executado = FALSE,
                  mensagem = "Menos de 4 células com serviços para Moran's I sobre a grade hexagonal."))
    }
    nb <- spdep::poly2nb(sf::st_geometry(ocupadas), queen = TRUE)
    lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
    teste <- tryCatch(
      spdep::moran.test(ocupadas$n_servicos, lw, zero.policy = TRUE),
      error = function(e) NULL
    )
    if (is.null(teste)) {
      return(list(executado = FALSE,
                  mensagem = "Falha ao executar Moran's I (configuração de vizinhança inválida)."))
    }
    moran_i <- unname(teste$estimate["Moran I statistic"])
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
      n_celulas = nrow(ocupadas), interpretacao = interpretacao,
      objeto = teste
    ))
  }

  # --- Versão DIAGNÓSTICA: Moran sobre a distância radial ----------------------
  chave <- sprintf("%.6f|%.6f", resultado$longitude, resultado$latitude)
  dup   <- duplicated(chave)
  d     <- resultado[!dup, , drop = FALSE]
  n_dup <- sum(dup)
  if (n_dup > 0) {
    message("  moran: removidas ", n_dup, " ocorrências com coordenadas ",
            "idênticas para estabilizar o teste (restam ", nrow(d), " pontos).")
  }
  if (nrow(d) < 4) {
    return(list(executado = FALSE,
                mensagem = "Menos de 4 pontos distintos — número insuficiente para Moran's I."))
  }
  coords <- as.matrix(d[, c("longitude", "latitude")])
  nb <- spdep::knn2nb(spdep::knearneigh(coords, k = min(5, nrow(d) - 1)))
  ncomp <- spdep::n.comp.nb(nb)
  avisos <- character(0)
  if (ncomp$nc > 1) {
    avisos <- c(avisos, sprintf(
      "Vizinhança com %d sub-grafo(s) desconexo(s); os resultados locais nesses sub-grafos não são confiáveis.",
      ncomp$nc))
    message("  moran: ", avisos[length(avisos)])
  }
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  teste <- tryCatch(
    spdep::moran.mc(d$distancia_m, lw, nsim = 999, zero.policy = TRUE),
    error = function(e) NULL
  )
  if (is.null(teste)) {
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
  options(osrm.server = gs_osrm_server(),
              osrm.profile = gs_osrm_profile())
  origem <- gs_osrm_input("origem", ponto$longitude, ponto$latitude)
  destinos <- gs_osrm_input(as.character(seq_len(nrow(resultado))),
                            resultado$longitude, resultado$latitude)
  if (nrow(resultado) > 200) {
    message("  rede viária: calculando distâncias para ", nrow(resultado),
            " destinos via OSRM (pode demorar)...")
  }
  tab <- tryCatch(
    osrm::osrmTable(src = origem, dst = destinos, measure = "distance"),
    error = function(e) NULL
  )
  if (is.null(tab)) {
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
  dist_rede_m <- gs_osrm_dist_m(tab$distances[1, ])
  out <- data.frame(
    camada            = resultado$camada,
    nome              = resultado$nome,
    distancia_reta_m  = resultado$distancia_m,
    distancia_rede_m  = dist_rede_m,
    razao_rede_reta   = round(dist_rede_m / pmax(resultado$distancia_m, 1), 2),
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
  resumo <- function(x) {
    x <- x[!is.na(x)]
    q25 <- unname(stats::quantile(x, 0.25))
    q75 <- unname(stats::quantile(x, 0.75))
    media <- mean(x)
    sd <- stats::sd(x)
    c(n = length(x), min = min(x), p25 = q25, mediana = unname(stats::median(x)),
      media = media, p75 = q75, max = max(x), iqr = q75 - q25,
      sd = sd, cv = round(100 * sd / max(media, 1e-9), 1))
  }
  geral <- as.data.frame(t(resumo(resultado$distancia_m)))
  por_camada <- do.call(rbind, lapply(split(resultado, resultado$camada), function(d) {
    data.frame(t(resumo(d$distancia_m)))
  }))
  por_camada <- cbind(camada = rownames(por_camada), as.data.frame(por_camada))
  rownames(por_camada) <- NULL
  por_tipo <- NULL
  if ("tipo_servico" %in% names(resultado) && !all(is.na(resultado$tipo_servico))) {
    por_tipo <- do.call(rbind, lapply(split(resultado, resultado$tipo_servico), function(d) {
      data.frame(t(resumo(d$distancia_m)))
    }))
    por_tipo <- cbind(tipo_servico = rownames(por_tipo), as.data.frame(por_tipo))
    rownames(por_tipo) <- NULL
  }
  qs <- stats::quantile(resultado$distancia_m, c(0.25, 0.5, 0.75))
  grafico_ecdf <- ggplot2::ggplot(resultado, ggplot2::aes(x = distancia_m)) +
    ggplot2::stat_ecdf(geom = "step", color = "#2c7fb8", linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = unname(qs), linetype = "dashed",
                        color = "grey55") +
    ggplot2::labs(x = "Distância (m)", y = "Proporção acumulada de serviços",
                  title = "Curva acumulada das distâncias (ECDF)",
                  subtitle = "Linhas tracejadas: P25, mediana e P75") +
    ggplot2::theme_minimal()
  list(geral = geral, por_camada = por_camada, por_tipo = por_tipo,
       grafico_ecdf = grafico_ecdf)
}

# --- Cobertura por buffer: área coberta pelos buffers dos serviços ------------
# Calcula, por camada (e no geral), a área coberta pela união de buffers de
# `raio_buffer_m` ao redor de cada serviço, comparada à área do casco convexo
# (hull) dos pontos — uma medida simples de cobertura sem dados externos.
gs_analise_cobertura <- function(resultado, ponto, raio_buffer_m = gs_raio_buffer_m) {
  pts <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                      crs = gs_epsg$wgs84)
  pts_utm <- sf::st_transform(pts, gs_epsg$oficial)
  # deduplica coordenadas (mesmo prédio) para acelerar as operações
  coords <- round(sf::st_coordinates(pts_utm), 1)
  pts_utm <- pts_utm[!duplicated(coords), , drop = FALSE]
  if (nrow(pts_utm) == 0) {
    return(list(executado = FALSE, mensagem = "Nenhum ponto para calcular cobertura."))
  }
  hull <- sf::st_convex_hull(sf::st_union(pts_utm))
  area_hull <- as.numeric(sf::st_area(hull))
  por_camada <- lapply(split(seq_len(nrow(pts_utm)), pts_utm$camada), function(i) {
    un <- sf::st_union(sf::st_buffer(pts_utm[i, , drop = FALSE], raio_buffer_m))
    area <- as.numeric(sf::st_area(un))
    area_int <- as.numeric(sf::st_area(sf::st_intersection(un, hull)))
    data.frame(camada = pts_utm$camada[i[1]], n = length(i),
               area_coberta_km2 = round(area / 1e6, 2),
               area_no_hull_km2 = round(area_int / 1e6, 2),
               pct_hull = round(100 * area_int / max(area_hull, 1), 2))
  })
  por_camada <- do.call(rbind, por_camada)
  rownames(por_camada) <- NULL
  un_todas <- sf::st_union(sf::st_buffer(pts_utm, raio_buffer_m))
  area_todas <- as.numeric(sf::st_area(sf::st_intersection(un_todas, hull)))
  list(
    executado = TRUE,
    raio_buffer_m = raio_buffer_m,
    area_hull_km2 = round(area_hull / 1e6, 2),
    area_coberta_km2 = round(area_todas / 1e6, 2),
    pct_cobertura = round(100 * area_todas / max(area_hull, 1), 2),
    por_camada = por_camada
  )
}

# --- Raio ótimo: percentis da distribuição das distâncias ----------------------
# O menor raio que "alcança" X% dos serviços (quantis da distância), com o
# gráfico ECDF que permite ver visualmente o percentil correspondente a cada
# raio.
gs_analise_raio_otimo <- function(resultado, p = c(0.5, 0.75, 0.9, 0.95)) {
  q <- stats::quantile(resultado$distancia_m, probs = p)
  percentis <- data.frame(percentil = names(q), raio_m = unname(round(q, 0)))
  grafico <- ggplot2::ggplot(resultado, ggplot2::aes(x = distancia_m)) +
    ggplot2::stat_ecdf(geom = "step", color = "#2c7fb8", linewidth = 0.9) +
    ggplot2::geom_hline(yintercept = as.numeric(p), linetype = "dashed",
                        color = "grey50") +
    ggplot2::geom_vline(xintercept = unname(q), linetype = "dotted",
                        color = "#d7301f") +
    ggplot2::labs(x = "Distância (m)", y = "Proporção acumulada de serviços",
                  title = "Raio necessário para alcançar X% dos serviços",
                  subtitle = "Vermelho pontilhado: percentis P50, P75, P90 e P95") +
    ggplot2::theme_minimal()
  list(percentis = percentis, grafico = grafico)
}

# --- Índice de Vizinho Mais Próximo (NNI) --------------------------------------
# Razão entre a distância média observada ao vizinho mais próximo e a esperada
# para um padrão aleatório. R < 1 → agrupado; R > 1 → disperso.
gs_analise_nni <- function(resultado) {
  pts_sf <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                         crs = gs_epsg$wgs84)
  d <- as.matrix(sf::st_distance(pts_sf))
  diag(d) <- Inf
  obs <- mean(apply(d, 1, min))
  n <- nrow(pts_sf)
  bbox_utm <- sf::st_bbox(sf::st_transform(pts_sf, gs_epsg$oficial))
  area <- (bbox_utm["xmax"] - bbox_utm["xmin"]) * (bbox_utm["ymax"] - bbox_utm["ymin"])
  esperado <- 0.5 * sqrt(area / n)
  se <- 0.26136 * sqrt(area / n^2)
  R <- obs / esperado
  z <- (obs - esperado) / se
  pvalor <- 2 * stats::pnorm(-abs(z))
  interpretacao <- if (R < 0.5) "Fortemente agrupado" else
                   if (R < 1) "Agrupado" else
                   if (R < 1.5) "Aleatório/disperso" else "Disperso (uniforme)"
  list(
    executado = TRUE, n = n,
    distancia_observada_m = round(obs, 1),
    distancia_esperada_m = round(esperado, 1),
    indice_nni = round(R, 3), z = round(z, 2),
    valor_p = round(pvalor, 4), area_km2 = round(area / 1e6, 2),
    interpretacao = interpretacao,
    avisos = c(
      "O NNI usa a área da caixa envolvente (bbox) dos pontos; pontos próximos à borda sofrem efeito de borda, o que tende a subestimar a dispersão."
    )
  )
}

# --- Grade hexagonal de contagens (apoio para LISA / Getis-Ord) ----------------
gs_grade_hex <- function(resultado, celula_m = gs_celula_hex_m) {
  pts <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                      crs = gs_epsg$wgs84)
  pts_utm <- sf::st_transform(pts, gs_epsg$oficial)
  bbox <- sf::st_bbox(pts_utm)
  bbox <- bbox + c(-celula_m, -celula_m, celula_m, celula_m)
  grade <- sf::st_make_grid(bbox, cellsize = celula_m, square = FALSE)
  grade_sf <- sf::st_sf(geometry = grade)
  idx <- sf::st_intersects(grade_sf, pts_utm)
  grade_sf$n_servicos <- lengths(idx)
  grade_sf$camadas <- vapply(idx, function(i) {
    if (length(i) == 0) NA_character_
    else paste(sort(unique(pts_utm$camada[i])), collapse = ", ")
  }, character(1))
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
             "não significativo" = "grey85")
  ggplot2::ggplot(grade) +
    ggplot2::geom_sf(ggplot2::aes(fill = classe), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = cores) +
    gs_tema_mapa() +
    ggplot2::labs(title = titulo, fill = "Classe")
}

# --- Getis-Ord (G* local) — aglomerados quentes/frios — requer spdep ------------
gs_analise_getis_ord <- function(resultado, celula_m = gs_celula_hex_m) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = "Pacote 'spdep' não instalado. Instale com: install.packages('spdep')"))
  }
  grade <- gs_grade_hex(resultado, celula_m)
  ocupadas <- grade[!is.na(grade$camadas), , drop = FALSE]
  if (nrow(ocupadas) < 4) {
    return(list(executado = FALSE,
                mensagem = "Menos de 4 células com serviços para Getis-Ord."))
  }
  nb <- spdep::poly2nb(sf::st_geometry(ocupadas), queen = TRUE)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  gi <- tryCatch(spdep::localG(ocupadas$n_servicos, lw, zero.policy = TRUE),
                 error = function(e) NULL)
  if (is.null(gi)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao executar Getis-Ord (pouca variância nas células)."))
  }
  ocupadas$gi <- unname(attr(gi, "gstat"))
  ocupadas$gi_z <- as.numeric(gi)
  ocupadas$p_valor <- 2 * stats::pnorm(-abs(ocupadas$gi_z))
  ocupadas$classe <- ifelse(ocupadas$gi_z > 1.96, "ponto quente",
                     ifelse(ocupadas$gi_z < -1.96, "ponto frio",
                            "não significativo"))
  list(executado = TRUE, celula_m = celula_m,
       grade = ocupadas,
       avisos = "P-valores locais não corrigidos para múltiplos testes; trate os resultados como exploratórios.",
       mapa = gs_mapa_grade_classe(ocupadas, "Getis-Ord (G*) por célula"))
}

# --- LISA (Moran local) — aglomerados alto-alto / baixo-baixo — requer spdep ----
gs_analise_lisa <- function(resultado, celula_m = gs_celula_hex_m) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = "Pacote 'spdep' não instalado. Instale com: install.packages('spdep')"))
  }
  grade <- gs_grade_hex(resultado, celula_m)
  ocupadas <- grade[!is.na(grade$camadas), , drop = FALSE]
  if (nrow(ocupadas) < 4) {
    return(list(executado = FALSE,
                mensagem = "Menos de 4 células com serviços para LISA."))
  }
  nb <- spdep::poly2nb(sf::st_geometry(ocupadas), queen = TRUE)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  lm <- tryCatch(spdep::localmoran(ocupadas$n_servicos, lw, zero.policy = TRUE),
                 error = function(e) NULL)
  if (is.null(lm)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao executar LISA (pouca variância nas células)."))
  }
  ocupadas$lisa_i <- lm[, "Ii"]
  ocupadas$lisa_z <- lm[, "Z.Ii"]
  ocupadas$p_valor <- lm[, "Pr(z != E(Ii))"]
  ocupadas$classe <- ifelse(ocupadas$lisa_z > 1.96 & ocupadas$p_valor < 0.05,
                            "alto-alto",
                     ifelse(ocupadas$lisa_z < -1.96 & ocupadas$p_valor < 0.05,
                            "baixo-baixo", "não significativo"))
  list(executado = TRUE, celula_m = celula_m,
       grade = ocupadas,
       avisos = "P-valores locais não corrigidos para múltiplos testes; trate os resultados como exploratórios.",
       mapa = gs_mapa_grade_classe(ocupadas, "LISA por célula"))
}

# --- Função K de Ripley — multiescala — requer spatstat -------------------------
gs_analise_ripley_k <- function(resultado, rmax_m = NULL) {
  if (!requireNamespace("spatstat", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = "Pacote 'spatstat' não instalado. Instale com: install.packages('spatstat')"))
  }
  pts_sf <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                         crs = gs_epsg$wgs84)
  pts_utm <- sf::st_transform(pts_sf, gs_epsg$oficial)
  coords <- sf::st_coordinates(pts_utm)
  bb <- sf::st_bbox(pts_utm)
  win <- spatstat.geom::owin(xrange = c(bb["xmin"], bb["xmax"]),
                             yrange = c(bb["ymin"], bb["ymax"]))
  ppp <- spatstat.geom::ppp(x = coords[, 1], y = coords[, 2], window = win)
  dup <- spatstat.geom::duplicated.ppp(ppp)
  if (any(dup)) {
    message("  ripley_k: removidos ", sum(dup), " pontos duplicados.")
    ppp <- ppp[!dup]
  }
  if (ppp$n < 4) {
    return(list(executado = FALSE, mensagem = "Menos de 4 pontos para a função K."))
  }
  K <- spatstat.explore::Kest(ppp, correction = "border", rmax = rmax_m)
  df <- as.data.frame(K)
  grafico <- ggplot2::ggplot(df, ggplot2::aes(x = r)) +
    ggplot2::geom_line(ggplot2::aes(y = theo), color = "#d7301f", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data[["border"]]), color = "#0570b0") +
    ggplot2::labs(x = "Distância (m)", y = "K(r)",
                  title = "Função K de Ripley",
                  subtitle = "Azul: observada | Vermelho: esperada (aleatório)") +
    ggplot2::theme_minimal()
  list(executado = TRUE, objeto = K, grafico = grafico)
}

# --- KDE com banda estimada (Silverman) ----------------------------------------
gs_analise_kde_banda <- function(resultado, ponto) {
  bx <- MASS::bandwidth.nrd(resultado$longitude)
  by <- MASS::bandwidth.nrd(resultado$latitude)
  ggplot2::ggplot(resultado, ggplot2::aes(x = longitude, y = latitude)) +
    ggplot2::stat_density_2d(ggplot2::aes(fill = ggplot2::after_stat(density)),
                             geom = "raster", contour = FALSE, alpha = 0.8,
                             h = c(bx, by)) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::geom_point(color = "#d7301f", size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Densidade de kernel (banda estimada)",
      subtitle = sprintf("Ponto: %s | bw_x = %.6f°, bw_y = %.6f°",
                         ponto$origem, bx, by)
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

# --- Distribuição por distrito (cruzamento espacial) ---------------------------
gs_analise_por_distrito <- function(resultado, dir = gs_pasta_dados()) {
  distritos <- tryCatch(gs_baixar_distritos(dir), error = function(e) NULL)
  if (is.null(distritos)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível carregar a camada de distritos ",
                "(verifique a conexão com o WFS do GeoSampa)."))
  }
  distritos <- sf::st_transform(distritos, gs_epsg$wgs84)
  pts <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                      crs = gs_epsg$wgs84)
  j <- sf::st_join(pts, distritos)
  cont <- as.data.frame(table(j$nm_distrito_municipal))
  names(cont) <- c("distrito", "n_servicos")
  dist <- merge(distritos, cont, by.x = "nm_distrito_municipal",
                by.y = "distrito", all.x = TRUE)
  dist$n_servicos[is.na(dist$n_servicos)] <- 0
  area <- as.numeric(sf::st_area(dist))
  dist$area_km2 <- round(area / 1e6, 3)
  dist$densidade_por_km2 <- round(dist$n_servicos / pmax(dist$area_km2, 0.001), 3)
  mapa <- ggplot2::ggplot(dist) +
    ggplot2::geom_sf(ggplot2::aes(fill = n_servicos), color = "white",
                     linewidth = 0.15) +
    ggplot2::scale_fill_viridis_c(na.value = "grey90") +
    gs_tema_mapa() +
    ggplot2::labs(title = "Serviços por distrito", fill = "Nº de serviços",
                  caption = "Fonte: Prefeitura de São Paulo / GeoSampa")
  list(executado = TRUE,
       por_distrito = sf::st_drop_geometry(dist),
       mapa = mapa)
}

# --- Moran's I agregado por distrito (diagnóstico fino) ------------------------
gs_analise_moran_distrital <- function(resultado, dir = gs_pasta_dados()) {
  if (!requireNamespace("spdep", quietly = TRUE)) {
    return(list(executado = FALSE,
                mensagem = "Pacote 'spdep' não instalado. Instale com: install.packages('spdep')"))
  }
  distritos <- tryCatch(gs_baixar_distritos(dir), error = function(e) NULL)
  if (is.null(distritos)) {
    return(list(executado = FALSE,
                mensagem = "Não foi possível carregar a camada de distritos ",
                "(verifique a conexão com o WFS do GeoSampa)."))
  }
  distritos <- sf::st_transform(distritos, gs_epsg$wgs84)
  pts <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                      crs = gs_epsg$wgs84)
  j <- sf::st_join(pts, distritos)
  cont <- as.data.frame(table(j$nm_distrito_municipal))
  names(cont) <- c("distrito", "n_servicos")
  dist <- merge(distritos, cont, by.x = "nm_distrito_municipal",
                by.y = "distrito", all.x = TRUE)
  dist$n_servicos[is.na(dist$n_servicos)] <- 0
  if (nrow(dist) < 4 || stats::var(dist$n_servicos) == 0) {
    return(list(executado = FALSE,
                mensagem = "Menos de 4 distritos ou sem variação de contagem ",
                "— insuficiente para Moran distrital."))
  }
  nb <- spdep::poly2nb(sf::st_geometry(dist), queen = TRUE)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  teste <- tryCatch(spdep::moran.test(dist$n_servicos, lw, zero.policy = TRUE),
                    error = function(e) NULL)
  if (is.null(teste)) {
    return(list(executado = FALSE,
                mensagem = "Falha ao executar Moran distrital (vizinhança inválida)."))
  }
  lm <- spdep::localmoran(dist$n_servicos, lw, zero.policy = TRUE)
  dist$lisa_i <- lm[, "Ii"]
  dist$lisa_z <- lm[, "Z.Ii"]
  dist$p_valor <- lm[, "Pr(z != E(Ii))"]
  dist$classe <- ifelse(dist$lisa_z > 1.96 & dist$p_valor < 0.05, "alto-alto",
                 ifelse(dist$lisa_z < -1.96 & dist$p_valor < 0.05, "baixo-baixo",
                        "não significativo"))
  mapa <- ggplot2::ggplot(dist) +
    ggplot2::geom_sf(ggplot2::aes(fill = classe), color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(
      values = c("alto-alto" = "#d7301f", "baixo-baixo" = "#0570b0",
                 "não significativo" = "grey85")) +
    gs_tema_mapa() +
    ggplot2::labs(title = "Moran local por distrito (LISA)", fill = "Classe")
  list(executado = TRUE,
       moran_i = unname(teste$estimate["Moran I statistic"]),
       valor_p = teste$p.value,
       por_distrito = sf::st_drop_geometry(dist),
       mapa = mapa)
}

# --- Cobertura populacional: população dentro do raio de busca ------------------
# Fontes: (a) `pop_layer` — objeto sf com coluna `populacao` e geometria
# poligonal (ex.: setores censitários do IBGE unidos com os dados do Censo);
# (b) `densidade_km2` — densidade média estimada (hab/km²) para estimativa.
gs_analise_cobertura_populacional <- function(resultado, ponto, raio_m,
                                              pop_layer = NULL,
                                              densidade_km2 = NULL) {
  if (is.null(pop_layer) && is.null(densidade_km2)) {
    return(list(executado = FALSE,
      mensagem = paste0(
        "Nenhuma fonte de população informada. Forneça `pop_layer` ",
        "(objeto sf com coluna `populacao`, ex.: setores censitários do IBGE) ",
        "ou `densidade_km2` (hab/km²) para estimar a população no raio.")))
  }
  b <- sf::st_buffer(sf::st_transform(ponto$sf, gs_epsg$oficial), raio_m)
  if (!is.null(densidade_km2)) {
    area_km2 <- as.numeric(sf::st_area(b)) / 1e6
    return(list(executado = TRUE, metodo = "densidade", raio_m = raio_m,
                area_km2 = round(area_km2, 2), densidade_km2 = densidade_km2,
                populacao_estimada = round(densidade_km2 * area_km2)))
  }
  if (!inherits(pop_layer, "sf")) stop("`pop_layer` deve ser um objeto sf.")
  if (!"populacao" %in% names(pop_layer)) {
    stop("`pop_layer` precisa de uma coluna chamada 'populacao'.")
  }
  pop_t <- sf::st_transform(pop_layer, gs_epsg$oficial)
  pop_t$pop_id <- seq_len(nrow(pop_t))
  inter <- sf::st_intersection(pop_t, b)
  if (nrow(inter) == 0) {
    return(list(executado = TRUE, metodo = "pop_layer", raio_m = raio_m,
                area_km2 = 0, populacao_atendida = 0, n_unidades = 0))
  }
  area_orig <- as.numeric(sf::st_area(pop_t))
  inter$area_orig_m2 <- area_orig[inter$pop_id]
  inter$area_piece_m2 <- as.numeric(sf::st_area(inter))
  fracao <- pmin(inter$area_piece_m2 / pmax(inter$area_orig_m2, 1e-9), 1)
  pop <- sum(inter$populacao * fracao)
  area_km2 <- as.numeric(sf::st_area(b)) / 1e6
  list(executado = TRUE, metodo = "pop_layer", raio_m = raio_m,
       area_km2 = round(area_km2, 2),
       populacao_atendida = round(pop), n_unidades = nrow(inter))
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
                                         "cobertura_populacional")) {
  tipo <- match.arg(tipo, several.ok = TRUE)
  if (is.null(resultado)) {
    resultado <- gs_servicos_proximos(
      cep = cep, coordenadas = coordenadas, camadas = camadas,
      raio_m = raio_m, n_por_camada = n_por_camada,
      tipo_distancia = tipo_distancia
    )
  }
  ponto <- attr(resultado, "ponto")
  if (is.null(ponto)) {
    stop("`resultado` não tem o atributo 'ponto'. Use a saída de gs_servicos_proximos().")
  }
  raio <- attr(resultado, "raio_m")
  if (is.null(raio)) raio <- raio_m

  saida <- list()
  if ("descritivas" %in% tipo)            saida$descritivas        <- gs_analise_descritivas(resultado)
  if ("vizinho_mais_proximo" %in% tipo)   saida$vizinho_mais_proximo <- gs_analise_vizinho(resultado)
  if ("voronoi" %in% tipo)                saida$voronoi            <- gs_analise_voronoi(resultado, ponto)
  if ("kde" %in% tipo)                    saida$kde                <- gs_analise_kde(resultado, ponto)
  if ("kde_banda" %in% tipo)              saida$kde_banda          <- gs_analise_kde_banda(resultado, ponto)
  if ("raios_progressivos" %in% tipo)     saida$raios_progressivos <- gs_analise_raios(resultado, ponto)
  if ("moran" %in% tipo)                  saida$moran              <- gs_analise_moran(resultado)
  if ("rede_viaria" %in% tipo)            saida$rede_viaria        <- gs_analise_rede(resultado, ponto)
  if ("acessibilidade_media" %in% tipo)   saida$acessibilidade_media <- gs_analise_acessibilidade(resultado)
  if ("cobertura_buffer" %in% tipo)       saida$cobertura_buffer   <- gs_analise_cobertura(resultado, ponto)
  if ("raio_otimo" %in% tipo)             saida$raio_otimo         <- gs_analise_raio_otimo(resultado)
  if ("nni" %in% tipo)                    saida$nni                <- gs_analise_nni(resultado)
  if ("getis_ord" %in% tipo)              saida$getis_ord          <- gs_analise_getis_ord(resultado)
  if ("lisa" %in% tipo)                   saida$lisa               <- gs_analise_lisa(resultado)
  if ("ripley_k" %in% tipo)               saida$ripley_k           <- gs_analise_ripley_k(resultado)
  if ("por_distrito" %in% tipo)           saida$por_distrito       <- gs_analise_por_distrito(resultado)
  if ("moran_distrital" %in% tipo)        saida$moran_distrital    <- gs_analise_moran_distrital(resultado)
  if ("cobertura_populacional" %in% tipo) saida$cobertura_populacional <- gs_analise_cobertura_populacional(resultado, ponto, raio)
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
  d <- resultado$distancia_m

  if (!is.null(analises$descritivas)) {
    r <- summary(d)
    out$descritivas <- fmt(
      "Foram encontrados %d serviço(s) num raio de %d m. As distâncias vão de %.0f m a %.0f m, com mediana de %.0f m — ou seja, metade dos serviços está a até %.0f m do ponto — e média de %.0f m.",
      analises$descritivas$n_total, raio_m,
      r[["Min."]], r[["Max."]], r[["Median"]], r[["Median"]], r[["Mean"]])
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
      !is.null(analises$acessibilidade_media$geral)) {
    g <- analises$acessibilidade_media$geral
    out$acessibilidade_media <- fmt(
      "Acessibilidade média: distância mediana de %.0f m, com P25 de %.0f m e P75 de %.0f m (IQR = %.0f m). A média é %.0f m com desvio-padrão de %.0f m (CV = %s%%).",
      g$mediana, g$p25, g$p75, g$iqr, g$media, g$sd, g$cv)
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
      "Os buffers de %d m ao redor dos serviços cobrem %.1f%% da área do casco convexo dos pontos (%.2f de %.2f km²).",
      cb$raio_buffer_m, cb$pct_cobertura, cb$area_coberta_km2, cb$area_hull_km2)
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
    n_lisa <- sum(md$por_distrito$classe %in% c("alto-alto", "baixo-baixo"))
    out$moran_distrital <- fmt(
      "Moran's I agregado por distrito: I = %.4f, p = %.4f. %d distrito(s) foram sinalizados como aglomerado 'alto-alto' ou 'baixo-baixo' no mapa LISA.",
      md$moran_i, md$valor_p, n_lisa)
  }

  if (!is.null(analises[["getis_ord"]]) && isTRUE(analises[["getis_ord"]]$executado)) {
    g <- analises[["getis_ord"]]
    out$getis_ord <- fmt(
      "Getis-Ord G* em grade hexagonal de %d m: %d célula(s) de ponto quente e %d de ponto frio (|z| > 1,96). Trate como exploratório.",
      g$celula_m, sum(g$grade$classe == "ponto quente"),
      sum(g$grade$classe == "ponto frio"))
  }

  if (!is.null(analises[["lisa"]]) && isTRUE(analises[["lisa"]]$executado)) {
    l <- analises[["lisa"]]
    out$lisa <- fmt(
      "LISA (Moran local) em grade hexagonal de %d m: %d célula(s) 'alto-alto' e %d 'baixo-baixo' (p < 0,05). Trate como exploratório.",
      l$celula_m, sum(l$grade$classe == "alto-alto"),
      sum(l$grade$classe == "baixo-baixo"))
  }

  if (!is.null(analises[["por_distrito"]]) && isTRUE(analises[["por_distrito"]]$executado)) {
    pd <- analises[["por_distrito"]]$por_distrito
    if (nrow(pd) > 0) {
      top <- pd[which.max(pd$n_servicos), , drop = FALSE]
      out$por_distrito <- fmt(
        "Serviços distribuídos em %d distrito(s) da cidade. O distrito com mais serviços é %s (%d serviço(s), densidade de %.2f por km²).",
        sum(pd$n_servicos > 0), top$distrito, top$n_servicos,
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

  if (!is.null(analises[["kde"]])) {
    out$kde <- paste0(
      "O mapa de densidade de kernel mostra as áreas de maior concentração de ",
      "serviços: quanto mais quente a cor, maior a concentração local.")
  }

  if (!is.null(analises[["kde_banda"]])) {
    out$kde_banda <- paste0(
      "O mapa de densidade de kernel com banda estimada (Silverman) mostra as ",
      "áreas de maior concentração de serviços, com suavização ajustada aos dados.")
  }

  if (!is.null(analises[["voronoi"]])) {
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