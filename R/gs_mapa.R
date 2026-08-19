# ============================================================
# GeoSampa — Mapas do ponto de interesse e serviços próximos
# ------------------------------------------------------------
# Gera mapas estáticos (ggplot2 -> PNG/PDF) ou interativos
# (leaflet -> HTML) com o ponto do CEP, o raio de busca e os
# serviços próximos encontrados por gs_servicos_proximos().
# ============================================================

# --- Helpers compartilhados de tema, legenda e paleta ------------------------
# Tema padrão dos mapas: legenda no rodapé em várias linhas, tipografia maior
# e grid limpo. Usado pelos mapas estáticos e pelas figuras dos relatórios.
gs_tema_mapa <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold",
                                           size = ggplot2::rel(0.9)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.75)),
      legend.key.width = ggplot2::unit(1.4, "lines"),
      legend.key.height = ggplot2::unit(1.1, "lines"),
      legend.spacing.x = ggplot2::unit(0.4, "cm"),
      legend.spacing.y = ggplot2::unit(0.15, "cm"),
      legend.margin = ggplot2::margin(t = 6, b = 2),
      legend.box.spacing = ggplot2::unit(0.25, "cm"),
      plot.title = ggplot2::element_text(face = "bold",
                                         size = ggplot2::rel(1.15)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.9),
                                            colour = "grey30"),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.75),
                                           colour = "grey50"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# Guia de legenda para escalas de cor: chaves maiores e, quando pedido,
# quebra em várias colunas/linhas para a legenda do rodapé caber sem ser
# cortada (ncol/nrow — informe apenas um deles).
gs_guia_legenda_mapa <- function(nrow = NULL, ncol = NULL) {
  ggplot2::guides(color = ggplot2::guide_legend(
    nrow = nrow, ncol = ncol,
    keywidth = 0.9, keyheight = 0.9,
    override.aes = list(size = 3.4, alpha = 1)
  ))
}

# Paleta qualitativa por nº de níveis: Paired até 12; hcl "Set 2" acima disso.
gs_paleta_tipos <- function(n) {
  n <- max(n, 2L)
  if (n <= 12 && requireNamespace("RColorBrewer", quietly = TRUE)) {
    RColorBrewer::brewer.pal(12, "Paired")[seq_len(n)]
  } else {
    grDevices::hcl.colors(n, palette = "Set 2")
  }
}

# Encurta rótulos longos (legendas interativas e estáticas).
gs_lab_format_curto <- function(max_chars = 32) {
  function(s) {
    s <- as.character(s)
    ifelse(nchar(s) > max_chars,
           paste0(substr(s, 1, max_chars - 3), "..."),
           s)
  }
}

# Formata rótulos da legenda ESTÁTICA para caber no rodapé sem ser cortada:
# encurta textos muito longos e quebra palavras sem espaço (ex.: nomes de
# camada) em pedaços de `largura` caracteres, antes de quebrar nas palavras.
gs_lab_format_quebra <- function(largura = 18, max_caracteres = 30) {
  function(s) {
    s <- as.character(s)
    s <- ifelse(nchar(s) > max_caracteres,
                paste0(substr(s, 1, max_caracteres - 3), "..."), s)
    vapply(s, function(x) {
      x <- gsub(sprintf("([^ ]{%d})", largura), "\\1 ", x)
      x <- trimws(x)
      paste(strwrap(x, width = largura), collapse = "\n")
    }, character(1))
  }
}

# --- Popups HTML dos serviços (mapa interativo) ------------------------------
gs_popup_servicos <- function(resultado) {
  vapply(seq_len(nrow(resultado)), function(i) {
    r <- resultado[i, ]
    sprintf("<b>%s</b><br>Tipo: %s<br>Endereço: %s<br>Bairro: %s<br>Distância: %.0f m<br>Camada: %s",
            r$nome, r$tipo_servico, r$endereco, r$bairro,
            r$distancia_m, r$camada)
  }, character(1))
}

# Rótulos de hover (curtos) dos serviços — aparecem ao passar o mouse.
# Marcados com a classe "html" para o leaflet NÃO escapar as tags <b>/<br>
# (a função interna safeLabel() só preserva HTML em objetos da classe "html").
# Nota: não usar htmltools::HTML() aqui — ele colapsa vetores num único
# elemento; a classe é aplicada manualmente para manter um vetor por ponto.
gs_label_servicos <- function(resultado) {
  tipo <- if ("tipo_servico" %in% names(resultado) &&
               !all(is.na(resultado$tipo_servico))) {
    resultado$tipo_servico
  } else {
    resultado$camada
  }
  label <- vapply(seq_len(nrow(resultado)), function(i) {
    r <- resultado[i, ]
    sprintf("<b>%s</b><br>%s<br>%.0f m",
            ifelse(is.na(r$nome), r$camada, r$nome), tipo[i], r$distancia_m)
  }, character(1))
  structure(label, class = c("html", "character"))
}

# --- Mapa estático com ggplot2 ----------------------------------------------
gs_mapa_ggplot <- function(resultado, ponto, raio_m) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Pacote 'ggplot2' não instalado. Instale com install.packages('ggplot2').")
  }
  pts <- sf::st_as_sf(resultado, coords = c("longitude", "latitude"),
                      crs = gs_epsg$wgs84)
  origem <- sf::st_sfc(sf::st_point(c(ponto$longitude, ponto$latitude)),
                       crs = gs_epsg$wgs84)
  buffer <- sf::st_transform(
    sf::st_buffer(sf::st_transform(origem, gs_epsg$oficial), raio_m),
    gs_epsg$wgs84
  )

  cores <- if ("tipo_servico" %in% names(resultado) &&
               !all(is.na(resultado$tipo_servico))) "tipo_servico" else "camada"
  n_niveis <- length(unique(resultado[[cores]][!is.na(resultado[[cores]])]))
  # Legenda no rodapé em várias COLUNAS: evita que a legenda fique alta demais
  # e seja cortada na borda inferior da imagem (coord_sf fixa o aspecto do
  # painel e não "encolhe" para acomodar a legenda).
  ncol_legenda <- ceiling(sqrt(n_niveis))

  # Enquadra o mapa no raio de busca (com margem) para a legenda não espremer
  # a área de plotagem e os rótulos não se sobreporem.
  bb <- sf::st_bbox(buffer)
  margem <- 0.12 * max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])
  xlim <- c(bb["xmin"] - margem, bb["xmax"] + margem)
  ylim <- c(bb["ymin"] - margem, bb["ymax"] + margem)

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = buffer, fill = "#2c7fb8", alpha = 0.08,
                     color = "#2c7fb8", linetype = "dashed") +
    ggplot2::geom_sf(data = pts, ggplot2::aes(color = .data[[cores]]),
                     size = 2.5, alpha = 0.95) +
    ggplot2::geom_sf(data = origem, color = "#d7301f", size = 4, shape = 17) +
    ggplot2::scale_color_manual(values = gs_paleta_tipos(n_niveis),
                                labels = gs_lab_format_quebra(18),
                                na.value = "#999999") +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
    gs_tema_mapa(base_size = 15) +
    gs_guia_legenda_mapa(ncol = ncol_legenda) +
    ggplot2::labs(
      title = "Serviços próximos",
      subtitle = sprintf("Ponto: %s | Raio: %s m | %d serviço(s)",
                         ponto$origem, raio_m, nrow(resultado)),
      color = "Tipo de serviço",
      caption = "Fonte: Prefeitura de São Paulo / GeoSampa"
    )
}

# --- Mapa interativo com leaflet ---------------------------------------------
gs_mapa_leaflet <- function(resultado, ponto, raio_m) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Pacote 'leaflet' não instalado. Instale com install.packages('leaflet').")
  }
  cores <- if ("tipo_servico" %in% names(resultado) &&
               !all(is.na(resultado$tipo_servico))) {
    resultado$tipo_servico
  } else {
    resultado$camada
  }
  n_niveis <- length(unique(cores[!is.na(cores)]))
  pal <- leaflet::colorFactor(gs_paleta_tipos(n_niveis),
                              domain = factor(cores), na.color = "#999999")

  estilo_label <- leaflet::labelOptions(
    style = list("font-family" = "sans-serif",
                 "box-shadow" = "3px 3px rgba(0,0,0,0.3)",
                 "font-size" = "12px",
                 "border-color" = "rgba(0,0,0,0.5)"),
    textsize = "12px", direction = "auto", sticky = TRUE)

  leaflet::leaflet() |>
    leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "OSM") |>
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
    leaflet::addCircles(lng = ponto$longitude, lat = ponto$latitude,
                        radius = raio_m, color = "#2c7fb8", weight = 1.5,
                        opacity = 0.7, fillOpacity = 0.08, dashArray = "4 4",
                        group = "Raio de busca",
                        label = sprintf("Raio de busca: %s m", raio_m),
                        labelOptions = estilo_label,
                        highlightOptions = leaflet::highlightOptions(
                          weight = 3, color = "#2c7fb8", opacity = 1,
                          fillOpacity = 0.15)) |>
    leaflet::addCircleMarkers(lng = ponto$longitude, lat = ponto$latitude,
                              color = "#d7301f", radius = 9, fillOpacity = 0.95,
                              weight = 2, fillColor = "#d7301f",
                              group = "Ponto de interesse",
                              label = htmltools::HTML(
                                sprintf("<b>Ponto de interesse</b><br>%s",
                                        ponto$origem)),
                              labelOptions = estilo_label,
                              popup = sprintf("<b>Ponto de interesse</b><br>%s",
                                              ponto$origem)) |>
    leaflet::addCircleMarkers(lng = resultado$longitude, lat = resultado$latitude,
                              color = pal(cores), radius = 6, fillOpacity = 0.9,
                              weight = 1.5, fillColor = pal(cores),
                              group = "Serviços",
                              label = gs_label_servicos(resultado),
                              labelOptions = estilo_label,
                              popup = gs_popup_servicos(resultado)) |>
    leaflet::addLegend("bottomright", pal = pal,
                       values = cores[!is.na(cores)],
                       title = "Tipo de serviço", opacity = 1,
                       labFormat = leaflet::labelFormat(
                         transform = gs_lab_format_curto(30))) |>
    leaflet::addScaleBar(position = "bottomleft",
                         options = leaflet::scaleBarOptions(imperial = FALSE)) |>
    leaflet::addControl(
      html = sprintf(
        "<div style='font-family:sans-serif;font-size:14px;font-weight:bold;padding:6px 10px;background:rgba(255,255,255,0.9);border-radius:4px;border:1px solid #ddd'>Serviços próximos<br><span style='font-weight:normal;font-size:11px;color:#444'>%s · raio %s m · %d serviço(s)</span></div>",
        ponto$origem, raio_m, nrow(resultado)),
      position = "topright") |>
    leaflet::addLayersControl(
      baseGroups = c("OSM", "CartoDB", "Satélite"),
      overlayGroups = c("Serviços", "Raio de busca", "Ponto de interesse"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}

# --- Função principal: gera o mapa (estático ou interativo) ------------------
# Se `resultado` (vindo de gs_servicos_proximos) não for informado, os demais
# argumentos (cep/coordenadas/camadas/raio...) são usados para calculá-lo.
# `salvar`: caminho do arquivo de saída — .html para interativo, .png/.pdf
# para estático. Se NULL, apenas devolve o objeto de plot invisivelmente.
# `largura`/`altura`/`dpi` controlam a resolução do mapa estático.
gs_mapa_servicos <- function(resultado = NULL, cep = NULL, coordenadas = NULL,
                             camadas = NULL, raio_m = gs_raio_padrao_m,
                             tipo_distancia = c("geodesica", "euclidiana",
                                                "haversine", "manhattan",
                                                "rede_viaria"),
                             n_por_camada = NULL, interativo = TRUE,
                             salvar = NULL, largura = 12, altura = NULL,
                             dpi = 300) {
  if (is.null(resultado)) {
    resultado <- gs_servicos_proximos(
      cep = cep, coordenadas = coordenadas, camadas = camadas,
      raio_m = raio_m, n_por_camada = n_por_camada,
      tipo_distancia = tipo_distancia
    )
  }
  ponto <- attr(resultado, "ponto")
  if (is.null(ponto)) {
    stop("`resultado` não tem o atributo 'ponto'. Use a saída de ",
         "gs_servicos_proximos() ou informe cep/coordenadas para calculá-la.")
  }
  raio <- attr(resultado, "raio_m")
  if (is.null(raio)) raio <- raio_m

  mapa <- if (interativo) {
    gs_mapa_leaflet(resultado, ponto, raio)
  } else {
    gs_mapa_ggplot(resultado, ponto, raio)
  }

  if (!is.null(salvar)) {
    dir.create(dirname(salvar), showWarnings = FALSE, recursive = TRUE)
    if (interativo) {
      if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
        stop("Pacote 'htmlwidgets' não instalado para salvar o mapa interativo.")
      }
      htmlwidgets::saveWidget(mapa, file = salvar)
    } else {
      if (is.null(altura)) {
        # Altura ajustada à legenda do rodapé: reserva espaço extra
        # proporcional ao nº de níveis para a legenda não ser cortada.
        cores <- if ("tipo_servico" %in% names(resultado) &&
                     !all(is.na(resultado$tipo_servico))) {
          resultado$tipo_servico
        } else {
          resultado$camada
        }
        n_niveis <- length(unique(cores[!is.na(cores)]))
        altura <- largura + 0.5 + 0.35 * ceiling(sqrt(n_niveis))
      }
      ggplot2::ggsave(salvar, plot = mapa, width = largura, height = altura,
                      dpi = dpi)
    }
    message("Mapa salvo em: ", salvar)
  }
  invisible(mapa)
}