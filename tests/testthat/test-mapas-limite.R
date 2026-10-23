local_resultado_mapa_limite <- function(tipo, limitar = TRUE,
                                       .local_envir = parent.frame()) {
  pasta <- withr::local_tempdir(.local_envir = .local_envir)
  origem <- sf::st_sfc(sf::st_point(c(-46.63, -23.55)), crs = gs_epsg$wgs84)
  centro <- sf::st_coordinates(sf::st_transform(origem, gs_epsg$oficial))[1, ]
  xy <- sweep(rbind(c(100, 0), c(0, 200), c(700, 600)), 2, centro, "+")
  pts <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]),
                      coords = c("x", "y"), crs = gs_epsg$oficial)
  lonlat <- sf::st_coordinates(sf::st_transform(pts, gs_epsg$wgs84))
  readr::write_csv(data.frame(
    nm_equipamento = c("UBS A", "UBS B", "UBS C"),
    nm_tipo_equipamento = "Saúde",
    latitude = lonlat[, 2], longitude = lonlat[, 1]
  ), file.path(pasta, "equipamento_saude_teste.csv"))
  local_mock_global(
    gs_http_get = function(...) stop("Consulta externa inesperada"),
    .local_envir = .local_envir
  )
  if (tipo == "rede_viaria") {
    # Distâncias sintéticas da rede, sem depender do servidor ou pacote OSRM.
    local_mock_global(
      gs_calcular_distancias = function(ponto_sf, pontos_sf, tipo_distancia) {
        stopifnot(identical(tipo_distancia, "rede_viaria"))
        c(150, 300, 1200)[seq_len(nrow(pontos_sf))]
      },
      .local_envir = .local_envir
    )
  }
  gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), raio_m = 1000, tipo_distancia = tipo,
    n_por_camada = if (limitar) 1 else NULL, dir = pasta
  )
}

chamadas_mapa_limite <- function(mapa, metodo) {
  Filter(function(x) identical(x$method, metodo), mapa$x$calls)
}

expect_losango_mapa <- function(geometria, ponto, raio_m) {
  utm <- sf::st_transform(sf::st_geometry(geometria), gs_epsg$oficial)
  centro <- sf::st_coordinates(sf::st_transform(ponto$sf, gs_epsg$oficial))[1, ]
  vertices <- sf::st_coordinates(utm)[, 1:2, drop = FALSE]
  desvios <- abs(sweep(vertices, 2, centro, "-"))
  expect_equal(nrow(vertices), 5)
  expect_equal(rowSums(desvios), rep(raio_m, nrow(vertices)), tolerance = 1e-7)
  expect_true(all(pmin(desvios[, 1], desvios[, 2]) < 1e-5))
  expect_equal(as.numeric(sf::st_area(utm)), 2 * raio_m^2, tolerance = 1e-7)
  expect_true(all(sf::st_is_valid(utm)))
}

for (limitar in c(FALSE, TRUE)) {
  test_that(paste("mapas Manhattan usam losango; n_por_camada limitado:", limitar), {
    resultado <- local_resultado_mapa_limite("manhattan", limitar)
    original <- resultado
    expect_identical(attr(resultado, "amostra_truncada"), limitar)
    expect_equal(nrow(resultado), if (limitar) 1 else 2)
    expect_identical(gs_dominio_consulta(resultado)$executado, !limitar)
    local_mock_global(
      gs_dominio_consulta = function(...) stop("Mapa não deve validar amostra estatística"),
      gs_servicos_proximos = function(...) stop("Mapa não deve refazer a seleção")
    )
    # A métrica armazenada no resultado prevalece sobre o argumento da busca.
    estatico <- gs_mapa_servicos(resultado, interativo = FALSE,
                                 tipo_distancia = "euclidiana")
    expect_losango_mapa(estatico$layers[[1]]$data, attr(resultado, "ponto"), 1000)
    expect_match(estatico$labels$subtitle, "Manhattan", fixed = TRUE)

    interativo <- gs_mapa_servicos(resultado, interativo = TRUE)
    expect_length(chamadas_mapa_limite(interativo, "addCircles"), 0)
    poligonos <- chamadas_mapa_limite(interativo, "addPolygons")
    expect_length(poligonos, 1)
    if (length(poligonos) == 1) {
      vertices <- poligonos[[1]]$args[[1]][[1]][[1]][[1]]
      geometria <- sf::st_sfc(sf::st_polygon(list(as.matrix(
        vertices[, c("lng", "lat")]
      ))), crs = gs_epsg$wgs84)
      expect_losango_mapa(geometria, attr(resultado, "ponto"), 1000)
      expect_match(paste(unlist(poligonos[[1]]$args), collapse = " "), "Manhattan")
    }
    titulo <- chamadas_mapa_limite(interativo, "addControl")[[1]]$args[[1]]
    expect_match(titulo, "Manhattan", fixed = TRUE)
    expect_identical(resultado, original)
  })

  test_that(paste("rede usa circulo de referencia rotulado; limitado:", limitar), {
    resultado <- local_resultado_mapa_limite("rede_viaria", limitar)
    original <- resultado
    expect_identical(attr(resultado, "amostra_truncada"), limitar)
    expect_equal(nrow(resultado), if (limitar) 1 else 2)
    expect_false(gs_dominio_consulta(resultado)$executado)
    local_mock_global(
      gs_dominio_consulta = function(...) stop("Mapa não deve exigir isócrona"),
      gs_servicos_proximos = function(...) stop("Mapa não deve refazer a seleção")
    )
    estatico <- gs_mapa_servicos(resultado, interativo = FALSE)
    expect_match(estatico$labels$subtitle, "Rede viária", fixed = TRUE)
    expect_match(estatico$labels$caption, "linha reta", fixed = TRUE)
    expect_match(estatico$labels$caption, "não representa.*alcançável pela rede")
    limite <- sf::st_transform(estatico$layers[[1]]$data, gs_epsg$oficial)
    expect_gt(nrow(sf::st_coordinates(limite)), 5)
    expect_equal(as.numeric(sf::st_area(limite)), pi * 1000^2, tolerance = 0.001)

    interativo <- gs_mapa_servicos(resultado, interativo = TRUE)
    circulos <- chamadas_mapa_limite(interativo, "addCircles")
    expect_length(circulos, 1)
    expect_length(chamadas_mapa_limite(interativo, "addPolygons"), 0)
    texto_circulo <- paste(unlist(circulos[[1]]$args), collapse = " ")
    expect_match(texto_circulo, "linha reta", fixed = TRUE)
    expect_match(texto_circulo, "não representa.*alcançável pela rede")
    expect_false(grepl("Raio de busca", texto_circulo, fixed = TRUE))
    titulo <- chamadas_mapa_limite(interativo, "addControl")[[1]]$args[[1]]
    expect_match(titulo, "Rede viária", fixed = TRUE)
    expect_match(titulo, "linha reta", fixed = TRUE)
    expect_match(titulo, "não representa.*alcançável pela rede")
    camadas <- chamadas_mapa_limite(interativo, "addLayersControl")[[1]]$args
    expect_match(paste(unlist(camadas), collapse = " "), "linha reta", fixed = TRUE)
    expect_identical(resultado, original)
  })
}

test_that("metricas usuais preservam circulos e informam a distancia com limite", {
  rotulos <- c(geodesica = "Geodésica", euclidiana = "Euclidiana", haversine = "Haversine")
  for (tipo in names(rotulos)) {
    resultado <- local_resultado_mapa_limite(tipo)
    expect_true(attr(resultado, "amostra_truncada"))
    estatico <- gs_mapa_servicos(resultado, interativo = FALSE)
    expect_match(estatico$labels$subtitle, rotulos[[tipo]], fixed = TRUE)
    limite <- sf::st_transform(estatico$layers[[1]]$data, gs_epsg$oficial)
    expect_gt(nrow(sf::st_coordinates(limite)), 5)
    expect_equal(as.numeric(sf::st_area(limite)), pi * 1000^2, tolerance = 0.001)
    interativo <- gs_mapa_servicos(resultado, interativo = TRUE)
    expect_length(chamadas_mapa_limite(interativo, "addCircles"), 1)
    expect_length(chamadas_mapa_limite(interativo, "addPolygons"), 0)
    titulo <- chamadas_mapa_limite(interativo, "addControl")[[1]]$args[[1]]
    expect_match(titulo, rotulos[[tipo]], fixed = TRUE)
  }
})

test_that("resultados antigos sem metrica continuam desenhando circulos", {
  resultado <- local_resultado_mapa_limite("geodesica")
  attr(resultado, "tipo_distancia") <- NULL
  estatico <- gs_mapa_servicos(resultado, interativo = FALSE)
  interativo <- gs_mapa_servicos(resultado, interativo = TRUE)
  expect_match(estatico$labels$subtitle, "Geodésica", fixed = TRUE)
  expect_length(chamadas_mapa_limite(interativo, "addCircles"), 1)
})
