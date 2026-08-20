rede_habilitada <- function() {
  identical(tolower(Sys.getenv("GEOSAMPA_RUN_NETWORK_TESTS", "false")), "true")
}

test_that("integração real: catálogo e download WFS filtrado", {
  skip_if_not(rede_habilitada(), "Defina GEOSAMPA_RUN_NETWORK_TESTS=true")
  pasta <- withr::local_tempdir()
  withr::local_options(list(gs.cache_capabilities = file.path(pasta, "cap.xml")))

  catalogo <- gs_camadas_disponiveis(force = TRUE)
  expect_gt(nrow(catalogo), 400)
  expect_true("equipamento_bombeiros" %in% catalogo$camada)
  expect_true(all(grepl("31983", catalogo$crs)))

  baixado <- gs_baixar_camada(
    "equipamento_bombeiros", filtro = "cd_identificador = 150001",
    dir = pasta, tamanho_pagina = 2, verbose = FALSE
  )
  expect_identical(baixado$total, 1L)
  expect_true(file.exists(baixado$geojson))
  expect_true(file.exists(baixado$csv))
  objeto <- sf::st_read(baixado$geojson, quiet = TRUE)
  expect_equal(sf::st_crs(objeto)$epsg, gs_epsg$oficial)
})

test_that("integração real: ViaCEP e Nominatim", {
  skip_if_not(rede_habilitada(), "Defina GEOSAMPA_RUN_NETWORK_TESTS=true")
  endereco <- gs_ler_cep("01001-000")
  expect_identical(endereco$uf, "SP")
  expect_match(endereco$cidade, "São Paulo|Sao Paulo")

  coordenada <- gs_consultar_nominatim(
    list(postalcode = "01001-000", country = "Brazil")
  )
  expect_false(is.null(coordenada))
  expect_true(is.finite(coordenada$latitude))
  expect_true(is.finite(coordenada$longitude))
})

test_that("integração real: catálogo GeoNetwork", {
  skip_if_not(rede_habilitada(), "Defina GEOSAMPA_RUN_NETWORK_TESTS=true")
  registros <- gs_metadados("UBS", de = 1, ate = 3)
  expect_s3_class(registros, "data.frame")
  expect_true(all(c("uuid", "id", "categoria", "data_criacao") %in% names(registros)))
  expect_true(!is.null(attr(registros, "total")))
})

test_that("integração real: distância OSRM", {
  skip_if_not(rede_habilitada(), "Defina GEOSAMPA_RUN_NETWORK_TESTS=true")
  skip_if_not_installed("osrm")
  origem <- sf::st_sfc(sf::st_point(c(-46.6333, -23.5505)), crs = 4326)
  destino <- sf::st_sfc(sf::st_point(c(-46.6250, -23.5570)), crs = 4326)
  distancia <- gs_calcular_distancias(origem, destino, "rede_viaria")
  expect_length(distancia, 1)
  expect_true(is.finite(distancia) && distancia > 0 && distancia < 10000)
})
