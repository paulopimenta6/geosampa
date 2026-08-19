test_that("gs_mapa_servicos valida resultado sem atributo ponto", {
  df <- data.frame(nome = "x", distancia_m = 5,
                   latitude = -23.55, longitude = -46.58, camada = "a")
  expect_error(gs_mapa_servicos(df, interativo = FALSE), "atributo 'ponto'")
})

test_that("gs_analise_servicos valida resultado sem atributo ponto", {
  df <- data.frame(nome = "x", distancia_m = 5,
                   latitude = -23.55, longitude = -46.58, camada = "a")
  expect_error(gs_analise_servicos(df, tipo = "descritivas"), "atributo 'ponto'")
})

test_that("gs_tipos_distancia lista os cinco tipos", {
  t <- gs_tipos_distancia()
  expect_true(all(c("geodesica", "euclidiana", "haversine", "manhattan",
                    "rede_viaria") %in% t$tipo))
})

test_that("gs_mapa_servicos interativo tem hover (label) e popup nos serviços", {
  df <- data.frame(
    nome = c("UBS A", "Hospital B"),
    tipo_servico = c("Unidade Básica de Saúde", "Hospital"),
    endereco = c("Rua 1", "Av 2"),
    bairro = c("Centro", "Pinheiros"),
    distancia_m = c(100, 500),
    latitude = c(-23.55, -23.56),
    longitude = c(-46.58, -46.57),
    camada = "equipamento_saude",
    stringsAsFactors = FALSE
  )
  attr(df, "ponto") <- list(latitude = -23.55, longitude = -46.58,
                            origem = "CEP 05508-090")
  attr(df, "raio_m") <- 2000
  m <- gs_mapa_servicos(df, interativo = TRUE)
  metodos <- vapply(m$x$calls, function(c) c$method, character(1))
  expect_true("addCircleMarkers" %in% metodos)
  chamadas <- m$x$calls[metodos == "addCircleMarkers"]
  # última chamada de addCircleMarkers = serviços (label e popup preenchidos)
  args <- chamadas[[length(chamadas)]]$args
  expect_type(args[[11]], "character")   # label de hover
  expect_length(args[[11]], 2)
  expect_true(all(grepl("UBS A|Hospital B", args[[11]])))
  expect_true(all(grepl("100 m|500 m", args[[11]])))
  # label com classe "html": o leaflet NÃO escapa as tags <b>/<br>
  expect_true(inherits(args[[11]], "html"))
  expect_true(all(grepl("<b>", args[[11]], fixed = TRUE)))
  expect_false(any(grepl("&lt;b&gt;", args[[11]], fixed = TRUE)))
  expect_type(args[[9]], "character")    # popup
  expect_true(all(grepl("Distância", args[[9]])))
  expect_true(all(grepl("endereco|Rua 1|Av 2", args[[9]], ignore.case = TRUE)))
  # legenda com rótulos encurtados
  expect_true(any(metodos == "addLegend"))
})

test_that("gs_mapa_servicos estático usa legenda no rodapé e coord_sf", {
  df <- data.frame(
    nome = c("UBS A", "Hospital B"),
    tipo_servico = c("Unidade Básica de Saúde", "Hospital"),
    distancia_m = c(100, 500),
    latitude = c(-23.55, -23.56),
    longitude = c(-46.58, -46.57),
    camada = "equipamento_saude",
    stringsAsFactors = FALSE
  )
  attr(df, "ponto") <- list(latitude = -23.55, longitude = -46.58,
                            origem = "CEP 05508-090")
  attr(df, "raio_m") <- 2000
  p <- gs_mapa_servicos(df, interativo = FALSE)
  expect_identical(p$theme$legend.position, "bottom")
  expect_s3_class(p$coordinates, "CoordSf")
})

test_that("gs_mapa_servicos estático usa legenda em várias colunas", {
  df <- data.frame(
    nome = paste("Serviço", 1:9),
    tipo_servico = paste("Tipo", LETTERS[1:9]),
    distancia_m = seq(50, 450, by = 50),
    latitude = rep(-23.55, 9) + (1:9) * 0.001,
    longitude = rep(-46.58, 9) + (1:9) * 0.001,
    camada = "equipamento_saude",
    stringsAsFactors = FALSE
  )
  attr(df, "ponto") <- list(latitude = -23.55, longitude = -46.58,
                            origem = "CEP 05508-090")
  attr(df, "raio_m") <- 2000
  p <- gs_mapa_servicos(df, interativo = FALSE)
  g <- p$guides$guides[["colour"]]
  expect_true(g$params$ncol > 1)
})

test_that("gs_paleta_tipos cobre 1..12 (Paired) e >12 níveis", {
  expect_length(gs_paleta_tipos(1), 2)
  expect_length(gs_paleta_tipos(12), 12)
  expect_length(gs_paleta_tipos(20), 20)
})

test_that("gs_lab_format_curto encurta rótulos longos", {
  f <- gs_lab_format_curto(10)
  expect_identical(f("curto"), "curto")
  expect_identical(f("texto muito longo"), "texto m...")
})