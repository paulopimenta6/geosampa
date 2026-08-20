local_mock_global <- function(..., .local_envir = parent.frame()) {
  novos <- list(...)
  ambiente <- globalenv()
  antigos <- mget(names(novos), envir = ambiente, inherits = FALSE)
  for (nome in names(novos)) assign(nome, novos[[nome]], envir = ambiente)
  withr::defer({
    for (nome in names(antigos)) assign(nome, antigos[[nome]], envir = ambiente)
  }, envir = .local_envir)
  invisible(NULL)
}

resposta_http_mock <- function(conteudo, tipo = "application/json") {
  structure(
    list(
      url = "mock://geosampa",
      status_code = 200L,
      headers = list(`content-type` = paste0(tipo, "; charset=UTF-8")),
      content = charToRaw(conteudo)
    ),
    class = "response"
  )
}

feicao_mock <- function(id, chave) {
  list(
    type = "Feature",
    id = id,
    properties = list(cd_identificador = chave, nome = paste("Servico", chave)),
    geometry = list(type = "Point", coordinates = c(333000 + chave, 7390000))
  )
}

test_that("catalogo normaliza namespace e grava cache atomico", {
  pasta <- tempfile("catalogo-")
  dir.create(pasta)
  cache <- file.path(pasta, "capabilities.xml")
  withr::local_options(list(gs.cache_capabilities = cache))

  xml <- paste0(
    "<wfs:WFS_Capabilities xmlns:wfs='http://www.opengis.net/wfs/2.0'>",
    "<wfs:FeatureTypeList><wfs:FeatureType>",
    "<wfs:Name>geoportal:equipamento_saude_ubs</wfs:Name>",
    "<wfs:Title>UBS</wfs:Title><wfs:Abstract>Unidades</wfs:Abstract>",
    "<wfs:DefaultCRS>urn:ogc:def:crs:EPSG::31983</wfs:DefaultCRS>",
    "</wfs:FeatureType></wfs:FeatureTypeList></wfs:WFS_Capabilities>"
  )
  local_mock_global(gs_http_get = function(...) {
    resposta_http_mock(xml, "application/xml")
  })

  catalogo <- gs_camadas_disponiveis(force = TRUE)
  expect_identical(catalogo$camada, "equipamento_saude_ubs")
  expect_identical(gs_tema_camada("geoportal:equipamento_saude_ubs"), "saude")
  expect_true(file.exists(cache))
  expect_setequal(list.files(pasta, all.files = TRUE),
                  c(".", "..", "capabilities.xml"))
})

test_that("WFS envia CRS oficial e conta com resultType hits", {
  consultas <- list()
  local_mock_global(gs_http_get = function(url, query = NULL, ...) {
    consultas[[length(consultas) + 1L]] <<- query
    resposta_http_mock(paste0(
      '{"type":"FeatureCollection","numberMatched":"unknown",',
      '"features":[]}'
    ))
  })

  pagina <- gs_requisitar_pagina(
    "geoportal:teste", count = 10, resultType = "hits"
  )
  expect_identical(pagina$numberMatched, "unknown")
  expect_identical(consultas[[1]]$typeNames, "geoportal:teste")
  expect_identical(consultas[[1]]$srsName, "EPSG:31983")
  expect_identical(consultas[[1]]$resultType, "hits")
  expect_true(is.na(gs_contar("teste")))
  expect_identical(consultas[[2]]$resultType, "hits")
})

test_that("WFS interpreta contagem hits devolvida em XML", {
  local_mock_global(gs_http_get = function(...) {
    resposta_http_mock(
      paste0(
        "<wfs:FeatureCollection xmlns:wfs='http://www.opengis.net/wfs/2.0' ",
        "numberMatched='17' numberReturned='0'/>") ,
      "application/xml"
    )
  })
  expect_identical(gs_contar("teste"), 17L)
})

test_that("paginacao reaproveita somente paginas ordenadas", {
  f1 <- feicao_mock("teste.1", 1)
  f2 <- feicao_mock("teste.2", 2)
  f3 <- feicao_mock("teste.3", 3)
  chamadas <- list()

  local_mock_global(
    gs_contar = function(...) NA_integer_,
    gs_requisitar_pagina = function(camada, count, startIndex = 0,
                                    filtro = NULL, sortBy = NULL,
                                    resultType = NULL) {
      chamadas[[length(chamadas) + 1L]] <<- list(
        startIndex = startIndex, sortBy = sortBy
      )
      if (is.null(sortBy)) return(list(features = list(f2, f1)))
      if (startIndex == 0) return(list(features = list(f1, f2)))
      if (startIndex == 2) return(list(features = list(f3)))
      list(features = list())
    }
  )

  pasta <- tempfile("wfs-")
  dir.create(pasta)
  writeLines("csv antigo", file.path(pasta, "teste.csv"))
  resultado <- gs_baixar_camada(
    "teste", dir = pasta, csv = FALSE, tamanho_pagina = 2, verbose = FALSE
  )
  ordenadas <- Filter(function(x) !is.null(x$sortBy), chamadas)
  expect_identical(vapply(ordenadas, `[[`, numeric(1), "startIndex"), c(0, 2, 3))
  expect_true(all(vapply(ordenadas, `[[`, character(1), "sortBy") ==
                    "cd_identificador"))
  salvo <- jsonlite::fromJSON(resultado$geojson, simplifyVector = FALSE)
  expect_identical(vapply(salvo$features, `[[`, character(1), "id"),
                   c("teste.1", "teste.2", "teste.3"))
  expect_false(file.exists(file.path(pasta, "teste.csv")))
})

test_that("download valida IDs, chave e contagem antes de salvar", {
  f1 <- feicao_mock("teste.1", 1)
  f2 <- feicao_mock("teste.2", 2)
  expect_error(
    gs_validar_feicoes_baixadas(list(f1, f1), 2L),
    "ID de fei.* duplicado"
  )
  expect_error(
    gs_validar_feicoes_baixadas(list(f1, f2), 3L),
    "esperadas 3"
  )

  destino <- tempfile("atomico-")
  writeLines("anterior", destino)
  expect_error(gs_gravar_atomico(destino, function(temporario) {
    writeLines("parcial", temporario)
    stop("falha simulada")
  }), "falha simulada")
  expect_identical(readLines(destino), "anterior")
})

test_that("promoção de conjunto bloqueia leitores, remove e restaura pares", {
  pasta <- tempfile("transacao-")
  dir.create(pasta)
  geo <- file.path(pasta, "camada.geojson")
  csv <- file.path(pasta, "camada.csv")
  novo_geo <- file.path(pasta, "novo.geojson")
  writeLines("geo antigo", geo)
  writeLines("csv antigo", csv)
  writeLines("geo novo", novo_geo)

  gs_promover_conjunto_atomico(c(novo_geo, NA_character_), c(geo, csv))
  expect_identical(readLines(geo), "geo novo")
  expect_false(file.exists(csv))
  expect_false(dir.exists(gs_lock_transacao(geo)))

  writeLines("a", csv)
  dir.create(gs_lock_transacao(csv))
  withr::local_options(list(gs.transacao_timeout_s = 0))
  expect_error(gs_ler_cabecalho_csv(csv), "transação interrompida")
  unlink(gs_lock_transacao(csv), recursive = TRUE)
  dir.create(gs_lock_diretorio(pasta))
  expect_error(gs_camadas_local(pasta), "transação interrompida")
  unlink(gs_lock_diretorio(pasta), recursive = TRUE)

  origem1 <- file.path(pasta, "origem1")
  origem2 <- file.path(pasta, "origem2")
  destino1 <- file.path(pasta, "destino1")
  destino2 <- file.path(pasta, "destino2")
  writeLines("novo 1", origem1)
  writeLines("novo 2", origem2)
  writeLines("antigo 1", destino1)
  writeLines("antigo 2", destino2)
  rename_real <- base::file.rename
  rlang::local_bindings(
    file.rename = function(from, to) {
      if (identical(from, origem2)) return(FALSE)
      rename_real(from, to)
    },
    .env = globalenv()
  )
  expect_error(
    gs_promover_conjunto_atomico(c(origem1, origem2), c(destino1, destino2)),
    "Não foi possível promover"
  )
  expect_identical(readLines(destino1), "antigo 1")
  expect_identical(readLines(destino2), "antigo 2")
  expect_false(any(dir.exists(c(gs_lock_transacao(destino1),
                                gs_lock_transacao(destino2)))))
})

test_that("indice preserva CEP texto e invalida cache por arquivos e diretorio", {
  withr::local_options(list(gs.indice_cep = NULL))
  pasta1 <- tempfile("indice-a-")
  pasta2 <- tempfile("indice-b-")
  dir.create(pasta1)
  dir.create(pasta2)

  writeLines(
    c("id,geometria_wkt", "1,\"POLYGON ((conteudo nao lido"),
    file.path(pasta1, "poligono.csv")
  )
  pontos1 <- file.path(pasta1, "pontos.csv")
  writeLines(
    c("cd_cep_equipamento,latitude,longitude,nm_equipamento",
      "01234-567,-23.5,-46.6,A"),
    pontos1
  )
  idx1 <- gs_indice_cep(pasta1, force = TRUE)
  expect_identical(idx1$cep, "01234567")

  writeLines(
    c("cd_cep_equipamento,latitude,longitude,nm_equipamento",
      "01234-567,-23.5,-46.6,A",
      "01234-567,-23.6,-46.7,B"),
    pontos1
  )
  idx_atualizado <- gs_indice_cep(pasta1)
  expect_equal(nrow(idx_atualizado), 2)

  writeLines(
    c("cd_cep_equipamento,latitude,longitude",
      "87654-321,-22.9,-43.2"),
    file.path(pasta2, "pontos.csv")
  )
  idx2 <- gs_indice_cep(pasta2)
  expect_identical(idx2$cep, "87654321")
})

test_that("indice nao oculta CSV corrompido", {
  withr::local_options(list(gs.indice_cep = NULL))
  pasta <- tempfile("indice-corrompido-")
  dir.create(pasta)
  writeLines(
    c("cd_cep_equipamento,latitude,longitude",
      "01234-567,latitude-invalida,-46.6"),
    file.path(pasta, "pontos.csv")
  )
  expect_error(gs_indice_cep(pasta, force = TRUE), "corrompido|malformado")
})

test_that("cascata sem indice consulta viaCEP uma unica vez", {
  withr::local_options(list(gs.indice_cep = NULL))
  pasta <- tempfile("sem-indice-")
  dir.create(pasta)
  consultas <- list()
  n_viacep <- 0L

  local_mock_global(
    gs_consultar_nominatim = function(query) {
      consultas[[length(consultas) + 1L]] <<- query
      if (length(consultas) < 3) return(NULL)
      list(latitude = -23.55, longitude = -46.63, nome = "Sao Paulo")
    },
    gs_ler_cep = function(cep) {
      n_viacep <<- n_viacep + 1L
      data.frame(
        cep = "01234-567", logradouro = "Rua A", bairro = "Centro",
        cidade = "Sao Paulo", uf = "SP", ibge = "3550308",
        stringsAsFactors = FALSE
      )
    }
  )

  resultado <- gs_cep_para_coordenadas("01234-567", dir = pasta)
  expect_identical(n_viacep, 1L)
  expect_length(consultas, 3)
  expect_match(resultado$precisao, "cidade")
})

test_that("Nominatim restringe consultas ao Brasil", {
  consulta <- NULL
  local_mock_global(
    gs_pausa_nominatim_s = 0,
    gs_http_get = function(url, query = NULL, ...) {
      consulta <<- query
      resposta_http_mock("[]")
    }
  )
  expect_null(gs_consultar_nominatim(list(postalcode = "01234-567")))
  expect_identical(consulta$countrycodes, "br")
})

test_that("precisao de rua ou cidade produz verificacao indeterminada", {
  withr::local_options(list(gs.indice_cep = NULL))
  pasta <- tempfile("verificacao-")
  dir.create(pasta)
  local_mock_global(gs_cep_para_coordenadas = function(cep, fonte, dir) {
    data.frame(
      cep = "01234-567", latitude = -23.55, longitude = -46.63,
      fonte = "nominatim", precisao = "coordenada aproximada da rua",
      stringsAsFactors = FALSE
    )
  })

  resultado <- gs_verificar_cep(
    "01234-567", -23.55, -46.63, tolerancia_m = 300, dir = pasta
  )
  expect_true(is.na(resultado$confere))
  expect_identical(resultado$veredito, "SEM DADO SUFICIENTE")
})

test_that("interfaces publicas validam argumentos escalares", {
  pasta <- tempfile("validacao-")
  dir.create(pasta)
  expect_error(gs_normalizar_cep(c("01234-567", "87654-321")), "escalar")
  expect_error(
    gs_resolver_ponto("01234-567", c(-23.5, -46.6), dir = pasta),
    "exatamente um"
  )
  expect_error(gs_resolver_ponto(coordenadas = c(91, -46), dir = pasta),
               "latitude")
  expect_error(gs_verificar_cep("01234-567", c(-23, -24), -46, dir = pasta),
               "latitude")
  expect_error(gs_verificar_cep("01234-567", -23, -46,
                                tolerancia_m = -1, dir = pasta),
               "tolerancia_m")
  expect_error(gs_servicos_proximos(coordenadas = c(-23, -46), raio_m = 0,
                                    dir = pasta), "raio_m")
  expect_error(gs_servicos_proximos(coordenadas = c(-23, -46), raio_m = 10,
                                    n_por_camada = 1.5, dir = pasta),
               "n_por_camada")
})

test_that("proximidade ignora corpo poligonal e retorna vazio consistente", {
  pasta <- tempfile("proximidade-")
  dir.create(pasta)
  writeLines(
    c("id,geometria_wkt", "1,\"geometria deliberadamente malformada"),
    file.path(pasta, "poligono.csv")
  )
  writeLines(
    c("nm_equipamento,latitude,longitude", "Muito longe,-22,-45"),
    file.path(pasta, "pontos.csv")
  )

  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), raio_m = 10, dir = pasta
  )
  expect_equal(nrow(resultado), 0)
  expect_identical(
    names(resultado),
    c("camada", "nome", "tipo_servico", "endereco", "bairro",
      "distancia_m", "distancia_m_exata", "latitude", "longitude")
  )
  expect_equal(attr(resultado, "raio_m"), 10)
  expect_identical(attr(resultado, "tipo_distancia"), "geodesica")
  expect_false(attr(resultado, "amostra_truncada"))
  expect_null(attr(resultado, "n_por_camada"))
})

test_that("proximidade registra se o limite realmente truncou a amostra", {
  pasta <- tempfile("proximidade-limite-")
  dir.create(pasta)
  writeLines(c(
    "nm_equipamento,latitude,longitude",
    "A,-23.5500,-46.6300",
    "B,-23.5505,-46.6300",
    "C,-23.5510,-46.6300"
  ), file.path(pasta, "pontos.csv"))

  truncado <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), raio_m = 1000,
    n_por_camada = 2, dir = pasta
  )
  completo <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), raio_m = 1000,
    n_por_camada = 5, dir = pasta
  )
  expect_equal(nrow(truncado), 2)
  expect_identical(attr(truncado, "n_por_camada"), 2L)
  expect_true(attr(truncado, "amostra_truncada"))
  expect_false(attr(completo, "amostra_truncada"))
})

test_that("filtro do raio usa distancia sem arredondamento", {
  pasta <- tempfile("proximidade-arredondamento-")
  dir.create(pasta)
  writeLines(c(
    "nm_equipamento,latitude,longitude",
    "Fora,-23.5500,-46.6300"
  ), file.path(pasta, "pontos.csv"))
  local_mock_global(gs_calcular_distancias = function(...) 1000.04)

  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), raio_m = 1000, dir = pasta
  )
  expect_equal(nrow(resultado), 0)
})

test_that("distancia exata acompanha o resultado para cortes posteriores", {
  pasta <- tempfile("proximidade-exata-")
  dir.create(pasta)
  writeLines(c(
    "nm_equipamento,latitude,longitude",
    "Limite,-23.5500,-46.6300"
  ), file.path(pasta, "pontos.csv"))
  local_mock_global(gs_calcular_distancias = function(...) 500.04)

  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), raio_m = 1000, dir = pasta
  )
  expect_equal(resultado$distancia_m, 500)
  expect_equal(resultado$distancia_m_exata, 500.04)
  progressiva <- gs_analise_raios(resultado, attr(resultado, "ponto"), raios = 500)
  expect_equal(progressiva$contagem$n_servicos, 0)

  dois <- rbind(resultado, resultado)
  dois$distancia_m_exata <- c(100.04, 200.04)
  expect_equal(gs_distancias_resultado(dois[2:1, , drop = FALSE]),
               c(200.04, 100.04))

  legado <- data.frame(distancia_m = c(100, 200))
  attr(legado, "distancias_m_exatas") <- c(100.04, 200.04)
  expect_equal(gs_distancias_resultado(legado), c(100.04, 200.04))
  expect_equal(gs_distancias_resultado(legado[2:1, , drop = FALSE]),
               c(200, 100))
  colisao <- data.frame(id = c("a", "b"), distancia_m = c(100, 100))
  attr(colisao, "distancias_m_exatas") <- c(100.01, 100.04)
  expect_equal(gs_distancias_resultado(colisao[2:1, , drop = FALSE]),
               c(100, 100))
})

test_that("proveniencia preserva camadas consultadas sem ocorrencias no raio", {
  pasta <- tempfile("proximidade-zero-")
  dir.create(pasta)
  writeLines(c(
    "nm_equipamento,latitude,longitude",
    "Perto,-23.5500,-46.6300"
  ), file.path(pasta, "equipamento_perto.csv"))
  writeLines(c(
    "nm_equipamento,latitude,longitude",
    "Longe,-22.0000,-45.0000"
  ), file.path(pasta, "equipamento_longe.csv"))
  bloqueios <- logical(0)
  leitor_real <- gs_ler_csv_verificado
  local_mock_global(gs_ler_csv_verificado = function(arquivo, ...) {
    bloqueios <<- c(bloqueios, dir.exists(gs_lock_diretorio(dirname(arquivo))))
    leitor_real(arquivo, ...)
  })

  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), camadas = c("perto", "longe"),
    raio_m = 1000, dir = pasta
  )
  amostragem <- attr(resultado, "amostragem_por_camada")
  expect_setequal(amostragem$camada,
                  c("equipamento_perto", "equipamento_longe"))
  expect_equal(amostragem$n_disponivel[
    amostragem$camada == "equipamento_longe"
  ], 0)
  expect_true(length(bloqueios) == 2 && all(bloqueios))
})

test_that("conversao OSRM preserva precisao antes da exibicao", {
  skip_if_not_installed("osrm")
  valor <- gs_osrm_dist_m(1000.04)
  esperado <- if (utils::packageVersion("osrm") >= "4.0.0") 1000.04 else 1000040
  expect_equal(valor, esperado)
})

test_that("funcoes de leitura nao criam data", {
  raiz_temp <- tempfile("raiz-")
  dir.create(raiz_temp)
  withr::local_options(list(gs.raiz = raiz_temp, gs.indice_cep = NULL))

  expect_identical(gs_camadas_local(), character(0))
  expect_error(gs_indice_cep(), class = "gs_indice_ausente")
  expect_false(dir.exists(file.path(raiz_temp, "data")))
})

test_that("opcoes OSRM sao restauradas apos a consulta", {
  skip_if_not(requireNamespace("osrm", quietly = TRUE))
  withr::local_options(list(
    osrm.server = "http://antes.test/",
    osrm.profile = "antes",
    gs.osrm_server = "http://durante.test/",
    gs.osrm_profile = "driving"
  ))
  local_mock_global(gs_consultar_tabela_osrm = function(origem, destinos) {
    expect_identical(getOption("osrm.server"), "http://durante.test/")
    expect_identical(getOption("osrm.profile"), "driving")
    list(distances = matrix(100, nrow = 1))
  })

  origem <- sf::st_sfc(sf::st_point(c(-46.63, -23.55)), crs = 4326)
  destinos <- sf::st_sfc(sf::st_point(c(-46.62, -23.54)), crs = 4326)
  invisible(gs_calcular_distancias(origem, destinos, "rede_viaria"))
  expect_identical(getOption("osrm.server"), "http://antes.test/")
  expect_identical(getOption("osrm.profile"), "antes")
})

test_that("OSRM limita cada bloco a 99 destinos mais a origem", {
  skip_if_not(requireNamespace("osrm", quietly = TRUE))
  tamanhos <- integer(0)
  local_mock_global(gs_consultar_tabela_osrm = function(origem, destinos) {
    tamanhos <<- c(tamanhos, nrow(destinos))
    list(distances = matrix(rep(100, nrow(destinos)), nrow = 1))
  })
  origem <- sf::st_sfc(sf::st_point(c(-46.63, -23.55)), crs = 4326)
  destinos <- sf::st_sfc(
    lapply(seq_len(100), function(i) sf::st_point(c(-46.63 + i / 1e5, -23.55))),
    crs = 4326
  )
  d <- gs_calcular_distancias(origem, destinos, "rede_viaria")
  expect_length(d, 100)
  expect_identical(tamanhos, c(99L, 1L))
})
