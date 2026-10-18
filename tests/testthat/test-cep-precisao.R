# Cenários sintéticos: nenhuma consulta externa deve sair destes mocks.
local_cenario_cep <- function(nivel, .local_envir = parent.frame()) {
  pasta <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_options(list(gs.indice_cep = NULL), .local_envir = .local_envir)
  readr::write_csv(data.frame(
    nm_equipamento = c("UBS A", "UBS B", "UBS C"),
    nm_tipo_equipamento = "Saúde",
    cd_cep_equipamento = if (nivel == "local") "01234-567" else "99999-999",
    latitude = c(-23.551, -23.550, -23.549),
    longitude = c(-46.631, -46.630, -46.629)
  ), file.path(pasta, "equipamento_saude_teste.csv"))
  estado <- new.env(parent = emptyenv())
  estado$consultas <- character()
  estado$viacep <- 0L
  local_mock_global(
    gs_http_get = function(...) stop("Consulta HTTP inesperada no teste"),
    gs_consultar_nominatim = function(query) {
      tipo <- if (!is.null(query$postalcode)) "cep" else if (
        !is.null(query$street)) "rua" else "cidade"
      estado$consultas <- c(estado$consultas, tipo)
      if (nivel %in% c("manual", "local")) stop("Consulta externa inesperada")
      if (tipo != nivel) return(NULL)
      list(latitude = -23.55, longitude = -46.63, nome = "Referência sintética")
    },
    gs_ler_cep = function(cep) {
      estado$viacep <- estado$viacep + 1L
      if (nivel %in% c("manual", "local", "cep")) stop("ViaCEP inesperado")
      data.frame(cep = gs_cep_mascarado(cep), logradouro = "Rua A",
                 bairro = "Centro", cidade = "São Paulo", uf = "SP")
    },
    .local_envir = .local_envir
  )
  list(pasta = pasta, estado = estado)
}

for (nivel in c("cep", "rua", "local", "manual")) {
  test_that(paste("fonte e precisao chegam aos mapas e relatorios:", nivel), {
    cenario <- local_cenario_cep(nivel)
    if (nivel == "manual") {
      resultado <- gs_servicos_proximos(
        coordenadas = c(-23.55, -46.63), raio_m = 1000, dir = cenario$pasta
      )
      fonte <- "informada"
      precisao <- "coordenadas informadas pelo usuário (precisão não aferida)"
    } else {
      ref <- gs_cep_para_coordenadas("01234-567", dir = cenario$pasta)
      expect_identical(ref$fonte, if (nivel == "local") "local" else "nominatim")
      expect_match(ref$precisao, switch(nivel, cep = "código postal",
                                       rua = "rua", local = "mediana"))
      expect_identical(cenario$estado$consultas, switch(
        nivel, cep = "cep", rua = c("cep", "rua"), local = character()
      ))
      expect_identical(cenario$estado$viacep, if (nivel == "rua") 1L else 0L)
      fonte <- ref$fonte
      precisao <- ref$precisao
      resultado <- gs_servicos_proximos(
        cep = "01234-567", raio_m = 1000, dir = cenario$pasta
      )
    }

    expect_equal(nrow(resultado), 3)
    ponto <- attr(resultado, "ponto")
    expect_identical(ponto$fonte, fonte)
    expect_identical(ponto$precisao, precisao)
    expect_equal(c(ponto$latitude, ponto$longitude), c(-23.55, -46.63))
    expect_equal(as.numeric(sf::st_coordinates(ponto$sf)), c(-46.63, -23.55))
    if (nivel == "manual") {
      expect_identical(ponto$origem, "coordenadas informadas")
      expect_length(cenario$estado$consultas, 0)
      expect_identical(cenario$estado$viacep, 0L)
    }

    saida <- file.path(cenario$pasta, "exportacao")
    gs_exportar_resultado(resultado, list(), saida)
    metadados <- readr::read_csv(file.path(saida, "metadados_consulta.csv"),
                                show_col_types = FALSE)
    expect_identical(metadados$fonte, fonte)
    expect_identical(metadados$precisao, precisao)

    estatico <- gs_mapa_servicos(resultado, interativo = FALSE)
    interativo <- gs_mapa_servicos(resultado, interativo = TRUE)
    chamadas <- interativo$x$calls
    metodos <- vapply(chamadas, function(x) x$method, character(1))
    marcador <- chamadas[metodos == "addCircleMarkers"][[1]]$args
    controle <- chamadas[metodos == "addControl"][[1]]$args[[1]]

    arquivos <- file.path(saida, c("numerico.md", "analises.md", "analises.html"))
    gs_relatorio_numerico(resultado, list(), arquivos[1])
    for (i in 2:3) {
      gs_relatorio_analises(
        resultado = resultado, analises = list(), arquivo = arquivos[i],
        formato = if (i == 2) "md" else "html", manifesto = gs_manifesto_vazio()
      )
    }
    textos <- c(
      estatico$labels$subtitle, marcador[[9]], as.character(marcador[[11]]),
      controle, vapply(arquivos, function(x) paste(readLines(x, warn = FALSE),
                                                  collapse = "\n"), character(1))
    )
    if (nivel == "manual") {
      expect_false(any(grepl("localização aproximada", textos, ignore.case = TRUE)))
    } else {
      for (texto in textos) {
        texto <- gsub("[[:space:]]+", " ", texto)
        expect_match(texto, "localização aproximada", ignore.case = TRUE)
        expect_match(texto, fonte, fixed = TRUE)
        expect_match(texto, precisao, fixed = TRUE)
      }
    }
  })
}

test_that("cidade continua consultável mas não serve como origem de buscas", {
  cenario <- local_cenario_cep("cidade")
  ref <- gs_cep_para_coordenadas("01234-567", dir = cenario$pasta)
  expect_identical(ref$fonte, "nominatim")
  expect_match(ref$precisao, "aproximada da cidade")
  expect_equal(c(ref$latitude, ref$longitude), c(-23.55, -46.63))
  expect_identical(cenario$estado$consultas, c("cep", "rua", "cidade"))
  expect_identical(cenario$estado$viacep, 1L)
  local_mock_global(gs_resolver_camadas = function(...) {
    stop("A busca prosseguiu com referência da cidade")
  })
  mensagem <- "apenas.*cidade.*[Ii]nforme.*coordenadas"
  expect_error(gs_resolver_ponto(cep = "01234-567", dir = cenario$pasta), mensagem)
  expect_error(gs_servicos_proximos(cep = "01234-567", dir = cenario$pasta), mensagem)
  expect_error(gs_mapa_servicos(cep = "01234-567", dir = cenario$pasta), mensagem)
  expect_error(gs_relatorio_analises(
    cep = "01234-567", dir = cenario$pasta,
    arquivo = file.path(cenario$pasta, "cidade.html")
  ), mensagem)
  expect_false(file.exists(file.path(cenario$pasta, "cidade.html")))
})

test_that("metadados aceitam resultados antigos sem fonte e precisao", {
  cenario <- local_cenario_cep("manual")
  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), raio_m = 1000, dir = cenario$pasta
  )
  ponto <- attr(resultado, "ponto")
  ponto$fonte <- ponto$precisao <- NULL
  attr(resultado, "ponto") <- ponto
  metadados <- gs_metadados_consulta(resultado)
  expect_identical(metadados$fonte, NA_character_)
  expect_identical(metadados$precisao, NA_character_)
  expect_identical(metadados$origem, "coordenadas informadas")
})

test_that("busca vazia ainda preserva fonte e precisao nos metadados", {
  cenario <- local_cenario_cep("rua")
  local_mock_global(gs_consultar_nominatim = function(query) {
    if (is.null(query$street)) return(NULL)
    list(latitude = -23.56, longitude = -46.64, nome = "Rua A")
  })
  resultado <- gs_servicos_proximos(
    cep = "01234-567", raio_m = 1, dir = cenario$pasta
  )
  expect_equal(nrow(resultado), 0)
  metadados <- gs_metadados_consulta(resultado)
  expect_identical(metadados$fonte, "nominatim")
  expect_match(metadados$precisao, "aproximada da rua")
})

test_that("lote registra a cidade como erro e mantém a origem manual", {
  cenario <- local_cenario_cep("cidade")
  lote <- gs_analisar_locais(
    cep = "01234-567", coordenadas = c(-23.55, -46.63),
    ids = c("cidade", "manual"), raio_m = 1000, tipo = "descritivas",
    dir = cenario$pasta, dir_saida = file.path(cenario$pasta, "saidas"),
    nome_execucao = "lote", formato_relatorio = "md", salvar_mapa = FALSE
  )
  expect_identical(lote$origens$status, c("erro", "ok"))
  expect_match(lote$origens$mensagem[1], "apenas.*cidade.*[Ii]nforme.*coordenadas")
  expect_identical(unique(lote$servicos$id_origem), "manual")
  expect_false(file.exists(file.path(lote$pasta, "origens", "cidade",
                                     "metadados_consulta.csv")))
  metadados <- readr::read_csv(
    file.path(lote$pasta, "origens", "manual", "metadados_consulta.csv"),
    show_col_types = FALSE
  )
  expect_identical(metadados$fonte, "informada")
  expect_match(metadados$precisao, "precisão não aferida")
})
