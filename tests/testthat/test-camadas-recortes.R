local_base_recortes <- function(completa = TRUE, .local_envir = parent.frame()) {
  pasta <- withr::local_tempdir(.local_envir = .local_envir)
  camada <- "equipamento_saude_teste"
  recorte <- paste0(camada, "__filtro_unidade_a_12345678")
  # A e B compartilham coordenadas; A e C compartilham nome. São três unidades.
  dados <- data.frame(
    cd_identificador = c("A", "B", "C"),
    nm_equipamento = c("Unidade A", "Unidade B", "Unidade A"),
    nm_tipo_equipamento = "Saúde",
    latitude = c(-23.55, -23.55, -23.551),
    longitude = c(-46.63, -46.63, -46.631)
  )
  if (completa) readr::write_csv(dados, file.path(pasta, paste0(camada, ".csv")))
  readr::write_csv(dados[1, ], file.path(pasta, paste0(recorte, ".csv")))
  local_mock_global(
    gs_http_get = function(...) stop("Consulta externa inesperada"),
    .local_envir = .local_envir
  )
  list(pasta = pasta, camada = camada, recorte = recorte)
}

test_that("selecoes automaticas usam a camada completa sem acrescentar o recorte", {
  base <- local_base_recortes()
  arquivos <- list.files(base$pasta, full.names = TRUE)
  hashes <- tools::md5sum(arquivos)
  selecoes <- list(NULL, "todos", "all", "todas", "TODAS", "saude", "SaUdE",
                   "teste", "equipamento_saude", "__filtro_")
  for (selecao in selecoes) {
    resolvidas <- gs_resolver_camadas(selecao, dir = base$pasta)
    if (identical(selecao, "__filtro_")) {
      expect_length(resolvidas$resolvidas, 0)
      expect_identical(resolvidas$nao_encontradas, selecao)
      next
    }
    expect_identical(resolvidas$resolvidas, base$camada)
    expect_length(resolvidas$nao_encontradas, 0)
    resultado <- gs_servicos_proximos(
      coordenadas = c(-23.55, -46.63), camadas = selecao,
      raio_m = 1000, dir = base$pasta
    )
    expect_equal(nrow(resultado), 3)
    expect_identical(unique(resultado$camada), base$camada)
    expect_equal(sum(resultado$nome == "Unidade A"), 2)
    expect_equal(sum(resultado$latitude == -23.55 & resultado$longitude == -46.63), 2)
    expect_identical(attr(resultado, "amostragem_por_camada")$camada, base$camada)
  }
  expect_identical(list.files(base$pasta, full.names = TRUE), arquivos)
  expect_identical(tools::md5sum(arquivos), hashes)
})

test_that("nome completo seleciona recortes com prefixo ou em data.frame", {
  base <- local_base_recortes()
  selecoes <- list(base$recorte, paste0("geoportal:", base$recorte),
                   data.frame(camada = base$recorte))
  for (selecao in selecoes) {
    resolvidas <- gs_resolver_camadas(selecao, dir = base$pasta)
    expect_identical(resolvidas$resolvidas, base$recorte)
    expect_length(resolvidas$nao_encontradas, 0)
    resultado <- gs_servicos_proximos(
      coordenadas = c(-23.55, -46.63), camadas = selecao,
      raio_m = 1000, dir = base$pasta
    )
    expect_equal(nrow(resultado), 1)
    expect_identical(resultado$camada, base$recorte)
    expect_identical(resultado$nome, "Unidade A")
  }
  expect_identical(gs_resolver_camadas(base$camada, base$pasta)$resolvidas,
                   base$camada)
})

test_that("recorte explicitamente combinado com tema respeita a escolha sem deduplicar", {
  base <- local_base_recortes()
  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), camadas = c("saude", base$recorte),
    raio_m = 1000, dir = base$pasta
  )
  expect_equal(nrow(resultado), 4)
  expect_setequal(unique(resultado$camada), c(base$camada, base$recorte))
})

test_that("somente recortes exige selecao explicita e orienta o usuario", {
  base <- local_base_recortes(completa = FALSE)
  for (selecao in list(NULL, "todos", "all", "todas", "saude", "teste")) {
    expect_length(gs_resolver_camadas(selecao, base$pasta)$resolvidas, 0)
    expect_error(
      gs_servicos_proximos(
        coordenadas = c(-23.55, -46.63), camadas = selecao,
        raio_m = 1000, dir = base$pasta
      ),
      "recortes.*nome completo.*camadas"
    )
  }
  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.55, -46.63), camadas = base$recorte,
    raio_m = 1000, dir = base$pasta
  )
  expect_equal(nrow(resultado), 1)
  expect_identical(resultado$camada, base$recorte)
})

test_that("listagem mostra os nomes dos recortes e explica a selecao explicita", {
  base <- local_base_recortes(completa = FALSE)
  texto <- capture.output(tab <- gs_listar_servicos(dir = base$pasta))
  expect_identical(names(tab), c("tema", "camada"))
  expect_identical(tab$camada, base$recorte)
  expect_match(paste(texto, collapse = " "), base$recorte, fixed = TRUE)
  expect_match(paste(texto, collapse = " "), "recortes.*nome completo")
})

test_that("recorte de outro tema nao entra por tema nem por correspondencia parcial", {
  base <- local_base_recortes()
  recorte <- "equipamento_cultura_teste__filtro_bairro_12345678"
  file.copy(file.path(base$pasta, paste0(base$recorte, ".csv")),
             file.path(base$pasta, paste0(recorte, ".csv")))
  expect_length(gs_resolver_camadas("cultura", base$pasta)$resolvidas, 0)
  expect_identical(gs_resolver_camadas("teste", base$pasta)$resolvidas, base$camada)
  expect_identical(gs_resolver_camadas(recorte, base$pasta)$resolvidas, recorte)
})
