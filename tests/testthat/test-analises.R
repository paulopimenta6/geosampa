test_that("gs_analise_servicos devolve todos os tipos pedidos", {
  skip_if_not(tem_dados())
  tipos <- c("descritivas", "vizinho_mais_proximo", "raios_progressivos",
             "acessibilidade_media", "raio_otimo", "nni")
  a <- gs_analise_servicos(cep = "03175-001", raio_m = 2000, tipo = tipos)
  expect_true(all(tipos %in% names(a)))
  expect_true(is.data.frame(a$raios_progressivos$contagem))
  expect_true(inherits(a$raios_progressivos$grafico, "ggplot"))
  expect_true(is.data.frame(a$raio_otimo$percentis))
  expect_true(inherits(a$raio_otimo$grafico, "ggplot"))
  expect_true(is.data.frame(a$acessibilidade_media$por_camada))
  expect_true(inherits(a$acessibilidade_media$grafico_ecdf, "ggplot"))
})

test_that("gs_analise_acessibilidade devolve medidas robustas", {
  skip_if_not(tem_dados())
  prox <- gs_servicos_proximos(cep = "03175-001", raio_m = 2000)
  a <- gs_analise_acessibilidade(prox)
  expect_true(all(c("p25", "mediana", "p75", "iqr", "cv") %in% names(a$geral)))
})

test_that("gs_interpretar_analise gera leitura para análises executadas", {
  skip_if_not(tem_dados())
  prox <- gs_servicos_proximos(cep = "03175-001", raio_m = 2000)
  a <- gs_analise_servicos(prox, tipo = c("descritivas", "nni"))
  interp <- gs_interpretar_analise(a, prox, 2000)
  expect_true(all(c("descritivas", "nni") %in% names(interp)))
  expect_true(nzchar(interp$descritivas))
  expect_true(grepl("mediana", interp$descritivas))
})

test_that("gs_analise_moran usa grade hexagonal por padrão", {
  skip_if_not(tem_dados())
  skip_if(!requireNamespace("spdep", quietly = TRUE), "spdep não instalado")
  prox <- gs_servicos_proximos(cep = "03175-001", raio_m = 3000)
  m <- gs_analise_moran(prox)
  expect_true(m$executado)
  expect_identical(m$metodo, "grade_hex")
  expect_true(all(c("moran_i", "valor_p", "n_celulas", "interpretacao") %in% names(m)))
})

test_that("gs_analise_nni devolve estrutura completa", {
  skip_if_not(tem_dados())
  prox <- gs_servicos_proximos(cep = "03175-001", raio_m = 3000)
  n <- gs_analise_nni(prox)
  expect_true(n$executado)
  expect_true(all(c("indice_nni", "interpretacao", "valor_p") %in% names(n)))
  expect_true(is.numeric(n$indice_nni))
})

test_that("gs_analise_cobertura devolve area e percentual", {
  skip_if_not(tem_dados())
  prox <- gs_servicos_proximos(cep = "03175-001", raio_m = 3000)
  c <- gs_analise_cobertura(prox, attr(prox, "ponto"))
  expect_true(c$executado)
  expect_true(all(c("area_coberta_km2", "pct_cobertura", "por_camada") %in% names(c)))
})

test_that("moran sem spdep nao quebra", {
  skip_if_not(tem_dados())
  skip_if(requireNamespace("spdep", quietly = TRUE), "spdep instalado; teste de robustez ignorado")
  prox <- gs_servicos_proximos(cep = "03175-001", raio_m = 3000)
  m <- gs_analise_moran(prox)
  expect_false(m$executado)
})

test_that("gs_interpretar_analise nao confunde nomes por partial matching", {
  skip_if_not(tem_dados())
  prox <- gs_servicos_proximos(cep = "03175-001", raio_m = 3000)
  a <- gs_analise_servicos(prox, tipo = c("moran_distrital", "kde_banda"))
  interp <- gs_interpretar_analise(a, prox, 3000)
  expect_null(interp[["moran"]])    # "moran" não pode casar com "moran_distrital"
  expect_null(interp[["kde"]])      # "kde" não pode casar com "kde_banda"
  expect_true(all(c("moran_distrital", "kde_banda") %in% names(interp)))
})