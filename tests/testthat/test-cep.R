test_that("gs_verificar_cep confirma coordenada conhecida da UBS Agua Rasa", {
  skip_if_not(tem_dados())
  v <- gs_verificar_cep("03175-001", -23.553640, -46.580180)
  expect_true(v$confere)
  expect_true(v$veredito %in% c("CONFERE", "NAO CONFERE", "SEM DADO SUFICIENTE"))
  expect_true(all(c("cep", "distancia_m", "tolerancia_m") %in% names(v)))
})

test_that("gs_ler_cep normaliza CEP com e sem hifen", {
  skip_on_cran()
  expect_error(gs_normalizar_cep("123"), "CEP inválido")
  expect_equal(gs_normalizar_cep("03175-001"), "03175001")
  expect_equal(gs_cep_mascarado("03175001"), "03175-001")
})

test_that("gs_cep_para_coordenadas resolve via indice local", {
  skip_if_not(tem_dados())
  ref <- gs_cep_referencia()
  skip_if(!any(ref$cep == "03175001"))
  c <- gs_cep_para_coordenadas("03175-001")
  expect_equal(c$cep, "03175-001")
  expect_true(c$fonte %in% c("local", "nominatim"))
})