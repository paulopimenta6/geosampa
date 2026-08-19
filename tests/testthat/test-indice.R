test_that("gs_indice_cep tem n_ocorrencias e representante (1 por CEP)", {
  skip_if_not(tem_dados())
  idx <- gs_indice_cep(force = TRUE)
  expect_true(all(c("n_ocorrencias", "representante") %in% names(idx)))
  expect_equal(sum(idx$representante), length(unique(idx$cep)))
  expect_true(all(idx$n_ocorrencias >= 1))
})

test_that("gs_cep_referencia devolve 1 linha por CEP", {
  skip_if_not(tem_dados())
  ref <- gs_cep_referencia()
  expect_equal(nrow(ref), length(unique(ref$cep)))
})