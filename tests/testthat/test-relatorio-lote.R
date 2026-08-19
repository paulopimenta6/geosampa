cria_base_lote <- function(dir) {
  pontos <- data.frame(
    nm_equipamento = paste("Serviço", 1:8),
    nm_tipo_equipamento = rep(c("Saúde", "Educação"), 4),
    tx_endereco_equipamento = paste("Rua", 1:8),
    nm_bairro_equipamento = "Centro",
    cd_cep_equipamento = "01001-000",
    latitude = c(-23.5500, -23.5510, -23.5520, -23.5530,
                 -23.5540, -23.5490, -23.5480, -23.5550),
    longitude = c(-46.6300, -46.6310, -46.6290, -46.6320,
                  -46.6280, -46.6330, -46.6270, -46.6305),
    stringsAsFactors = FALSE
  )
  readr::write_csv(pontos, file.path(dir, "equipamento_saude_teste.csv"))
  invisible(pontos)
}

test_that("coletor salva plots, tabelas, metricas e sf como GeoJSON", {
  pasta <- withr::local_tempdir()
  dados <- cria_base_lote(pasta)
  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.551, -46.630), raio_m = 3000, dir = pasta
  )
  analises <- gs_analise_servicos(
    resultado, tipo = c("descritivas", "voronoi", "acessibilidade_media"),
    dir = pasta
  )
  saida <- file.path(pasta, "artefatos")
  manifesto <- gs_exportar_resultado(resultado, analises, saida, id_origem = "A")

  expect_true(file.exists(file.path(saida, "metricas.csv")))
  expect_true(file.exists(file.path(saida, "metadados_consulta.csv")))
  expect_true(file.exists(file.path(saida, "amostragem_por_camada.csv")))
  expect_true(any(manifesto$categoria == "figura" & manifesto$status == "ok"))
  expect_true(any(manifesto$categoria == "geometria" &
                    manifesto$formato == "geojson" & manifesto$status == "ok"))
  expect_false(any(manifesto$categoria == "geometria" &
                     manifesto$formato == "csv"))
  expect_true(all(file.exists(manifesto$caminho[manifesto$status == "ok"])))

  metricas <- readr::read_csv(file.path(saida, "metricas.csv"),
                              show_col_types = FALSE)
  expect_true(any(metricas$metrica == "estatisticas_distancia.mediana"))
  expect_true(any(metricas$metrica == "estatisticas_distancia.mad"))
  metadados <- readr::read_csv(file.path(saida, "metadados_consulta.csv"),
                               show_col_types = FALSE)
  expect_identical(metadados$tipo_distancia, "geodesica")
  expect_false(metadados$amostra_truncada)
})

test_that("salvamento consolidado mantém PNG e gera relatórios numéricos", {
  pasta <- withr::local_tempdir()
  cria_base_lote(pasta)
  resultado <- gs_servicos_proximos(
    coordenadas = c(-23.551, -46.630), raio_m = 2500, dir = pasta
  )
  analises <- gs_analise_servicos(
    resultado,
    tipo = c("descritivas", "acessibilidade_media", "raio_otimo", "nni"),
    dir = pasta
  )
  saida <- file.path(pasta, "consolidado")
  salvo <- gs_salvar_analises(
    resultado, analises, saida, id_origem = "origem_teste",
    formato_relatorio = "ambos", sobrescrever = FALSE
  )

  expect_true(file.exists(file.path(saida, "relatorio_numerico.md")))
  expect_true(file.exists(file.path(saida, "relatorio_analises.md")))
  expect_true(file.exists(file.path(saida, "relatorio_analises.html")))
  expect_true(file.exists(file.path(saida, "figuras", "mapa_servicos.png")))
  expect_gt(length(list.files(file.path(saida, "figuras"), pattern = "[.]png$")), 2)
  expect_true(all(file.exists(salvo$manifesto$caminho[
    salvo$manifesto$status == "ok"
  ])))
  html <- paste(readLines(file.path(saida, "relatorio_analises.html"),
                          warn = FALSE), collapse = "\n")
  expect_match(html, "<table class=\"tabela\">", fixed = TRUE)
  expect_match(html, "Relatório", fixed = TRUE)
})

test_that("execução em lote aceita várias coordenadas e consolida resultados", {
  pasta <- withr::local_tempdir()
  dados <- file.path(pasta, "dados")
  saidas <- file.path(pasta, "saidas")
  dir.create(dados)
  cria_base_lote(dados)

  lote <- gs_analisar_locais(
    coordenadas = data.frame(
      id = c("Casa", "Casa"),
      latitude = c(-23.551, -23.554),
      longitude = c(-46.630, -46.629)
    ),
    camadas = "saude", raio_m = 2500,
    tipo = c("descritivas", "acessibilidade_media", "raio_otimo"),
    dir = dados, dir_saida = saidas, nome_execucao = "teste_lote",
    formato_relatorio = "md", salvar_mapa = TRUE
  )

  expect_s3_class(lote, "gs_lote")
  expect_identical(lote$origens$id_origem, c("casa", "casa__02"))
  expect_true(all(lote$origens$status == "ok"))
  expect_setequal(unique(lote$servicos$id_origem), lote$origens$id_origem)
  expect_equal(nrow(lote$comparacao), 2)
  expect_true(file.exists(file.path(lote$pasta, "origens.csv")))
  expect_true(file.exists(file.path(lote$pasta, "comparacao_origens.csv")))
  expect_true(file.exists(file.path(lote$pasta, "relatorio_lote.md")))
  expect_true(file.exists(file.path(lote$pasta, "figuras",
                                    "comparacao_contagens.png")))
})

test_that("normalização do lote combina CEPs e coordenadas com IDs seguros", {
  origens <- gs_normalizar_origens(
    cep = c(casa = "01001-000", trabalho = "03175-001"),
    coordenadas = c(-23.55, -46.63),
    ids = c("../../casa", "Trabalho", "Ponto manual")
  )
  expect_equal(nrow(origens), 3)
  expect_identical(origens$id_origem,
                   c("casa", "trabalho", "ponto_manual"))
  expect_false(any(grepl("[./\\\\]", origens$id_origem)))
})

test_that("conteúdo do usuário é escapado nos popups Leaflet", {
  resultado <- data.frame(
    nome = "<script>alert(1)</script>", tipo_servico = "<b>tipo</b>",
    endereco = "Rua & Avenida", bairro = "Centro", distancia_m = 10,
    camada = "teste", stringsAsFactors = FALSE
  )
  popup <- gs_popup_servicos(resultado)
  label <- gs_label_servicos(resultado)
  expect_false(grepl("<script>", popup, fixed = TRUE))
  expect_match(popup, "&lt;script&gt;", fixed = TRUE)
  expect_match(label, "&lt;script&gt;", fixed = TRUE)
  expect_match(label, "<b>", fixed = TRUE)
})
