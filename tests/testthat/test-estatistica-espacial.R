cria_resultado_espacial <- function(deslocamentos, raio_m = 2500,
                                    camadas = NULL) {
  deslocamentos <- as.matrix(deslocamentos)
  if (ncol(deslocamentos) != 2) stop("Deslocamentos devem ter duas colunas.")
  n <- nrow(deslocamentos)
  if (is.null(camadas)) camadas <- rep("teste", n)

  ponto_sf <- sf::st_sfc(sf::st_point(c(-46.63, -23.55)), crs = gs_epsg$wgs84)
  ponto_utm <- sf::st_transform(ponto_sf, gs_epsg$oficial)
  centro <- sf::st_coordinates(ponto_utm)[1, ]
  xy <- sweep(deslocamentos, 2, centro, "+")
  pts <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]),
                      coords = c("x", "y"), crs = gs_epsg$oficial)
  lonlat <- sf::st_coordinates(sf::st_transform(pts, gs_epsg$wgs84))
  resultado <- data.frame(
    camada = camadas,
    nome = paste0("servico_", seq_len(n)),
    tipo_servico = "tipo_teste",
    distancia_m = sqrt(rowSums(deslocamentos^2)),
    latitude = lonlat[, 2],
    longitude = lonlat[, 1],
    stringsAsFactors = FALSE
  )
  attr(resultado, "ponto") <- list(
    origem = "ponto de teste", longitude = -46.63, latitude = -23.55,
    sf = ponto_sf
  )
  attr(resultado, "raio_m") <- raio_m
  resultado
}

cria_distritos_teste <- function() {
  ponto <- sf::st_sfc(sf::st_point(c(-46.63, -23.55)), crs = gs_epsg$wgs84)
  centro <- sf::st_coordinates(sf::st_transform(ponto, gs_epsg$oficial))[1, ]
  centros <- expand.grid(x = c(-900, 0, 900), y = c(-900, 0, 900))
  poligonos <- lapply(seq_len(nrow(centros)), function(i) {
    x <- centro[1] + centros$x[i]
    y <- centro[2] + centros$y[i]
    sf::st_polygon(list(rbind(
      c(x - 450, y - 450), c(x + 450, y - 450),
      c(x + 450, y + 450), c(x - 450, y + 450),
      c(x - 450, y - 450)
    )))
  })
  sf::st_sf(
    nm_distrito_municipal = paste0("Distrito ", seq_len(nrow(centros))),
    geometry = sf::st_sfc(poligonos, crs = gs_epsg$oficial)
  )
}

test_that("Voronoi usa MULTIPOINT deduplicado, associa sitios e recorta ao buffer", {
  resultado <- cria_resultado_espacial(
    rbind(c(-1000, 0), c(1000, 0), c(0, 1000), c(-1000, 0)),
    raio_m = 2000,
    camadas = c("A", "B", "C", "D")
  )

  vor <- gs_analise_voronoi(resultado)
  expect_s3_class(vor, "sf")
  expect_equal(nrow(vor), 3)
  expect_equal(attr(vor, "n_deduplicados"), 1)

  dominio <- gs_dominio_consulta(resultado)$dominio
  vor_utm <- sf::st_transform(vor, gs_epsg$oficial)
  expect_equal(sum(as.numeric(sf::st_area(vor_utm))),
               as.numeric(sf::st_area(dominio)), tolerance = 1)

  pontos_unicos <- cria_resultado_espacial(
    rbind(c(-1000, 0), c(1000, 0), c(0, 1000)), raio_m = 2000,
    camadas = c("A", "B", "C")
  )
  sitios <- sf::st_transform(
    sf::st_as_sf(pontos_unicos, coords = c("longitude", "latitude"),
                 crs = gs_epsg$wgs84),
    gs_epsg$oficial
  )
  celula_do_sitio <- vapply(sf::st_intersects(sitios, vor_utm), `[`, integer(1), 1)
  expect_match(vor$camada[celula_do_sitio[1]], "A")
  expect_match(vor$camada[celula_do_sitio[1]], "D")
  expect_match(vor$camada[celula_do_sitio[2]], "B")
  expect_match(vor$camada[celula_do_sitio[3]], "C")
})

test_that("grade hexagonal cobre o buffer e conserva celulas sem servicos", {
  resultado <- cria_resultado_espacial(
    rbind(c(-1200, -500), c(0, 0), c(700, 900), c(700, 900)),
    raio_m = 2500
  )
  grade <- gs_grade_hex(resultado, celula_m = 700)
  dominio <- gs_dominio_consulta(resultado)$dominio

  expect_s3_class(grade, "sf")
  expect_true(any(grade$n_servicos == 0))
  expect_equal(sum(grade$n_servicos), nrow(resultado))
  expect_equal(sum(as.numeric(sf::st_area(grade))),
               as.numeric(sf::st_area(dominio)), tolerance = 1)
  expect_equal(grade$densidade_por_km2,
               grade$n_servicos / grade$area_observada_km2)
  expect_true(any(grade$fracao_area_observada < 0.5))
})

test_that("dominio respeita metrica, truncamento e janela da rede", {
  resultado <- cria_resultado_espacial(
    rbind(c(0, 0), c(400, 300), c(-500, 200)), raio_m = 1000
  )
  attr(resultado, "tipo_distancia") <- "manhattan"
  dominio <- gs_dominio_consulta(resultado)
  expect_true(dominio$executado)
  expect_identical(dominio$tipo_distancia, "manhattan")
  expect_equal(as.numeric(sf::st_area(dominio$dominio)), 2e6,
               tolerance = 1)

  truncado <- resultado
  attr(truncado, "amostra_truncada") <- TRUE
  expect_false(gs_dominio_consulta(truncado)$executado)
  expect_match(gs_dominio_consulta(truncado)$mensagem, "truncado")

  rede <- resultado
  attr(rede, "tipo_distancia") <- "rede_viaria"
  expect_false(gs_dominio_consulta(rede)$executado)
  expect_match(gs_dominio_consulta(rede)$mensagem, "isócrona")

  expect_false(gs_dominio_consulta(resultado, raio_m = 2000)$executado)
  outro_ponto <- list(longitude = -46.60, latitude = -23.50)
  expect_false(gs_dominio_consulta(resultado, ponto = outro_ponto)$executado)
})

test_that("analises de oportunidade bloqueiam amostra truncada e raio nao observado", {
  resultado <- cria_resultado_espacial(
    rbind(c(100, 0), c(400, 0), c(800, 0)), raio_m = 1000
  )
  attr(resultado, "amostra_truncada") <- TRUE
  descritivas <- gs_analise_descritivas(resultado)
  expect_true(descritivas$executado)
  expect_true(descritivas$amostra_truncada)
  expect_false(gs_analise_acessibilidade(resultado)$executado)
  expect_false(gs_analise_raio_otimo(resultado)$executado)
  expect_false(gs_analise_kde(resultado, attr(resultado, "ponto"))$executado)
  expect_false(gs_analise_kde_banda(
    resultado, attr(resultado, "ponto")
  )$executado)
  expect_false(gs_analise_raios(
    resultado, attr(resultado, "ponto"), raios = 500
  )$executado)
  texto <- gs_interpretar_analise(
    list(descritivas = descritivas), resultado, raio_m = 1000
  )$descritivas
  expect_match(texto, "retidos")

  attr(resultado, "amostra_truncada") <- FALSE
  fora <- gs_analise_raios(
    resultado, attr(resultado, "ponto"), raios = c(500, 1500)
  )
  expect_false(fora$executado)
  expect_match(fora$mensagem, "raio observado")
})

test_that("interpretacao de acessibilidade com um ponto nao imprime NA", {
  resultado <- cria_resultado_espacial(matrix(c(100, 0), ncol = 2), raio_m = 1000)
  acesso <- gs_analise_acessibilidade(resultado)
  texto <- gs_interpretar_analise(
    list(acessibilidade_media = acesso), resultado, raio_m = 1000
  )$acessibilidade_media
  expect_true(acesso$executado)
  expect_false(grepl("NA", texto, fixed = TRUE))
  expect_match(texto, "não definidos")
})

test_that("janela esferica contem pontos aceitos pela mesma metrica", {
  ponto <- sf::st_sfc(sf::st_point(c(-46.63, -23.55)), crs = gs_epsg$wgs84)
  centro <- sf::st_coordinates(sf::st_transform(ponto, gs_epsg$oficial))[1, ]
  deslocamentos <- rbind(c(999, 0), c(0, 999), c(-999, 0), c(0, -999))
  pts <- sf::st_as_sf(
    data.frame(x = centro[1] + deslocamentos[, 1],
               y = centro[2] + deslocamentos[, 2]),
    coords = c("x", "y"), crs = gs_epsg$oficial
  )
  pts_wgs <- sf::st_transform(pts, gs_epsg$wgs84)
  d <- gs_distancias_s2(ponto, pts_wgs)
  resultado <- cria_resultado_espacial(deslocamentos, raio_m = 1000)
  attr(resultado, "tipo_distancia") <- "geodesica"
  dominio <- gs_dominio_consulta(resultado)$dominio
  dentro <- lengths(sf::st_covered_by(
    sf::st_transform(pts_wgs[d <= 1000, ], gs_epsg$oficial), dominio
  )) > 0
  expect_true(all(dentro))
})

test_that("Moran global usa toda a grade e Monte Carlo reproduzivel", {
  skip_if_not_installed("spdep")
  deslocamentos <- rbind(
    matrix(c(-900, -700), nrow = 8, ncol = 2, byrow = TRUE),
    matrix(c(900, 800), nrow = 4, ncol = 2, byrow = TRUE),
    c(-1500, 1000), c(1400, -1200), c(0, 0)
  )
  resultado <- cria_resultado_espacial(deslocamentos, raio_m = 2400)
  grade <- gs_grade_hex(resultado, celula_m = 650)

  m1 <- gs_analise_moran(resultado, celula_m = 650, nsim = 49, seed = 91)
  m2 <- gs_analise_moran(resultado, celula_m = 650, nsim = 49, seed = 91)
  expect_true(m1$executado)
  expect_equal(m1$n_celulas, nrow(grade))
  expect_equal(m1$n_celulas_ocupadas, sum(grade$n_servicos > 0))
  expect_gt(m1$n_celulas, m1$n_celulas_ocupadas)
  expect_identical(m1$metodo_p,
                   "Monte Carlo bilateral condicional à área")
  expect_identical(m1$moran_i, m2$moran_i)
  expect_identical(m1$valor_p, m2$valor_p)
})

test_that("Getis-Ord inclui self, usa todas as celulas e ajusta p bilateral", {
  skip_if_not_installed("spdep")
  deslocamentos <- rbind(
    matrix(c(-900, -700), nrow = 8, ncol = 2, byrow = TRUE),
    matrix(c(900, 800), nrow = 4, ncol = 2, byrow = TRUE),
    c(-1500, 1000), c(1400, -1200), c(0, 0)
  )
  resultado <- cria_resultado_espacial(deslocamentos, raio_m = 2400)
  grade <- gs_grade_hex(resultado, celula_m = 650)
  g <- gs_analise_getis_ord(resultado, celula_m = 650, nsim = 49, seed = 91)
  g2 <- gs_analise_getis_ord(resultado, celula_m = 650, nsim = 49, seed = 91)

  expect_true(g$executado)
  expect_true(g$inclui_self)
  expect_equal(nrow(g$grade), nrow(grade))
  expect_equal(g$grade$p_ajustado,
               gs_ajustar_p_finitos(g$grade$p_valor))
  finitos <- is.finite(g$grade$gi_z)
  expect_true(all(g$grade$p_valor[finitos] >= 1 / 50))
  expect_equal(g$grade$p_valor, g2$grade$p_valor)
  expect_identical(g$metodo_p,
                   "Monte Carlo bilateral condicional à área")
})

test_that("LISA usa valor centrado, lag, quatro quadrantes e BH", {
  skip_if_not_installed("spdep")
  deslocamentos <- rbind(
    matrix(c(-900, -700), nrow = 8, ncol = 2, byrow = TRUE),
    matrix(c(900, 800), nrow = 4, ncol = 2, byrow = TRUE),
    c(-1500, 1000), c(1400, -1200), c(0, 0)
  )
  resultado <- cria_resultado_espacial(deslocamentos, raio_m = 2400)
  grade <- gs_grade_hex(resultado, celula_m = 650)
  lisa <- gs_analise_lisa(resultado, celula_m = 650, nsim = 49, seed = 91)

  expect_true(lisa$executado)
  expect_equal(nrow(lisa$grade), nrow(grade))
  expect_equal(lisa$grade$valor_centrado,
               lisa$grade$densidade_por_km2 -
                 mean(lisa$grade$densidade_por_km2))
  expect_equal(lisa$grade$p_ajustado,
               gs_ajustar_p_finitos(lisa$grade$p_valor))
  expect_identical(lisa$variavel, "densidade_por_km2")
  expect_equal(
    gs_classificar_lisa(c(1, -1, 1, -1), c(1, -1, -1, 1), rep(0.01, 4)),
    c("alto-alto", "baixo-baixo", "alto-baixo", "baixo-alto")
  )
  esperado <- gs_classificar_lisa(
    lisa$grade$valor_centrado, lisa$grade$lag_espacial,
    lisa$grade$p_ajustado
  )
  expect_equal(lisa$grade$classe, esperado)
})

test_that("LISA distrital aplica quadrantes e ajuste BH", {
  skip_if_not_installed("spdep")
  distritos <- cria_distritos_teste()
  rlang::local_bindings(
    gs_baixar_distritos = function(dir, force = FALSE) distritos,
    .env = globalenv()
  )
  deslocamentos <- rbind(
    matrix(c(-900, 900), nrow = 8, ncol = 2, byrow = TRUE),
    matrix(c(0, 0), nrow = 4, ncol = 2, byrow = TRUE),
    c(-900, -900), c(0, -900), c(900, -900),
    c(-900, 0), c(900, 0), c(0, 900), c(900, 900)
  )
  resultado <- cria_resultado_espacial(deslocamentos, raio_m = 2500)
  md <- gs_analise_moran_distrital(resultado, dir = tempdir(),
                                    nsim = 39, seed = 7)

  expect_true(md$executado)
  expect_equal(md$por_distrito$p_ajustado,
               stats::p.adjust(md$por_distrito$p_valor, method = "BH"))
  expect_equal(
    md$por_distrito$classe,
    gs_classificar_lisa(md$por_distrito$valor_centrado,
                        md$por_distrito$lag_espacial,
                        md$por_distrito$p_ajustado)
  )
  expect_identical(md$variavel, "densidade_por_km2")
  expect_identical(md$metodo_p,
                   "Monte Carlo bilateral condicional à área")
})

test_that("distritos usam apenas a area observada no dominio", {
  distritos <- cria_distritos_teste()
  rlang::local_bindings(
    gs_baixar_distritos = function(dir, force = FALSE) distritos,
    .env = globalenv()
  )
  resultado <- cria_resultado_espacial(
    rbind(c(0, 0), c(500, 0), c(-500, 0)), raio_m = 1000
  )
  cruzamento <- gs_distritos_no_dominio(resultado, dir = tempdir())
  dominio <- gs_dominio_consulta(resultado)$dominio

  expect_true(cruzamento$executado)
  expect_equal(sum(as.numeric(sf::st_area(cruzamento$distritos))),
               as.numeric(sf::st_area(dominio)), tolerance = 1)
  expect_true(any(cruzamento$distritos$fracao_area_observada < 0.5))
  expect_equal(
    cruzamento$distritos$densidade_por_km2,
    cruzamento$distritos$n_servicos /
      cruzamento$distritos$area_observada_km2
  )
})

test_that("ponto em fronteira distrital recebe atribuicao unica e auditavel", {
  distritos <- cria_distritos_teste()
  rlang::local_bindings(
    gs_baixar_distritos = function(dir, force = FALSE) distritos,
    .env = globalenv()
  )
  resultado <- cria_resultado_espacial(matrix(c(450, 0), ncol = 2),
                                       raio_m = 2000)
  cruzamento <- gs_distritos_no_dominio(resultado, dir = tempdir())
  expect_true(cruzamento$executado)
  expect_equal(sum(cruzamento$distritos$n_servicos), 1)
  expect_equal(cruzamento$n_atribuicoes_ambiguas, 1)
})

test_that("NNI trata amostra pequena, duplicatas e dominio circular", {
  resultado <- cria_resultado_espacial(
    rbind(c(0, 0), c(0, 0), c(-700, 0), c(700, 0)), raio_m = 2000
  )
  n1 <- gs_analise_nni(resultado, nsim = 39, seed = 1234)
  n2 <- gs_analise_nni(resultado, nsim = 39, seed = 1234)

  expect_true(n1$executado)
  expect_equal(n1$n, 3)
  expect_equal(n1$n_deduplicados, 1)
  expect_equal(n1$area_km2,
               round(as.numeric(sf::st_area(gs_dominio_consulta(resultado)$dominio)) /
                       1e6, 2))
  expect_identical(n1$valor_p, n2$valor_p)
  expect_match(n1$metodo_p, "Monte Carlo")
  expect_true(is.character(n1$interpretacao) && nzchar(n1$interpretacao))

  unico <- resultado[1, , drop = FALSE]
  attr(unico, "ponto") <- attr(resultado, "ponto")
  attr(unico, "raio_m") <- attr(resultado, "raio_m")
  expect_false(gs_analise_nni(unico)$executado)
})

test_that("NNI e Ripley usam a janela Manhattan", {
  deslocamentos <- rbind(
    c(-600, 0), c(-300, 200), c(0, 0), c(300, -200),
    c(600, 0), c(0, 500), c(0, -500)
  )
  resultado <- cria_resultado_espacial(deslocamentos, raio_m = 1000)
  attr(resultado, "tipo_distancia") <- "manhattan"
  nni <- gs_analise_nni(resultado, nsim = 39, seed = 11)
  expect_true(nni$executado)
  expect_identical(nni$tipo_janela, "manhattan")
  expect_equal(nni$area_km2, 2, tolerance = 0.01)
  expect_match(nni$metodo_p, "janela observacional")

  skip_if_not_installed("spatstat.geom")
  skip_if_not_installed("spatstat.explore")
  ripley <- gs_analise_ripley_k(resultado, rmax_m = 400)
  expect_true(ripley$executado)
  expect_identical(ripley$tipo_janela, "manhattan")
  expect_equal(ripley$area_dominio_km2, 2, tolerance = 0.01)
})

test_that("cobertura usa buffer da consulta e preserva camadas co-localizadas", {
  resultado <- cria_resultado_espacial(
    rbind(c(0, 0), c(0, 0), c(900, 0)), raio_m = 2000,
    camadas = c("saude", "educacao", "saude")
  )
  cobertura <- gs_analise_cobertura(resultado, raio_buffer_m = 500)
  dominio <- gs_dominio_consulta(resultado)$dominio

  expect_true(cobertura$executado)
  expect_equal(cobertura$area_consulta_km2,
               round(as.numeric(sf::st_area(dominio)) / 1e6, 2))
  expect_lte(cobertura$pct_cobertura, 100)
  expect_equal(cobertura$por_camada$n[
    match(c("educacao", "saude"), cobertura$por_camada$camada)
  ], c(1, 2))
})

test_that("cobertura populacional trata NA e conserva area do buffer sem intersecao", {
  resultado <- cria_resultado_espacial(rbind(c(0, 0), c(500, 0)), raio_m = 1000)
  dominio <- gs_dominio_consulta(resultado)$dominio
  centro <- sf::st_coordinates(gs_dominio_consulta(resultado)$ponto)[1, ]
  quadrado <- function(cx, cy, lado) {
    sf::st_polygon(list(rbind(
      c(cx - lado, cy - lado), c(cx + lado, cy - lado),
      c(cx + lado, cy + lado), c(cx - lado, cy + lado),
      c(cx - lado, cy - lado)
    )))
  }
  pop_na <- sf::st_sf(
    populacao = NA_real_,
    geometry = sf::st_sfc(quadrado(centro[1], centro[2], 1500),
                          crs = gs_epsg$oficial)
  )
  cob_na <- gs_analise_cobertura_populacional(resultado, pop_layer = pop_na)
  expect_true(cob_na$executado)
  expect_equal(cob_na$populacao_atendida, 0)
  expect_equal(cob_na$n_populacao_na, 1)
  expect_equal(cob_na$area_km2,
               round(as.numeric(sf::st_area(dominio)) / 1e6, 2))

  pop_longe <- sf::st_sf(
    populacao = 100,
    geometry = sf::st_sfc(quadrado(centro[1] + 5000, centro[2], 100),
                          crs = gs_epsg$oficial)
  )
  cob_vazia <- gs_analise_cobertura_populacional(resultado,
                                                  pop_layer = pop_longe)
  expect_true(cob_vazia$executado)
  expect_equal(cob_vazia$populacao_atendida, 0)
  expect_equal(cob_vazia$area_km2,
               round(as.numeric(sf::st_area(dominio)) / 1e6, 2))
})

test_that("descritivas incluem medidas robustas, quantis, IC e plots", {
  resultado <- data.frame(
    camada = c("A", "A", "B", "B", "B"),
    tipo_servico = "teste",
    distancia_m = c(100, 200, 300, 1200, NA_real_)
  )
  d <- gs_analise_descritivas(resultado)
  a <- gs_analise_acessibilidade(resultado)
  medidas <- c("p05", "p25", "mediana", "p75", "p95", "mad", "iqr",
               "ic95_media_inf", "ic95_media_sup")

  expect_true(d$executado)
  expect_true(all(medidas %in% names(d$estatisticas_distancia)))
  expect_true(inherits(d$histograma, "ggplot"))
  expect_true(inherits(d$boxplot, "ggplot"))
  expect_true(a$executado)
  expect_true(all(medidas %in% names(a$geral)))
  expect_true(inherits(a$grafico_ecdf, "ggplot"))
})

test_that("casos degenerados retornam executado falso e interpretador usa distrito correto", {
  sem_dominio <- data.frame(longitude = -46.63, latitude = -23.55)
  expect_false(gs_analise_voronoi(sem_dominio)$executado)
  expect_false(gs_analise_descritivas(
    data.frame(camada = "A", distancia_m = NA_real_)
  )$executado)

  resultado <- cria_resultado_espacial(rbind(c(0, 0), c(500, 0)), raio_m = 1000)
  repetidos <- cria_resultado_espacial(rbind(c(0, 0), c(0, 0)), raio_m = 1000)
  expect_false(gs_analise_voronoi(repetidos)$executado)
  expect_false(gs_analise_cobertura(resultado, raio_buffer_m = 0)$executado)
  expect_false(gs_analise_cobertura_populacional(
    resultado, pop_layer = data.frame(populacao = 1)
  )$executado)
  expect_error(gs_analise_servicos(
    data.frame(camada = "A", distancia_m = 100), tipo = "descritivas"
  ), "atributo 'ponto'")

  if (requireNamespace("spdep", quietly = TRUE)) {
    vazio <- resultado[0, , drop = FALSE]
    attr(vazio, "ponto") <- attr(resultado, "ponto")
    attr(vazio, "raio_m") <- attr(resultado, "raio_m")
    expect_false(gs_analise_moran(vazio, celula_m = 400, nsim = 19)$executado)
    expect_false(gs_analise_getis_ord(vazio, celula_m = 400)$executado)
    expect_false(gs_analise_lisa(vazio, celula_m = 400)$executado)
  }

  analises <- list(por_distrito = list(
    executado = TRUE,
    por_distrito = data.frame(
      nm_distrito_municipal = c("Sé", "Mooca"),
      n_servicos = c(3, 1), densidade_por_km2 = c(2, 1)
    )
  ))
  interpretacao <- gs_interpretar_analise(
    analises, data.frame(distancia_m = c(100, 200)), raio_m = 1000
  )
  expect_match(interpretacao$por_distrito, "Sé", fixed = TRUE)
})
