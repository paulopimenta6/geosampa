# ============================================================
# GeoSampa — Serviços próximos e tipos de distância
# ------------------------------------------------------------
# Dado um CEP (ou coordenadas), calcula a distância até cada
# equipamento público das camadas escolhidas e devolve os que
# estão dentro de um raio (ou os N mais próximos por camada).
# Suporta diferentes métricas de distância.
# ============================================================

# --- Documenta os tipos de distância disponíveis ----------------------------
gs_tipos_distancia <- function() {
  data.frame(
    tipo = c("geodesica", "euclidiana", "haversine", "manhattan", "rede_viaria"),
    descricao = c(
      "Geodésica (elipsoidal) via sf::st_distance em CRS geográfico",
      "Euclidiana em metros na projeção oficial UTM/SIRGAS2000 (EPSG:31983)",
      "Haversine sobre a esfera (aproximação leve em WGS84)",
      "Manhattan (|dx| + |dy|) em metros projetados",
      "Rota real de carro via OSRM (requer pacote osrm; servidor configurável)"
    ),
    quando_usar = c(
      "Padrão recomendado: precisa e suficiente para raios em geral",
      "Rápida e boa até ~20 km; ideal para buffers em metros",
      "Alternativa leve quando não se quer transformar o CRS",
      "Métrica de 'caminhabilidade' em quadrículas urbanas",
      "Caminho real de carro/pé (mais fiel, porém mais lento e limitado)"
    ),
    stringsAsFactors = FALSE
  )
}

# --- Calcula distâncias do ponto de origem até cada ponto -------------------
# `ponto_sf` e `pontos_sf` são sf/sfc em EPSG:4326. Devolve vetor em metros.
# Para `rede_viaria` usa o OSRM (pacote `osrm`, instalado sob demanda); o
# servidor pode ser configurado com options(osrm.server = ...) ou via
# options(gs.osrm_server = ...).
gs_calcular_distancias <- function(ponto_sf, pontos_sf,
                                   tipo_distancia = c("geodesica", "euclidiana",
                                                      "haversine", "manhattan",
                                                      "rede_viaria")) {
  tipo_distancia <- match.arg(tipo_distancia)
  switch(
    tipo_distancia,
    geodesica = as.numeric(sf::st_distance(ponto_sf, pontos_sf)),
    euclidiana = {
      pp <- sf::st_transform(ponto_sf, gs_epsg$oficial)
      qq <- sf::st_transform(pontos_sf, gs_epsg$oficial)
      as.numeric(sf::st_distance(pp, qq))
    },
    haversine = {
      raio_t <- 6371000
      p <- sf::st_coordinates(ponto_sf)
      q <- sf::st_coordinates(pontos_sf)
      dlat <- (q[, "Y"] - p[1, "Y"]) * pi / 180
      dlon <- (q[, "X"] - p[1, "X"]) * pi / 180
      a <- sin(dlat / 2)^2 +
        cos(p[1, "Y"] * pi / 180) * cos(q[, "Y"] * pi / 180) * sin(dlon / 2)^2
      raio_t * 2 * atan2(sqrt(a), sqrt(1 - a))
    },
    manhattan = {
      pp <- sf::st_transform(ponto_sf, gs_epsg$oficial)
      qq <- sf::st_transform(pontos_sf, gs_epsg$oficial)
      p <- sf::st_coordinates(pp)
      q <- sf::st_coordinates(qq)
      abs(q[, "X"] - p[1, "X"]) + abs(q[, "Y"] - p[1, "Y"])
    },
    rede_viaria = {
      if (!requireNamespace("osrm", quietly = TRUE)) {
        stop("Pacote 'osrm' não instalado para distância por rede viária. ",
             "Instale com: install.packages('osrm')")
      }
      options(osrm.server = gs_osrm_server(),
              osrm.profile = gs_osrm_profile())
      p0 <- sf::st_coordinates(ponto_sf)[1, ]
      qq <- sf::st_coordinates(pontos_sf)
      if (nrow(qq) > 200) {
        message("  rede viária: calculando distâncias para ", nrow(qq),
                " pontos via OSRM (pode demorar)...")
      }
      origem <- gs_osrm_input("origem", p0[1], p0[2])
      destinos <- gs_osrm_input(as.character(seq_len(nrow(qq))),
                                qq[, 1], qq[, 2])
      tab <- tryCatch(
        osrm::osrmTable(src = origem, dst = destinos, measure = "distance"),
        error = function(e) NULL
      )
      if (is.null(tab)) {
        stop("Falha ao consultar o servidor OSRM (", gs_osrm_server(),
             "). Configure outro com options(osrm.server = 'http://...') ",
             "ou use um tipo de distância em linha reta.")
      }
      gs_osrm_dist_m(tab$distances[1, ])
    }
  )
}

# --- Resolve o argumento `camadas` para nomes reais presentes em data/ -------
# Aceita:
#   - NULL / "todos" / "all" / "todas" -> todas as camadas locais;
#   - nomes exatos de camadas (com ou sem o prefixo "geoportal:");
#   - temas (ex.: "saude") -> expande para todas as camadas daquele tema;
#   - pedaços do nome (ex.: "ubs") -> casa por substring;
#   - data.frame vindo de gs_catalogo_equipamentos() (usa a coluna `camada`).
# Devolve list(resolvidas, nao_encontradas).
gs_resolver_camadas <- function(camadas, dir = gs_pasta_dados()) {
  locais <- gs_camadas_local(dir)
  if (length(locais) == 0) {
    stop("Nenhum CSV em ", dir,
         ". Baixe as camadas com gs_baixar_todos_equipamentos().")
  }
  if (is.null(camadas)) {
    return(list(resolvidas = locais, nao_encontradas = character(0)))
  }
  if (is.data.frame(camadas)) camadas <- camadas$camada
  camadas <- as.character(camadas)
  if (length(camadas) == 1 && tolower(camadas) %in% c("todos", "all", "todas")) {
    return(list(resolvidas = locais, nao_encontradas = character(0)))
  }

  resolvidas <- character(0)
  nao_encontradas <- character(0)
  temas_locais <- unique(gs_tema_camada(locais))

  for (v in unique(camadas)) {
    v <- sub("^geoportal:", "", v)
    if (v %in% locais) {
      resolvidas <- c(resolvidas, v)
    } else if (tolower(v) %in% temas_locais) {
      resolvidas <- c(resolvidas, locais[gs_tema_camada(locais) == tolower(v)])
    } else {
      parcial <- locais[grepl(v, locais, ignore.case = TRUE)]
      if (length(parcial) > 0) {
        resolvidas <- c(resolvidas, parcial)
      } else {
        nao_encontradas <- c(nao_encontradas, v)
      }
    }
  }
  list(resolvidas = unique(resolvidas), nao_encontradas = unique(nao_encontradas))
}

# --- Busca serviços próximos a um CEP ou coordenada --------------------------
# Parâmetros:
#   cep            CEP (ex.: "03175001" ou "03175-001") — alternativa a coordenadas.
#   coordenadas    vetor c(latitude, longitude) — alternativa a cep.
#   camadas        vetor de nomes de camadas (base sem prefixo, ex.:
#                  "equipamento_saude_ubs_posto_centro"). NULL usa TODAS as locais.
#   raio_m         raio de busca em metros.
#   n_por_camada   limite de resultados por camada (NULL = todos dentro do raio).
#   tipo_distancia métrica de distância (ver gs_tipos_distancia()).
# Devolve data.frame ordenado (mais próximo primeiro), com os atributos
# `ponto`, `tipo_distancia` e `raio_m` usados pelos mapas e análises.
gs_servicos_proximos <- function(cep = NULL, coordenadas = NULL, camadas = NULL,
                                 raio_m = gs_raio_padrao_m,
                                 n_por_camada = NULL,
                                 tipo_distancia = c("geodesica", "euclidiana",
                                                    "haversine", "manhattan",
                                                    "rede_viaria"),
                                 dir = gs_pasta_dados()) {
  tipo_distancia <- match.arg(tipo_distancia)
  ponto <- gs_resolver_ponto(cep, coordenadas)

  resolvidas <- gs_resolver_camadas(camadas, dir)
  if (length(resolvidas$nao_encontradas) > 0) {
    message("⚠️ Ignorando valores de `camadas` não encontrados em data/: ",
            paste(resolvidas$nao_encontradas, collapse = ", "),
            ". Veja as opções com gs_listar_servicos().")
  }
  camadas <- resolvidas$resolvidas
  if (length(camadas) == 0) {
    stop("Nenhuma camada válida em data/ para os valores informados. ",
         "Use gs_listar_servicos() para ver o que está disponível.")
  }

  res <- lapply(camadas, function(cam) {
    arq <- file.path(dir, paste0(cam, ".csv"))
    tab <- tryCatch(readr::read_csv(arq, show_col_types = FALSE),
                    error = function(e) NULL)
    if (is.null(tab)) return(NULL)
    if (!all(c("latitude", "longitude") %in% names(tab))) return(NULL)
    tab$latitude  <- as.numeric(tab$latitude)
    tab$longitude <- as.numeric(tab$longitude)
    tab <- tab[!is.na(tab$latitude) & !is.na(tab$longitude), , drop = FALSE]
    if (nrow(tab) == 0) return(NULL)

    pts <- sf::st_as_sf(tab, coords = c("longitude", "latitude"),
                        crs = gs_epsg$wgs84)
    if (tipo_distancia == "rede_viaria") {
      # Pré-filtro por linha reta: a distância por rede é sempre >= a geodésica,
      # então pontos além de `raio_m` em linha reta nunca entrariam no raio.
      # Evita requests gigantes ao servidor OSRM.
      geo <- as.numeric(sf::st_distance(ponto$sf, pts))
      manter <- geo <= raio_m
      pts <- pts[manter, , drop = FALSE]
      tab <- tab[manter, , drop = FALSE]
      if (nrow(pts) == 0) return(NULL)
    }
    dists <- gs_calcular_distancias(ponto$sf, pts, tipo_distancia)
    tab$distancia_m <- round(dists, 1)
    tab$camada      <- cam
    tab$nome        <- if ("nm_equipamento" %in% names(tab)) tab$nm_equipamento else NA_character_
    tab$tipo_servico <- if ("nm_tipo_equipamento" %in% names(tab)) tab$nm_tipo_equipamento else NA_character_
    tab$endereco    <- if ("tx_endereco_equipamento" %in% names(tab)) tab$tx_endereco_equipamento else NA_character_
    tab$bairro      <- if ("nm_bairro_equipamento" %in% names(tab)) tab$nm_bairro_equipamento else NA_character_

    tab <- tab[order(tab$distancia_m), , drop = FALSE]
    cols <- c("camada", "nome", "tipo_servico", "endereco", "bairro",
              "distancia_m", "latitude", "longitude")
    tab[, intersect(cols, names(tab)), drop = FALSE]
  })

  out <- do.call(rbind, Filter(Negate(is.null), res))
  if (is.null(out) || nrow(out) == 0) {
    stop("Nenhum serviço encontrado nas camadas informadas.")
  }

  sel <- out[out$distancia_m <= raio_m, , drop = FALSE]
  if (!is.null(n_por_camada)) {
    sel <- do.call(rbind, lapply(split(sel, sel$camada), function(d) {
      if (nrow(d) > n_por_camada) d[seq_len(n_por_camada), , drop = FALSE] else d
    }))
    rownames(sel) <- NULL
  }

  # Ordenação GLOBAL por distância (mais próximos primeiro), independente
  # da ordem alfabética das camadas.
  sel <- sel[order(sel$distancia_m), , drop = FALSE]
  rownames(sel) <- NULL

  cols <- c("camada", "nome", "tipo_servico", "endereco", "bairro",
            "distancia_m", "latitude", "longitude")
  sel <- sel[, intersect(cols, names(sel)), drop = FALSE]
  rownames(sel) <- NULL

  attr(sel, "ponto")          <- ponto
  attr(sel, "tipo_distancia") <- tipo_distancia
  attr(sel, "raio_m")         <- raio_m
  sel
}