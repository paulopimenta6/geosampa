# ============================================================
# GeoSampa — Configuração central
# ------------------------------------------------------------
# Aqui moram os "endereços" dos serviços web do GeoSampa, as
# projeções cartográficas e as pastas padrão do projeto.
# ============================================================

# --- URLs dos serviços web do GeoSampa -------------------------------------
# WFS  = "Web Feature Service": entrega os DADOS VETORIAIS (o "baú").
# WMS  = "Web Map Service": entrega IMAGENS do mapa (o "espelho").
# GeoNetwork = catálogo de metadados (os "documentos de identidade" das camadas).
gs_urls <- list(
  wfs         = "https://wfs.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wfs",
  wms         = "https://wms.geosampa.prefeitura.sp.gov.br/geoserver/geoportal/wms",
  metadados   = "https://metadados.geosampa.prefeitura.sp.gov.br/geonetwork/srv/por",
  geonet_api  = "https://metadados.geosampa.prefeitura.sp.gov.br/geonetwork/srv/api",
  viacep      = "https://viacep.com.br/ws/{cep}/json/",
  nominatim   = "https://nominatim.openstreetmap.org/search"
)

# --- Sistemas de referência cartográfica ----------------------------------
# EPSG 31983 = SIRGAS2000 / UTM 23S  -> referência oficial do GeoSampa.
# EPSG 4326  = WGS84 (latitude/longitude) -> usado no CSV para ficar "legível".
gs_epsg <- list(
  oficial    = 31983,  # SIRGAS2000 / UTM 23S
  geografica = 4674,   # SIRGAS2000 geográfica
  wgs84      = 4326    # WGS84 (graus)
)

# --- Página padrão do WFS (quantas feições buscar por requisição) ----------
gs_tamanho_pagina <- 1000

# --- Configurações HTTP ------------------------------------------------------
# Podem ser reduzidas em testes ou aumentadas para conexões mais lentas.
gs_http_timeout_s <- function() {
  gs_validar_numero_escalar(
    getOption("gs.http_timeout_s", 120), "options(gs.http_timeout_s)",
    minimo = 0, minimo_inclusivo = FALSE
  )
}

gs_http_tentativas <- function() {
  as.integer(gs_validar_numero_escalar(
    getOption("gs.http_tentativas", 3L), "options(gs.http_tentativas)",
    minimo = 1, inteiro = TRUE
  ))
}

# Requisições idempotentes usam retry para falhas transitórias e timeout finito.
gs_http_get <- function(url, query = NULL, timeout_s = gs_http_timeout_s(), ...) {
  httr::RETRY(
    "GET", url,
    query = query,
    ...,
    httr::timeout(timeout_s),
    times = gs_http_tentativas(),
    pause_base = 0.5,
    pause_cap = 4,
    quiet = TRUE
  )
}

# --- Configurações do módulo de CEP -----------------------------------------
# Tolerância padrão (metros) para verificar se uma coordenada confere com um CEP.
gs_tolerancia_cep_m <- 300
# Pausa (segundos) entre consultas ao Nominatim, respeitando a política de uso
# (máximo ~1 requisição por segundo).
gs_pausa_nominatim_s <- 1
# Raio padrão (metros) para buscar serviços próximos.
gs_raio_padrao_m <- 2000
# Raio padrão (metros) usado nas análises de cobertura por buffer.
gs_raio_buffer_m <- 1000
# Tamanho padrão (metros) das células hexagonais das análises LISA/Getis-Ord.
gs_celula_hex_m <- 1000
# Servidor OSRM para distância por rede viária. Configurável por
# options(gs.osrm_server = 'http://...') ou options(osrm.server = ...).
gs_osrm_server <- function() {
  getOption("gs.osrm_server",
            getOption("osrm.server",
                      "https://router.project-osrm.org/"))
}
# Perfil OSRM (rota). Configurável por options(gs.osrm_profile = 'driving')
# ou options(osrm.profile = 'driving').
gs_osrm_profile <- function() {
  getOption("gs.osrm_profile",
            getOption("osrm.profile",
                      "driving"))
}
# Tabela de origem/destino para o pacote osrm, compatível com a API antiga
# (< 4.0: colunas id/lon/lat) e nova (>= 4.0: apenas lon/lat, id nos row.names).
gs_osrm_input <- function(ids, lon, lat) {
  if (utils::packageVersion("osrm") >= "4.0.0") {
    data.frame(lon = lon, lat = lat, row.names = ids, stringsAsFactors = FALSE)
  } else {
    data.frame(id = ids, lon = lon, lat = lat, stringsAsFactors = FALSE)
  }
}
# Converte distâncias do osrmTable para metros: < 4.0 devolve km; >= 4.0 devolve m.
gs_osrm_dist_m <- function(distancias) {
  if (utils::packageVersion("osrm") >= "4.0.0") {
    as.numeric(distancias)
  } else {
    as.numeric(distancias) * 1000
  }
}
# Nomes das camadas administrativas (baixadas sob demanda do WFS do GeoSampa).
gs_camadas_apoio <- list(
  distritos = "distrito_municipal",
  setores   = "setor_censitario_2022"
)

# --- Localização da raiz do projeto ----------------------------------------
# Sobe de diretório em diretório até encontrar a pasta que contém R/ e scripts/.
# Também respeita a opção gs.raiz, que pode ser definida pelo usuário.
gs_raiz <- function() {
  raiz <- getOption("gs.raiz")
  if (!is.null(raiz)) return(raiz)
  dir <- getwd()
  repeat {
    if (dir.exists(file.path(dir, "R")) && dir.exists(file.path(dir, "scripts"))) {
      return(dir)
    }
    pai <- dirname(dir)
    if (identical(pai, dir)) {
      stop("Não consegui achar a raiz do projeto GeoSampa (procuro por R/ e scripts/). ",
           "Defina com options(gs.raiz = 'caminho') ou rode a partir da pasta do projeto.")
    }
    dir <- pai
  }
}

# --- Pasta de dados ----------------------------------------------------------
# O caminho sem efeitos colaterais é usado por funções que apenas leem dados.
gs_caminho_dados <- function() {
  file.path(gs_raiz(), "data")
}

# Mantém o comportamento histórico para chamadas diretas e rotinas de escrita.
gs_pasta_dados <- function(criar = TRUE) {
  if (length(criar) != 1 || is.na(criar) || !is.logical(criar)) {
    stop("`criar` deve ser TRUE ou FALSE.")
  }
  dir <- gs_caminho_dados()
  if (criar) dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

# --- Funções auxiliares internas -------------------------------------------
# Valida números usados nas interfaces escalares do projeto.
gs_validar_numero_escalar <- function(x, nome, minimo = -Inf, maximo = Inf,
                                      minimo_inclusivo = TRUE,
                                      maximo_inclusivo = TRUE,
                                      inteiro = FALSE) {
  valido <- is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x)
  if (valido && inteiro) valido <- x == floor(x)
  if (valido) {
    valido <- if (minimo_inclusivo) x >= minimo else x > minimo
  }
  if (valido) {
    valido <- if (maximo_inclusivo) x <= maximo else x < maximo
  }
  if (!valido) {
    tipo <- if (inteiro) "um inteiro" else "um número"
    stop("`", nome, "` deve ser ", tipo, " escalar, finito e dentro do intervalo permitido.")
  }
  as.numeric(x)
}

gs_validar_coordenadas <- function(coordenadas) {
  if (!is.numeric(coordenadas) || length(coordenadas) != 2 ||
      anyNA(coordenadas) || any(!is.finite(coordenadas))) {
    stop("`coordenadas` deve ser um vetor numérico c(latitude, longitude), sem NA.")
  }
  latitude <- gs_validar_numero_escalar(coordenadas[1], "latitude",
                                        minimo = -90, maximo = 90)
  longitude <- gs_validar_numero_escalar(coordenadas[2], "longitude",
                                         minimo = -180, maximo = 180)
  c(latitude, longitude)
}

gs_slug <- function(x, padrao = "item", max_chars = 64) {
  x <- if (length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- padrao
  substr(x, 1, max_chars)
}

# Escreve no mesmo diretório e só substitui o destino após concluir a escrita.
gs_gravar_atomico <- function(caminho, escrever) {
  pasta <- dirname(caminho)
  if (!dir.exists(pasta)) {
    stop("Diretório de destino inexistente: ", pasta)
  }
  extensao <- tools::file_ext(caminho)
  temporario <- tempfile(
    pattern = paste0(".", basename(caminho), "-"),
    tmpdir = pasta,
    fileext = if (nzchar(extensao)) paste0(".", extensao) else ""
  )
  concluido <- FALSE
  on.exit({
    if (!concluido && file.exists(temporario)) unlink(temporario)
  }, add = TRUE)

  escrever(temporario)
  if (!file.exists(temporario)) {
    stop("A escrita temporária não produziu o arquivo esperado: ", caminho)
  }
  if (!file.rename(temporario, caminho)) {
    stop("Não foi possível substituir atomicamente o arquivo: ", caminho)
  }
  concluido <- TRUE
  caminho
}

gs_hash_curto <- function(x) {
  codigos <- utf8ToInt(enc2utf8(as.character(x)[1]))
  hash <- 0
  for (codigo in codigos) hash <- (hash * 131 + codigo) %% 2147483647
  sprintf("%08x", as.integer(hash))
}

# Um diretório de bloqueio por camada impede que leitores do projeto observem
# o intervalo entre os renames de um par CSV/GeoJSON.
gs_lock_transacao <- function(caminho) {
  base <- tools::file_path_sans_ext(basename(caminho))
  file.path(dirname(caminho), paste0(".", base, ".gs-lock"))
}

gs_lock_diretorio <- function(dir) {
  file.path(dir, ".gs-diretorio.gs-lock")
}

gs_token_lock <- function() {
  token <- getOption("gs.token_lock")
  prefixo <- paste0(Sys.getpid(), "-")
  if (is.null(token) || !startsWith(token, prefixo)) {
    token <- paste(Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%OS6"),
                   proc.time()[["elapsed"]], sep = "-")
    options(gs.token_lock = token)
  }
  token
}

gs_adquirir_lock <- function(lock, timeout_s = getOption(
                               "gs.transacao_timeout_s", 5
                             )) {
  inicio <- proc.time()[["elapsed"]]
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) {
      concluido <- FALSE
      on.exit({
        if (!concluido) unlink(lock, recursive = TRUE, force = TRUE)
      }, add = TRUE)
      writeLines(gs_token_lock(), file.path(lock, "owner"), useBytes = TRUE)
      concluido <- TRUE
      return(TRUE)
    }
    owner <- file.path(lock, "owner")
    if (file.exists(owner) && identical(
      tryCatch(readLines(owner, warn = FALSE, n = 1), error = function(e) ""),
      gs_token_lock()
    )) {
      return(FALSE)
    }
    if (proc.time()[["elapsed"]] - inicio >= timeout_s) {
      stop(
        "A fonte está em atualização ou possui uma transação interrompida: ",
        basename(lock), ". Conclua a aquisição antes da leitura."
      )
    }
    Sys.sleep(0.05)
  }
}

gs_com_lock <- function(lock, expr) {
  adquirido <- gs_adquirir_lock(lock)
  if (adquirido) {
    on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  }
  force(expr)
}

gs_com_lock_arquivo <- function(caminho, expr) {
  gs_com_lock(gs_lock_transacao(caminho), expr)
}

gs_listar_arquivos_consistente <- function(dir, ...) {
  if (!dir.exists(dir)) return(character(0))
  gs_com_lock(gs_lock_diretorio(dir), list.files(dir, ...))
}

# `NA` em `origens` representa remoção transacional do destino. Arquivos novos
# devem estar no mesmo filesystem dos destinos para que `file.rename` seja
# atômico por arquivo.
gs_promover_conjunto_atomico <- function(origens, destinos) {
  if (length(origens) != length(destinos) || length(origens) == 0 ||
      anyDuplicated(destinos)) {
    stop("Conjunto de arquivos inválido para promoção atômica.")
  }
  remover <- is.na(origens) | !nzchar(origens)
  if (any(!remover & !file.exists(origens))) {
    stop("Conjunto de arquivos inválido para promoção atômica.")
  }
  for (pasta in unique(dirname(destinos))) {
    dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
  }
  locks <- c(
    vapply(sort(unique(dirname(destinos))), gs_lock_diretorio, character(1)),
    sort(unique(vapply(destinos, gs_lock_transacao, character(1))))
  )
  locks_criados <- character(0)
  backups <- paste0(
    destinos, ".backup-", Sys.getpid(), "-", seq_along(destinos)
  )
  havia <- rep(FALSE, length(destinos))
  movidos <- rep(FALSE, length(destinos))
  concluido <- FALSE
  on.exit({
    if (!concluido) {
      unlink(destinos[movidos], force = TRUE)
      for (i in which(havia)) {
        if (file.exists(backups[i])) file.rename(backups[i], destinos[i])
      }
    }
    unlink(backups[file.exists(backups)], force = TRUE)
    unlink(locks_criados, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  for (lock in locks) {
    resultado_lock <- tryCatch({
      gs_adquirir_lock(lock)
    }, error = function(e) e)
    if (inherits(resultado_lock, "condition")) {
      stop(conditionMessage(resultado_lock))
    }
    if (isTRUE(resultado_lock)) locks_criados <- c(locks_criados, lock)
  }
  havia <- file.exists(destinos)

  for (i in which(havia)) {
    if (!file.rename(destinos[i], backups[i])) {
      stop("Não foi possível preparar a substituição de: ", destinos[i])
    }
  }
  for (i in which(!remover)) {
    if (!file.rename(origens[i], destinos[i])) {
      stop("Não foi possível promover o arquivo: ", destinos[i])
    }
    movidos[i] <- TRUE
  }
  concluido <- TRUE
  unlink(backups[file.exists(backups)], force = TRUE)
  invisible(destinos)
}

# Leitores compartilhados validam problemas de parsing em vez de ocultá-los.
gs_validar_csv_lido <- function(tab, arquivo) {
  problemas <- readr::problems(tab)
  if (nrow(problemas) > 0) {
    stop("Arquivo CSV corrompido ou malformado: ", arquivo,
         " (primeiro problema na linha ", problemas$row[1], ").")
  }
  tab
}

gs_ler_cabecalho_csv <- function(arquivo) {
  tab <- gs_com_lock_arquivo(arquivo, suppressWarnings(readr::read_csv(
    arquivo, n_max = 0, show_col_types = FALSE
  )))
  gs_validar_csv_lido(tab, arquivo)
}

gs_ler_csv_verificado <- function(arquivo, ...) {
  tab <- gs_com_lock_arquivo(arquivo, suppressWarnings(readr::read_csv(
    arquivo, ..., show_col_types = FALSE
  )))
  gs_validar_csv_lido(tab, arquivo)
}

# Remove o namespace usado pelo GeoServer nas respostas do catálogo.
gs_nome_base <- function(camada) {
  sub("^geoportal:", "", as.character(camada))
}

# Garante que o vetor de camadas comece com o prefixo "geoportal:".
gs_nome_completo <- function(camada) {
  paste0("geoportal:", gs_nome_base(camada))
}
