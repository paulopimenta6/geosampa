# ============================================================
# GeoSampa — Metadados (GeoNetwork)
# ------------------------------------------------------------
# Os "documentos de identidade" de cada camada: quem produziu,
# do que se trata, desde quando existe. Estas funções consultam
# o Catálogo de Metadados Geográficos do GeoSampa.
# ============================================================

# --- Busca registros de metadados por palavra-chave -------------------------
# Ex.: gs_metadados("saude") -> data.frame com uuid, categoria e data.
gs_metadados <- function(termo, de = 1, ate = 20) {
  resp <- httr::GET(paste0(gs_urls$metadados, "/q"),
                    query = list(any = termo, from = de, to = ate),
                    httr::timeout(60))
  httr::stop_for_status(resp)
  xml <- xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))

  # O catálogo repete a declaração de namespace em cada registro; usar
  # local-name() evita dor de cabeça com prefixos duplicados.
  info <- xml2::xml_find_all(xml, ".//*[local-name()='info']")
  if (length(info) == 0) {
    message("Nenhum registro encontrado para '", termo, "'.")
    return(data.frame())
  }

  texto <- function(tag) {
    n <- xml2::xml_find_first(info, paste0("./*[local-name()='", tag, "']"))
    if (inherits(n, "xml_missing")) NA_character_ else xml2::xml_text(n)
  }

  total <- xml2::xml_attr(
    xml2::xml_find_first(xml, "./*[local-name()='summary']"), "count"
  )

  out <- data.frame(
    uuid         = texto("uuid"),
    id           = as.integer(texto("id")),
    categoria    = texto("category"),
    data_criacao = texto("createDate"),
    stringsAsFactors = FALSE
  )
  attr(out, "total") <- if (is.na(total)) nrow(out) else as.integer(total)
  out
}

# --- Detalhes de um registro (título, resumo, órgão responsável) ------------
# `uuid` pode vir de gs_metadados(). Ex.: gs_metadado_registro("12f1...")
gs_metadado_registro <- function(uuid) {
  url <- paste0(gs_urls$geonet_api, "/records/", uuid)
  resp <- httr::GET(url, httr::add_headers(Accept = "application/xml"),
                    httr::timeout(60))
  httr::stop_for_status(resp)
  xml <- xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))

  extrair <- function(caminho) {
    n <- xml2::xml_find_first(xml, caminho)
    if (inherits(n, "xml_missing")) NA_character_ else xml2::xml_text(n)
  }

  # Caminhos ISO19139 expressos com local-name() para não depender de prefixo.
  base <- ".//*[local-name()='identificationInfo']/*[local-name()='MD_DataIdentification']"
  list(
    uuid   = uuid,
    titulo = extrair(paste0(base, "/*[local-name()='citation']/*[local-name()='CI_Citation']",
                            "/*[local-name()='title']/*[local-name()='CharacterString']")),
    resumo = extrair(paste0(base, "/*[local-name()='abstract']/*[local-name()='CharacterString']")),
    orgao  = extrair(paste0(base, "/*[local-name()='pointOfContact']/*[local-name()='CI_ResponsibleParty']",
                            "/*[local-name()='organisationName']/*[local-name()='CharacterString']"))
  )
}