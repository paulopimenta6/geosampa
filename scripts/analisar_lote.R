#!/usr/bin/env Rscript

# Uso:
# Rscript scripts/analisar_lote.R --ceps 05508090,05586001 --camadas saude --raio 3000
# Rscript scripts/analisar_lote.R --coords "-23.55,-46.63;-23.57,-46.65"

arquivo <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
arquivo <- if (length(arquivo) > 0) sub("^--file=", "", arquivo[1]) else
  file.path(getwd(), "scripts", "analisar_lote.R")
raiz <- dirname(dirname(normalizePath(arquivo, mustWork = FALSE)))
source(file.path(raiz, "scripts", "carregar_funcoes.R"))

args <- commandArgs(trailingOnly = TRUE)
consumidos <- rep(FALSE, length(args))
valor_arg <- function(nome, padrao = NULL) {
  i <- which(args == nome)
  if (length(i) == 0) return(padrao)
  if (length(i) > 1 || i == length(args)) stop("Argumento inválido ou repetido: ", nome)
  consumidos[c(i, i + 1L)] <<- TRUE
  args[i + 1L]
}

ceps_txt <- valor_arg("--ceps")
coords_txt <- valor_arg("--coords")
camadas_txt <- valor_arg("--camadas")
ids_txt <- valor_arg("--ids")
raio_txt <- valor_arg("--raio", as.character(gs_raio_padrao_m))
saida <- valor_arg("--saida", file.path(raiz, "saidas"))
nome <- valor_arg("--nome")
formato <- valor_arg("--formato", "md")
if (any(!consumidos)) {
  stop("Argumento desconhecido ou posicional: ",
       paste(args[!consumidos], collapse = " "))
}

if (is.null(ceps_txt) && is.null(coords_txt)) {
  stop("Informe `--ceps CEP1,CEP2` e/ou `--coords lat,lon;lat,lon`.")
}

ceps <- if (is.null(ceps_txt)) NULL else trimws(strsplit(ceps_txt, ",", fixed = TRUE)[[1]])
camadas <- if (is.null(camadas_txt)) NULL else
  trimws(strsplit(camadas_txt, ",", fixed = TRUE)[[1]])
ids <- if (is.null(ids_txt)) NULL else trimws(strsplit(ids_txt, ",", fixed = TRUE)[[1]])
raio <- suppressWarnings(as.numeric(raio_txt))

coordenadas <- NULL
if (!is.null(coords_txt)) {
  pares <- strsplit(coords_txt, ";", fixed = TRUE)[[1]]
  valores <- lapply(pares, function(par) {
    x <- suppressWarnings(as.numeric(trimws(strsplit(par, ",", fixed = TRUE)[[1]])))
    if (length(x) != 2 || any(!is.finite(x))) {
      stop("Coordenada inválida: ", par, ". Use latitude,longitude.")
    }
    x
  })
  coordenadas <- as.data.frame(do.call(rbind, valores))
  names(coordenadas) <- c("latitude", "longitude")
}

lote <- gs_analisar_locais(
  cep = ceps, coordenadas = coordenadas, ids = ids, camadas = camadas,
  raio_m = raio, dir_saida = saida, nome_execucao = nome,
  formato_relatorio = formato
)
print(lote)
