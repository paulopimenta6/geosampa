#!/usr/bin/env Rscript
# ============================================================
# GeoSampa — Atalho para carregar as funções do projeto
# ------------------------------------------------------------
# Uso no R:
#   source("scripts/carregar_funcoes.R")
#
# Uso no terminal:
#   Rscript scripts/carregar_funcoes.R
#
# O que ele faz:
#   1. Acha a raiz do projeto (a pasta que tem R/ e scripts/).
#   2. Confere se os pacotes necessários estão instalados.
#   3. Carrega TODAS as funções da pasta R/ sem bagunçar a tela
#      (usa invisible(), então nada de [[1]], [[2]], [[3]]...).
# ============================================================

# 1) Acha a raiz do projeto -------------------------------------------
achar_raiz <- function() {
  dir <- getwd()
  repeat {
    if (dir.exists(file.path(dir, "R")) && dir.exists(file.path(dir, "scripts"))) {
      return(dir)
    }
    pai <- dirname(dir)
    if (identical(pai, dir)) {
      stop("Não achei a raiz do projeto (procuro uma pasta com R/ e scripts/). ",
           "Rode a partir da pasta do projeto ou use setwd().")
    }
    dir <- pai
  }
}

raiz <- achar_raiz()
setwd(raiz)

# 2) Confere os pacotes ------------------------------------------------
pkg <- c("httr", "jsonlite", "sf", "readr", "xml2")
faltando <- pkg[!vapply(pkg, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) {
  stop("Faltam pacotes R: ", paste(faltando, collapse = ", "),
       ". Instale com install.packages(c('", paste(faltando, collapse = "','"), "'))")
}

# 3) Carrega as funções (em silêncio) -----------------------------------
invisible(lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), source))

cat("✅ Funções do GeoSampa carregadas! Boa garimpagem! 🗺️✨\n")
