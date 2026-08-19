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
caminho_script <- function() {
  arquivo <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(arquivo) && nzchar(arquivo)) return(arquivo)
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg) > 0) sub("^--file=", "", arg[1]) else NULL
}

achar_raiz <- function() {
  script <- caminho_script()
  inicios <- c(if (!is.null(script)) dirname(normalizePath(script, mustWork = FALSE)),
               getwd())
  for (inicio in unique(inicios)) {
    dir <- inicio
    repeat {
      if (dir.exists(file.path(dir, "R")) && dir.exists(file.path(dir, "scripts"))) {
        return(normalizePath(dir, winslash = "/"))
      }
      pai <- dirname(dir)
      if (identical(pai, dir)) break
      dir <- pai
    }
  }
  stop("Não achei a raiz do projeto (procuro uma pasta com R/ e scripts/).")
}

raiz <- achar_raiz()
options(gs.raiz = raiz)

# 2) Confere os pacotes ------------------------------------------------
pkg <- c("httr", "jsonlite", "sf", "readr", "xml2")
faltando <- pkg[!vapply(pkg, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) {
  stop("Faltam pacotes R: ", paste(faltando, collapse = ", "),
       ". Instale com install.packages(c('", paste(faltando, collapse = "','"), "'))")
}

# 3) Carrega as funções (em silêncio), sem alterar o working directory --
arquivos_r <- sort(list.files(file.path(raiz, "R"), full.names = TRUE,
                              pattern = "\\.R$"))
invisible(lapply(arquivos_r, source))

cat("✅ Funções do GeoSampa carregadas! Boa garimpagem! 🗺️✨\n")
