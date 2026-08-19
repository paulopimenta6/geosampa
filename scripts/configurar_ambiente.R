#!/usr/bin/env Rscript

arquivo <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
arquivo <- if (length(arquivo) > 0) sub("^--file=", "", arquivo[1]) else
  file.path(getwd(), "scripts", "configurar_ambiente.R")
raiz <- dirname(dirname(normalizePath(arquivo, mustWork = FALSE)))
lockfile <- file.path(raiz, "renv.lock")

if (!file.exists(lockfile)) stop("renv.lock não encontrado em: ", raiz)
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

cat("Restaurando o ambiente R registrado em renv.lock...\n")
renv::restore(project = raiz, lockfile = lockfile, prompt = FALSE)
cat("Ambiente restaurado. Reinicie a sessão R antes de usar o projeto.\n")
