#!/usr/bin/env Rscript
# ============================================================
# GeoSampa — Baixador de equipamentos públicos
# ------------------------------------------------------------
# Uso:
#   Rscript scripts/baixar_tudo.R                 # baixa TODOS os equipamentos
#   Rscript scripts/baixar_tudo.R saude           # baixa apenas o tema saúde
#   Rscript scripts/baixar_tudo.R educacao esporte # baixa educação e esporte
#   Rscript scripts/baixar_tudo.R --camada equipamento_saude_ubs_posto_centro
#
# Os arquivos caem em data/ como .geojson (mapa) e .csv (tabela).
# ============================================================

# 1) Carrega as funções do projeto -------------------------------------------
arquivo <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
arquivo <- if (length(arquivo) > 0) sub("^--file=", "", arquivo[1]) else
  file.path(getwd(), "scripts", "baixar_tudo.R")
raiz <- dirname(dirname(normalizePath(arquivo, mustWork = FALSE)))
source(file.path(raiz, "scripts", "carregar_funcoes.R"))

# 2) Interpreta os argumentos da linha de comando -----------------------------
args <- commandArgs(trailingOnly = TRUE)
i_camada <- which(args == "--camada")

if (length(i_camada) > 0) {
  if (length(i_camada) != 1 || i_camada == length(args)) {
    stop("Use `--camada NOME_DA_CAMADA` exatamente uma vez.")
  }
  extras <- setdiff(seq_along(args), c(i_camada, i_camada + 1L))
  if (length(extras) > 0) {
    stop("Não misture `--camada` com temas ou outros argumentos.")
  }
  camada <- args[i_camada + 1]
  cat("==> Baixando a camada:", camada, "\n")
  resumo <- gs_baixar_camada(camada)
} else if (length(args) == 0) {
  cat("==> Baixando TODOS os equipamentos públicos...\n")
  resumo <- gs_baixar_todos_equipamentos()
} else {
  for (tema in args) {
    cat("\n==> Tema:", tema, "\n")
    resumo <- gs_baixar_servicos(tema)
  }
}

# 3) Resumo final -------------------------------------------------------------
if (exists("resumo") && !is.null(resumo)) {
  cat("\nConcluído. Resumo:\n")
  print(resumo)
  cat("\nArquivos salvos em:", gs_pasta_dados(), "\n")
}
