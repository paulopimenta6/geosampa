# Carrega as funções do projeto e disponibiliza helpers para os testes.
# O testthat roda com working directory dentro de tests/, então subimos
# até achar a raiz do projeto (pasta que contém R/ e scripts/).
acha_raiz <- function() {
  dir <- getwd()
  repeat {
    if (dir.exists(file.path(dir, "R")) && dir.exists(file.path(dir, "scripts"))) {
      return(dir)
    }
    pai <- dirname(dir)
    if (identical(pai, dir)) stop("Não achei a raiz do projeto.")
    dir <- pai
  }
}

raiz <- acha_raiz()
invisible(lapply(list.files(file.path(raiz, "R"), full.names = TRUE,
                            pattern = "\\.R$"), source))

tem_dados <- function() {
  length(gs_camadas_local()) > 0
}