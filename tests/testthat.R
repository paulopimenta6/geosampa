library(testthat)

caminho <- "tests/testthat"
if (!dir.exists(caminho)) caminho <- "testthat"
testthat::test_dir(caminho)