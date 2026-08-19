# Tarefa considerada concluída quando...

Não há lint/typecheck formal (R sem ferramentas configuradas). Verificação:

1. Parse de todos os R files: `for f in R/*.R; do Rscript -e "invisible(parse('$f'))"; done`
2. Smoke test das funções (carregar + rodar):
   `Rscript -e 'invisible(lapply(list.files("R", full.names=TRUE, pattern="\\.R$"), source))'`
3. Testes automatizados:
   `Rscript tests/testthat.R` → esperado `[ FAIL 0 | WARN 0 | PASS 30 ]` (2 SKIP intencionais: rodar sob CRAN + robustez sem spdep).
4. Docs: README.md e DOCUMENTACAO.md refletem assinaturas/tipos (tabelas de distância e análises).

Observações:
- `data/` precisa ter CSVs (gitignored). Baixar com `Rscript scripts/baixar_tudo.R` se estiver vazio.
- Testes com rede (por_distrito baixa `distrito_municipal`) dependem de internet na primeira execução.