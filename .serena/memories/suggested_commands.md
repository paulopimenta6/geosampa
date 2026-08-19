# Comandos sugeridos

Rodar no R (a partir da raiz do projeto):
- `source("scripts/carregar_funcoes.R")` — carrega todas as funções em silêncio.
- `Rscript scripts/baixar_tudo.R` — baixa todos os equipamentos (WFS).
- `Rscript scripts/baixar_tudo.R saude` / `Rscript scripts/baixar_tudo.R --camada equipamento_saude_ubs_posto_centro` — tema/camada.
- `Rscript tests/testthat.R` — roda a suíte de testes.

Exemplos de análise:
- `prox <- gs_servicos_proximos(cep = "03175-001", raio_m = 3000, camadas = "saude")`
- `gs_analise_servicos(prox, tipo = c("descritivas", "nni", "lisa", "por_distrito"))`
- `gs_relatorio_analises(prox, tipo = c("descritivas", "raio_otimo", "nni"), arquivo = "relatorios/r.html")`

Config:
- `options(gs.osrm_server = "http://localhost:5000")` — servidor OSRM custom (rede viária).
- `options(gs.raiz = "/caminho/para/geosampa")` — raiz explícita (usada por `gs_raiz()`).