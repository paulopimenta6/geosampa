# Convenções

- Funções públicas prefixadas com `gs_` (ex.: `gs_servicos_proximos`). Helpers internos também `gs_`, sem export.
- Cabeçalho de cada arquivo em `# ===...===` descrevendo o propósito; comentários explicativos em português.
- Interface das funções de análise: lista nomeada; análises que dependem de pacote opcional devolvem `list(executado = FALSE, mensagem = "...")` em vez de quebrar (padrão usado por moran/rede_viaria/ripley_k/getis/lisa/cobertura_populacional).
- `match.arg` para argumentos de escolha (ex.: `tipo_distancia`, `tipo`).
- Dados de saída como data.frame simples; sf quando há geometria (voronoi, grade hex, distritos).
- Atributos de contexto (`ponto`, `raio_m`, `tipo_distancia`) ficam como `attr()` no data.frame de `gs_servicos_proximos`; funções de mapa/análise validam a presença de `attr("ponto")` com mensagem clara.
- Mapas retornam objeto ggplot/leaflet invisível (`invisible(mapa)`) quando `salvar` é NULL.
- Mensagens ao usuário em português, com `message()`/`cat()` para progresso e `stop()` para erros.
- Configurações globais via `options()` (ex.: `gs.raiz`, `gs.osrm_server`, cache `gs.indice_cep`) e funções `gs_*()` que leem options com fallback (ex.: `gs_osrm_server()`).
- Dados administrativos baixados sob demanda (distritos) são cacheados em `data/`; geometrias inválidas do GeoSampa são corrigidas com `sf::st_make_valid()`.