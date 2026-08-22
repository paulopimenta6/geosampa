# Preparação inicial
source("scripts/carregar_funcoes.R")

# Todo o catálogo de equipamentos: pode demorar e ocupar espaço
gs_baixar_todos_equipamentos()

# Lê um CEP e identifica a existência do endereço
gs_ler_cep("05596090")

# Transformar CEP em coordenada e verifica as coordenadas do CEP
gs_cep_para_coordenadas("05596090")

# Busca por CEP pelo aparelhos urbanos requeridos
proximos <- gs_servicos_proximos(
  cep = "05596090",
  camadas = c("saude", "esporte"),
  raio_m = 3000,
  tipo_distancia = "rede_viaria"
)

# Geração de mapas estáticos
gs_mapa_servicos(
  proximos,
  interativo = FALSE,
  raio_m = 3000,
  salvar = "mapas/05596090.png"
)

# Geração de mapas interativos
gs_mapa_servicos(
  proximos,
  interativo = TRUE,
  raio_m = 3000,
  salvar = "mapas/05596090.html"
)

# Gerar análises
analises <- gs_analise_servicos(
  proximos,
  tipo = c("descritivas", "vizinho_mais_proximo", "raio_otimo")
)

# Salvar análises
gs_salvar_analises(
  proximos,
  analises,
  dir = "saidas/05596090",
  formato_relatorio = "ambos"
)