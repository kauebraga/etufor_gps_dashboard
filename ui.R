
routes_shapes_list <- readRDS("data/routes_shapes_list.rds")
intervals_list <- readRDS("data/intervals.rds")


page_sidebar(
  title = "Penguins dashboard",
  sidebar = sidebar(
    title = "Filtros",
    
    # selecionar intervalo temporal
    selectInput(inputId = "interval",
                label = "Selecione o(s) intervalos(s)",
                choices = c("Todos",intervals_list),
                multiple = TRUE
    ),
    
    # selecionar rota
    selectInput(inputId = "route",
                label = "Selecione uma linha",
                choices = c("Todas", unique(routes_shapes_list$route_id)),
                multiple = TRUE
                
                
    ),
    
    
    numericInput(inputId = "velocidade_maxima", 
                 label = "Selecione uma velocidade maxima", 
                 value = 40,
                 max = 60,
                 min = 0
    ),
    actionButton(inputId = "submit",
                 label = "Filtrar"),
    hr(),
    downloadBttn(outputId = "download",
                 label = "Download",
                 style = "simple"
                 )
    
  ),
  card(
    card_header("Map"),
    leafletOutput("map")
  )
)