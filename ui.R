
routes_shapes_list <- readRDS("data/routes_shapes_list.rds")
intervals_list <- readRDS("data/intervals.rds")


page_sidebar(
  title = "Monitoramento de velocidade ETUFOR",
  sidebar = sidebar(
    title = "Filtros",
    width = 325, 
    # bg = "#e3e4e6",
    
    # selecionar intervalo temporal
    pickerInput(inputId = "interval",
                label = "Intervalos(s)",
                choices = c(intervals_list),
                multiple = TRUE,
                options = pickerOptions(selectAllText = "Selecionar todos",
                                        deselectAllText = "Zerar selecao",
                                        size = 10,
                                        actionsBox = TRUE,
                                        noneSelectedText = "Filtre para intervalo(s)",
                                        style = "background: white"
                )
    ),
    
    # selecionar rota
    pickerInput(inputId = "route",
                label = "Linha(s)",
                choices = c(unique(routes_shapes_list$route_id)),
                multiple = TRUE,
                options = pickerOptions(selectAllText = "Selecionar todas",
                                        deselectAllText = "Zerar selecao",
                                        size = 10,
                                        actionsBox = TRUE,
                                        liveSearch = TRUE,
                                        noneSelectedText = "Filtre para linha(s)",
                                        style = "background: white"
                )
                
                
    ),
    
    
    numericInput(inputId = "velocidade_maxima", 
                 label = "Selecione uma velocidade maxima (km/h)", 
                 value = 40,
                 max = 60,
                 min = 0
    ),
    actionButton(inputId = "submit",
                 label = "Filtrar"),
    hr(),
    downloadBttn(outputId = "download_png",
                 label = "Download map (.png)",
                 style = "simple",
                 size = "sm"
    ),
    downloadBttn(outputId = "download_html",
                 label = "Download map (.html)",
                 style = "simple",
                 size = "sm"
    ),
    downloadBttn(outputId = "download_data",
                 label = "Download data (.gpkg)",
                 style = "simple",
                 size = "sm"
    )
    # downloadBttn(outputId = "download_html",
    #              label = "Download",
    #              style = "simple"
    # )
    
  ),
  card(
    card_header("Map"),
    leafletOutput("map")
  )
)