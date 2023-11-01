
routes_shapes_list <- readRDS("data/routes_shapes_list.rds")
intervals_list <- readRDS("data/intervals.rds")


page_navbar(
  shinyjs::useShinyjs(),
  title = "Monitoramento de velocidade ETUFOR",
  theme = bs_theme(preset = "litera"),
  id = "navbar",
  bg = "#F7B93B",
  nav_panel(title = "Mapa",
            layout_sidebar(
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
              "Mapa",
              leafletOutput("map", height = 600),
            )),
  nav_panel(
    "Gráficos",
    value = "graphs",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filtros",
        width = 325, 
        # bg = "#e3e4e6",
        
        # selecionar intervalo temporal
        pickerInput(inputId = "interval1",
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
        pickerInput(inputId = "route1",
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
        actionButton("reset_graph1", "Resetar gráficos"),
      ),
      # leafletOutput("map")
      fluidRow(
               column(width = 6,
                      highchartOutput("output_graph_month", width = 400, height = 280)),
               column(width = 6,
                      highchartOutput("output_graph_fluxo", width = 400, height = 280))),
      fluidRow(      
               column(width = 12,
                      highchartOutput("output_graph_interval", width = 800, height = 350)))
      
      
    )
  )
)