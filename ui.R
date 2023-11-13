
routes_shapes_list <- readRDS("data/routes_shapes_list.rds")
intervals_list <- readRDS("data/intervals.rds")


page_navbar(
  tags$head(includeCSS("www/dropdown.css")),
  shinyjs::useShinyjs(),
  title = "Monitoramento de velocidade ETUFOR",
  theme = bs_theme(
    preset = "cerulean",
    # primary = "#EA80FC", secondary = "#48DAC6",
    base_font = font_google("Roboto Condensed", wght = "200..900"),
    # navbar_bg = "#25443B"
  ),
  id = "navbar",
  bg = "#F7B93B",
  nav_panel(title = "Mapa",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtros",
                width = 325, 
                # bg = "#e3e4e6",
                
                # selecionar intervalo temporal
                radioGroupButtons(inputId = "interval_type",
                                  label = "Intervalos(s)",
                                  choices = c("15 Minutos", "Hora"),
                                  selected = "15 Minutos"),
                pickerInput(inputId = "interval",
                            label = NULL,
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
                # selecionar sentido
                radioGroupButtons(inputId = "direction",
                                  label = "Sentido",
                                  choices = c("Ida e Volta" = "all", "Ida" = "I", "Volta" = "V"),
                                  disabled = TRUE
                                  
                                  
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
              # map
              layout_columns(col_widths = c(9, 3),              
                             card(
                               card_header("Mapa"),
                               leafletOutput("map", height = 400),
                               dropdown(inputId = "dropdown_graphs", label = "Velocidade do trecho ao longo do dia", up = TRUE,
                                              tagList(
                                                highchartOutput("output_graph_interval_map")
                                                # absolutePanel(
                                                #   style = "z-index: 9999999999; opacity: 0.97; background-color: white",
                                                #   height = 500, width = 500,
                                                #   "parpararara"
                                                # )
                                                
                                              ))),
                             layout_columns(col_widths = c(12),
                                            fill = FALSE,
                                            h3("TRECHO"),
                                            p("Selecione um trecho para mais infos"),
                                            uiOutput("info_speed"),
                                            uiOutput("info_paradas"),
                                            uiOutput("info_linhas")
                                            
                                            
                                            
                             )),
              # card(
              #   card_header("Info"),
              #   lorem::ipsum(15)),
            )),
  nav_panel(
    "Gráficos",
    value = "graphs",
    layout_sidebar(
      sidebar = sidebar(
        # title = "Filtros",
        width = 325, 
        # bg = "#e3e4e6",
        
        # selecionar intervalo temporal
        h4("Filtro"),
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
        
        h4("Adicione para comparacao"),
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
                                            noneSelectedText = "Adicione linha(s)",
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