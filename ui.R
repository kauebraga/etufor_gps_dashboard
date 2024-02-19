
routes_shapes_list <- readRDS("data/routes_shapes_list.rds")
intervals_list <- readRDS("data/intervals.rds")


page_navbar(
  tags$head(includeCSS("www/dropdown.css")),
  tags$head(includeCSS("www/sidebar.css")),
  tags$head(includeCSS("www/download.css")),
  shinyjs::useShinyjs(),
  title = tagList(
    span("Monitoramento de velocidades", style ="font-family: Encode Sans; font-size: 30px; font-weight: 800; line-height: 60px; letter-spacing: 0em; text-align: left;")
  ),
  theme = bs_theme(
    preset = "cerulean",
    # primary = "#EA80FC", secondary = "#48DAC6",
    base_font = font_google("Roboto Condensed", wght = "200..900"),
    # navbar_bg = "#25443B"
  ),
  id = "navbar1",
  bg = "#F7B93B",
  nav_panel(title = "Mapa",
            value = "map1",
            
            # map
            # layout_columns(col_widths = c(9, 3),      
            navset_pill_list(
              widths = c(1, 11),
              # card(
              # card_header("Mapa"),
              nav_panel(title = div(
                img(src = "img/search.png", height = "20", width = "20")
              ),
              
              layout_sidebar(
                sidebar = sidebar(
                  id = "sidebar_maps",
                  class = "sidebar_filters",
                  position = "right",
                  title = div(
                    img(src = "img/filtro.png", height = "30", width = "30", span("Filtros", style = "font-family: Encode Sans; font-size: 30px; font-weight: 600; line-height: 37.5px; letter-spacing: 0em; text-align: left; color: #434343; padding-left: 10px"))
                  ),
                  width = 325, 
                  height = "100%",
                  # bg = "#e3e4e6",
                  
                  # selecionar intervalo temporal
                  div(class = "filter_intervalo",
                      radioButtons(inputId = "interval_type",
                                   label = h3("Intervalos(s)"),
                                   choices = c("15 Minutos", "Hora"),
                                   selected = "15 Minutos",
                                   inline = TRUE),
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
                      )),
                  
                  # selecionar rota
                  pickerInput(inputId = "route",
                              label = h3("Linha(s)"),
                              choices = c(unique(routes_shapes_list$route_id)),
                              multiple = TRUE,
                              options = pickerOptions(selectAllText = "Selecionar todas",
                                                      deselectAllText = "Zerar selecao",
                                                      size = 5,
                                                      actionsBox = TRUE,
                                                      liveSearch = TRUE,
                                                      noneSelectedText = "Filtre para linha(s)",
                                                      style = "background: white"
                              )
                              
                              
                  ),
                  # selecionar sentido
                  div(class = "filter_intervalo",
                      awesomeCheckboxGroup(inputId = "direction",
                                           label = h3("Sentido"),
                                           choices = c("Ida/Volta" = "all", "Ida" = "I", "Volta" = "V"),
                                           inline = TRUE
                                           # disabled = TRUE
                                           
                                           
                      )),
                  
                  numericInput(inputId = "velocidade_maxima", 
                               label = h3("Velocidades"), 
                               
                               value = 40,
                               max = 60,
                               min = 0
                  ),
                  actionButton(inputId = "submit",
                               label = "Filtrar")
                  # hr(),
                  
                  
                ),
                div(
                  img(src = "img/fortaleza.png", height = "45", span("Mapa", style = "font-family: Encode Sans; font-size: 30px; font-weight: 600; line-height: 37.5px; letter-spacing: 0em; text-align: left; color: #434343; padding-left: 10px"))
                ),
                leafletOutput("map"
                              # height = "calc(100vh - 20px)"
                ),
                absolutePanel(bottom = 20, left = 30,
                              span(switchInput(inputId = "selection_mode", label = "Modo de selecao", inline = TRUE, labelWidth = 100)),
                              span(id = "submit_selection1", actionButton(inputId = "submit_selection", label = "Aplicar selecao", width = "150px"), style = "display: none"),
                              span(id = "resetar_selecao1", actionButton(inputId = "resetar_selecao", label = "Resetar selecao", width = "150px"), style = "display: none")
                )
                # dropdown(inputId = "dropdown_graphs", label = "Velocidade do trecho ao longo do dia", up = TRUE,
                #          tagList(
                #            highchartOutput("output_graph_interval_map")
                #            
                #          ))
                
              ),
              
              
              
              ),
              nav_panel(
                value = "tab_trechos",
                id =  "tab_trechos",
                title = div(
                  img(src = "img/trecho2.png", height = "20", width = "20")
                ),
                # span("Dashboard", style = "font-family: Encode Sans; font-size: 36px; font-weight: 700; line-height: 45px; letter-spacing: 0em; text-align: left; color: #F07D17"),
                div(
                  div(
                    style = "display: inline",
                    img(src = "img/trecho.png", height = "45", width = "45", span("Trechos", style = "font-family: Encode Sans; font-size: 30px; font-weight: 600; line-height: 37.5px; letter-spacing: 0em; text-align: left; color: #434343; padding-left: 10px;"))
                  ),
                  # capture_pdf(
                  #   selector = "#result-block",
                  #   filename = "results",
                  #   loading = loading(),
                  #   icon("camera"), "Take screenshot of results"
                  # ),
                  screenshotButton(label = "Baixar figura (.png)", id = "result-block", class = "download_trecho_report")
                  # downloadButton("report", "Gerar relatorio (.pdf)", class = "download_trecho_report")
                ),
                div(id = "result-block",
                    layout_columns(col_widths = c(3, 5, 4, 2, 5, 5),
                                   # row_heights = c(1, 1),
                                   fill = FALSE,
                                   # h3("TRECHOS"),
                                   # p("Selecione trecho(s) para mais infos"),
                                   # uiOutput("info_speed"),
                                   div(
                                     img(src = "img/speed.png", height = "30", width = "30", span("Velocidade média", style = "font-family: Encode Sans;font-size: 24px;font-weight: 600;line-height: 30px;letter-spacing: 0em;text-align: left;")),
                                     div(
                                       style = "box-shadow: 0px 4px 4px 0px #00000040; border-radius: 10px; padding: 10px",
                                       highchartOutput("output_graph_speed_segments", height = "300px"),
                                     )),
                                   div(
                                     img(src = "img/speed_dia.png", height = "30", width = "30",span("Velocidade média durante o dia", style = "font-family: Encode Sans;font-size: 24px;font-weight: 600;line-height: 30px;letter-spacing: 0em;text-align: left;")),
                                     div(
                                       style = "box-shadow: 0px 4px 4px 0px #00000040; border-radius: 10px; padding: 10px",
                                       highchartOutput("output_graph_interval_segments", height = "300px")
                                     )),
                                   div(
                                     img(src = "img/speed_dia.png", height = "30", width = "30",span("Velocidade média ao longo dos meses", style = "font-family: Encode Sans;font-size: 24px;font-weight: 600;line-height: 30px;letter-spacing: 0em;text-align: left;")),
                                     div(
                                       style = "box-shadow: 0px 4px 4px 0px #00000040; border-radius: 10px; padding: 10px",
                                       highchartOutput("output_graph_month_segments", height = "300px")
                                     )),
                                   div(
                                     img(src = "img/ponto_onibus.png", height = "30", width = "30",span("Pontos de ônibus", style = "font-family: Encode Sans;font-size: 24px;font-weight: 600;line-height: 30px;letter-spacing: 0em;text-align: left;")),
                                     div(
                                       style = "box-shadow: 0px 4px 4px 0px #00000040; border-radius: 10px; padding: 10px; background: #F07D1726;",
                                       uiOutput("info_paradas_n")
                                     )),
                                   div(
                                     img(src = "img/localizacao.png", height = "30", width = "30", span("Localização dos pontos de ônibus", style = "font-family: Encode Sans;font-size: 24px;font-weight: 600;line-height: 30px;letter-spacing: 0em;text-align: left;")),
                                     div(
                                       style = "box-shadow: 0px 4px 4px 0px #00000040; border-radius: 10px; padding: 10px;",
                                       uiOutput("info_paradas")
                                     )),
                                   div(
                                     img(src = "img/linha.png", height = "30", width = "30", span("Linhas", style = "font-family: Encode Sans;font-size: 24px;font-weight: 600;line-height: 30px;letter-spacing: 0em;text-align: left;")),
                                     div(
                                       style = "box-shadow: 0px 4px 4px 0px #00000040; border-radius: 10px; padding: 10px; background: #F07D1726;",
                                       uiOutput("info_linhas"))
                                   ))
                )),
              # nav_panel(title = icon("search"),
              #           layout_columns(col_widths = c(12),
              #                          fill = FALSE,
              #                          h3("TRECHO"),
              #                          p("Selecione um trecho para mais infos"),
              #                          uiOutput("info_speed"),
              #                          uiOutput("info_paradas"),
              #                          uiOutput("info_linhas"))
              # ),
              nav_panel(title = div(
                img(src = "img/download.png", height = "20", width = "20")
              ),
              div(img(src = "img/download1.png", height = "45", width = "45", span("Download", style = "font-family: Encode Sans; font-size: 30px; font-weight: 600; line-height: 37.5px; letter-spacing: 0em; text-align: left; color: #434343; padding-left: 10px"))),
              layout_columns(col_widths = c(2, 2, 2, 2), fill = FALSE, height = "800px",
                             div(h3("Download HTML"),
                                 div(style = "background: #FFFFFF; box-shadow: 0px 4px 4px 0px #00000040; padding: 10px",
                                     layout_columns(col_widths = c(12, 12), fill = FALSE, fillable = FALSE,
                                                    img(src = "img/download_html.png", height = "150px", style = "margin: auto; display:block"),
                                                    downloadBttn(outputId = "download_html1",
                                                                 label = "Download map (.html)",
                                                                 style = "simple",
                                                                 size = "sm"
                                                    )))),
                             div(h3("Download GPKG"),
                                 div(style = "background: #FFFFFF; box-shadow: 0px 4px 4px 0px #00000040; padding: 10px",
                                     layout_columns(col_widths = c(12, 12), fill = FALSE, fillable = FALSE,
                                                    img(src = "img/download_gpkg.png", height = "150px", style = "margin: auto; display:block"),
                                                    downloadBttn(outputId = "download_gpkg1",
                                                                 label = "Download data (.gpkg)",
                                                                 style = "simple",
                                                                 size = "sm"
                                                    )))),
                             div(h3("Download PNG"),
                                 div(style = "background: #FFFFFF; box-shadow: 0px 4px 4px 0px #00000040; padding: 10px",
                                     layout_columns(col_widths = c(12, 12), fill = FALSE, fillable = FALSE,
                                                    img(src = "img/download_png.png", height = "150px", style = "margin: auto; display:block"),
                                                    downloadBttn(outputId = "download_png1",
                                                                 label = "Download map (.png)",
                                                                 style = "simple",
                                                                 size = "sm"
                                                    ))))
                             # downloadBttn(outputId = "download_html",
                             #              label = "Download",
                             #              style = "simple"
                             # )
              ))
            )),
  nav_panel(
    "Gráficos",
    value = "graphs",
    layout_sidebar(
      sidebar = sidebar(
        # title = "Filtros",
        width = 325, 
        # bg = "#e3e4e6",
        class = "sidebar_filters_graph",
        title = div(
          img(src = "img/filtro.png", height = "30", width = "30", span("Filtros", style = "font-family: Encode Sans; font-size: 30px; font-weight: 600; line-height: 37.5px; letter-spacing: 0em; text-align: left; color: #434343; padding-left: 10px"))
        ),
        # selecionar intervalo temporal
        # radioGroupButtons(inputId = "interval_type1",
        #                   label = "Intervalos(s)",
        #                   choices = c("15 Minutos", "Hora"),
        #                   selected = "15 Minutos"),
        div(class = "filter_intervalo1",
            pickerInput(inputId = "interval1",
                        label = h3("Intervalos(s)"),
                        choices = c(intervals_list),
                        multiple = TRUE,
                        options = pickerOptions(selectAllText = "Selecionar todos",
                                                deselectAllText = "Zerar selecao",
                                                size = 10,
                                                actionsBox = TRUE,
                                                noneSelectedText = "Filtre para intervalo(s)",
                                                style = "background: white"
                        )
            )),
        
        # h4("Adicione para comparacao"),
        # selecionar rota
        div(class = "filter_intervalo1",
            pickerInput(inputId = "route1",
                        label = h3("Linha(s)"),
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
                        
                        
            )),
        actionButton("reset_graph1", "Resetar gráficos"),
      ),
      # leafletOutput("map")
      layout_columns(col_widths = c(10, 10),
                     div(
                       img(src = "img/velocidade_media_graph.png", height = "30", width = "30", span("Velocidade média", style = "font-family: Encode Sans;font-size: 24px;font-weight: 600;line-height: 30px;letter-spacing: 0em;text-align: left;")),
                       div(
                         style = "box-shadow: 0px 4px 4px 0px #00000040; border-radius: 10px; padding: 10px",
                         highchartOutput("output_graph_interval", height = "300px"),
                       )),
                     div(
                       img(src = "img/velocidade_media_mes_graph.png", height = "30", width = "30", span("Velocidade média por mês", style = "font-family: Encode Sans;font-size: 24px;font-weight: 600;line-height: 30px;letter-spacing: 0em;text-align: left;")),
                       div(
                         style = "box-shadow: 0px 4px 4px 0px #00000040; border-radius: 10px; padding: 10px",
                         highchartOutput("output_graph_month", height = "300px")
                       ))
      )
      
    )
  ),
  nav_spacer(),
  nav_item(img(src = "img/etufor.png", height = "50", style = "float: right")),
)