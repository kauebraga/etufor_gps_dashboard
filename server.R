
intervals_list <- readRDS("data/intervals.rds")
# open data
segments_sf <- setDT(readRDS("data/segments_gtfs_unique.rds"))
segments_speeds <- setDT(readRDS("data/gps_by_segment_unique.rds"))
segments_variables <- setDT(readRDS("data/gps_by_segment_variables.rds"))
stops_routes <- readRDS("data/stops_gtfs_routes_sf.rds")
stops_unique <- readRDS("data/stops_gtfs_sf.rds")

routes_shapes <- readRDS("data/routes_shapes.rds")

# graphs
data_month_all <- readRDS("data/graphs/graphs_month_all.rds")
data_month <- readRDS("data/graphs/graphs_month.rds")

data_interval_all <- readRDS("data/graphs/graphs_interval_all.rds")
data_interval <- readRDS("data/graphs/graphs_interval.rds")
data_interval_segments <- readRDS("data/graphs/graphs_interval_segments.rds")

data_fluxo_all <- readRDS("data/graphs/graphs_fluxo_all.rds")

trechos_possiveis <- readRDS("data/trechos_possiveis.rds")

server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
  
  observeEvent(c(input$submit), {
    
    req(input$submit >= 1)
    
    # print("guaaaa")
    
    shinyjs::disable(id = "download_png_bttn")
    shinyjs::disable(id = "download_html_bttn")
    shinyjs::disable(id = "download_gpkg_data")
    
    
  })
  
  # set data
  data <- reactiveValues(start = NULL,
                         segments = NULL,
                         month = NULL,
                         stops = NULL)
  map <- reactiveValues(start = NULL,
                        map1 = NULL)
  
  output$map <- renderLeaflet({
    
    
    # join the segment data with the sf
    segments_data <- merge(segments_speeds, segments_sf)
    segments_data <- sf::st_sf(segments_data, crs = 4326)
    
    data$start <- segments_data
    
    # data$segments <- segments_data
    # data$stops <- stops_unique
    
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = segments_data$velocidade)
    
    # label for clicking on the segment
    label_segment <- paste0("<b>Velocidade:</b>", round(segments_data$velocidade, 1), " km/h")
    
    
    m <- leaflet(data = segments_data, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light", layerId = "epa") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      addPolylines(color = ~pal(velocidade),
                   weight = 2,
                   opacity = 0.8,
                   layerId = ~segment_id,
                   label = lapply(label_segment, htmltools::HTML),
                   highlightOptions = highlightOptions(opacity = 1, weight = 4, color = "black")) %>%
      
      addCircleMarkers(data = stops_unique,  
                       stroke = FALSE, fillOpacity = 0.5,
                       radius = 2,
                       color = "black",
                       group = "Paradas") %>%
      
      addLayersControl(baseGroups = c("Light", "Dark", "Satellite"),
                       overlayGroups = c("Paradas"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "bottomright") %>%
      addLegend("bottomright", pal = pal, values = ~velocidade,
                title = "Velocidade (km/h)"
      ) %>%
      hideGroup("Paradas")
    
    
    map$start <- m
    
    m
    
  })
  
  
  # filter speed
  observeEvent(c(input$submit), {
    
    req(input$submit >= 1)
    
    print("input$interval")
    print(input$interval)
    
    
    # intervalo <- unlist(strsplit(intervalo, "\\|"))
    
    # # NEW CODE STARTS Here
    # # filter velocidade
    segments_data <- segments_variables[velocidade <= input$velocidade_maxima]
    # # filter interval
    segments_data <- if (is.null(input$interval)) segments_data else segments_data[interval %in% unlist(strsplit(input$interval, "\\|"))]
    # # filter route
    segments_data <- if (is.null(input$route)) segments_data else segments_data[route_id %in% input$route]
    # # filter direction
    segments_data <- if (is.null(input$route)) segments_data else if (input$direction == "all") segments_data else segments_data[direction %in% input$direction]
    print("segments_data")
    print(segments_data)
    
    # filter stops
    stops <- if (is.null(input$route)) stops_unique else if (input$direction == "all") subset(stops_routes, route_id %in% input$route) else subset(stops_routes, route_id %in% input$route & direction %in% input$direction)
    # # NEW CODE finishds Here
    
    segments_data <- segments_data[, .(velocidade = weighted.mean(velocidade, n)), by = segment_id]
    segments_data <- merge(segments_data, segments_sf)
    segments_data <- sf::st_sf(segments_data, crs = 4326)
    
    data$segments <- segments_data
    data$stops <- stops
    # data$segments <- st_sf(data$segments, crs = 4326)
    
    
    
  })
  
  
  
  
  
  observeEvent(c(data$segments), {
    
    req(input$submit >= 1)
    
    
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = data$segments$velocidade)
    
    # print(data$segments)
    
    # label for clicking on the segment
    label_segment <- paste0("<b>Velocidade:</b>", round(data$segments$velocidade, 1), " km/h")
    
    m <- leafletProxy("map", data = data$segments) %>%
      clearMarkers() %>%
      clearControls() %>%
      clearShapes() %>%
      addPolylines(color = ~pal(velocidade),
                   weight = 3,
                   opacity = 0.8,
                   layerId = ~segment_id,
                   label = lapply(label_segment, htmltools::HTML),
                   highlightOptions = highlightOptions(opacity = 1, weight = 10, color = "black")) %>%
      addCircleMarkers(data = data$stops,  
                       stroke = FALSE, fillOpacity = 0.5,
                       radius = 2,
                       color = "black",
                       group = "Paradas") %>%
      
      addLayersControl(baseGroups = c("Light", "Dark", "Satellite"),
                       overlayGroups = c("Paradas"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "bottomright") %>%
      
      addLegend("bottomright", pal = pal, values = ~velocidade,
                title = "Velocidade"
      ) %>%
      hideGroup("Paradas")
    
    
    if (!is.null(input$route)) {
      
      
      bbox <- sf::st_bbox(sf::st_transform(data$segments, 4326))
      
      
      
      m <- m %>%
        flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
      
    }
    
    map$map1 <- m
    
    m
    
  })
  
  
  
  
  
  
  
 
  # update intervalo selection depending on the option ------------------------------------------
  
  observeEvent(c(input$interval_type), {
    
    req(input$interval_type >= 1)
    
    if (input$interval_type == "Hora") {
      
      
      updatePickerInput(session = session,
                        inputId = "interval",
                        choices = c("06:00" = "06:00|06:15|06:30|06:45",
                                    "07:00" = "07:00|07:15|07:30|07:45",
                                    "08:00" = "08:00|08:15|08:30|08:45",
                                    "09:00" = "09:00|09:15|09:30|09:45",
                                    "10:00" = "10:00|10:15|10:30|10:45",
                                    "11:00" = "11:00|11:15|11:30|11:45")
      )
      
      
    } else if (input$interval_type == "15 Minutos") {
      
      
      updatePickerInput(session = session,
                        inputId = "interval",
                        choices = c(intervals_list))
      
      
    }
    
  })
  observeEvent(c(input$interval_type1), {
    
    req(input$interval_type1 >= 1)
    
    if (input$interval_type1 == "Hora") {
      
      
      updatePickerInput(session = session,
                        inputId = "interval1",
                        choices = c("06:00" = "06:00|06:15|06:30|06:45",
                                    "07:00" = "07:00|07:15|07:30|07:45",
                                    "08:00" = "08:00|08:15|08:30|08:45",
                                    "09:00" = "09:00|09:15|09:30|09:45",
                                    "10:00" = "10:00|10:15|10:30|10:45",
                                    "11:00" = "11:00|11:15|11:30|11:45")
      )
      
      
    } else if (input$interval_type1 == "15 Minutos") {
      
      
      updatePickerInput(session = session,
                        inputId = "interval1",
                        choices = c(intervals_list))
      
      
    }
    
  })
  
  
  
  # enable direction ------------------------------------------------------------------------------
  
  observeEvent(c(input$route), {
    # print("buh")
    
    updateRadioGroupButtons(session = session,
                            inputId = "direction",
                            disabled = FALSE)
    
    
  })  
  
  
  
  # info on the right bar -----------------------------------------------------------------------
  
  info <- reactiveValues(segment_id = NULL,
                         speed = NULL,
                         stop_name_initial = NULL,
                         stop_name_end = NULL,
                         linhas = NULL,
                         linhas_ida = NULL,
                         linhas_volta = NULL,
                         stops = NULL,
                         stops_id = NULL,
                         stops_n = NULL,
                         stops_sf = NULL,
                         data_interval = NULL)
  
  # calculate the infos to display
  
  observeEvent(c(input$map_shape_click), {
    
    # print("pururu")
    
    # print("input$map_shape_click")
    # print(input$map_shape_click$id)
    # print(data$start)
    
    req(isFALSE(input$selection_mode))
    
   
    
    
  })
  
  output$rank_final <- renderUI({
    
    # gather info
    text <- paste0("<h3>Trecho ", info$segment_id, "</h3><br>",
                   "<span style = 'font-weight: bold'>Velocidade</span>: <span> ", round(info$speed, 1), " km/h</span><br>",
                  "<span style = 'font-weight: bold'>Parada inicio: </span>", info$stop_name_initial, "<br>", 
                  "<span style = 'font-weight: bold'>Parada fim: </span>", info$stop_name_end)
    
    tagList(
      HTML(text)

    )
    
    
  })
  
  
  # 
  # output$info_speed <- renderUI({
  #   
  #   if (is.null(input$map_shape_click)) speed <- NULL else speed <- paste0(round(info$speed, 1), " km/h")
  #   
  #   # gather info
  #   value_box(
  #     title = "Velocidade media",
  #     value = speed,
  #     showcase = bsicons::bs_icon("speedometer"),
  #     theme_color = "secondary"
  #     # showcase = bsicons::bs_icon("align-bottom")
  #   )
  #   
  #   
  # })
  # 
  # output$info_paradas <- renderUI({
  #   
  #   # gather info
  #   value_box(
  #     title = "Paradas do trecho",
  #     HTML(paste0(info$stop_name_initial, "<br>", info$stop_name_end)),
  #     showcase = bsicons::bs_icon("sign-stop"),
  #     theme_color = "secondary"
  #     # showcase = bsicons::bs_icon("align-bottom")
  #   )
  #   
  #   
  # })
  # 
  # output$info_linhas <- renderUI({
  #   
  #   # gather info
  #   value_box(
  #     title = "Linhas servidas",
  #     HTML(paste(info$linhas, collapse = " /// ")),
  #     showcase = bsicons::bs_icon("bus-front"),
  #     theme_color = "secondary"
  #     # showcase = bsicons::bs_icon("align-bottom")
  #   )
  #   
  #   
  # })
  
  

# graph close to the map ----------------------------------------------------------------------

  output$output_graph_interval_map <- renderHighchart({
    
    req(input$map_shape_click$id)
    
    data_ok <- data_interval_segments[segment_id == input$map_shape_click$id]
    # print("data_ok")
    # print(data_ok)
    
    highchart() %>%
      hc_add_series(data = data_ok,
                    hcaes(x = interval, y = velocidade),
                    type = "line",
                    color = "#F7B93B",
                    # lineWidth = 5,
                    # opacity = 0.5,
                    name = "Todas",
                    tooltip = list(pointFormat = sprintf("{series.name}: {point.y} km/h"),
                                   valueDecimals = 1)
      ) %>%

      hc_title(text = sprintf("Velocidade media do trecho %s por intervalo de 15 minutos", input$map_shape_click$id)) %>%

      # hc_legend(verticalAlign = "top") %>%
      hc_yAxis(title = list(text = "Velocidade (km/h)", style = list(fontSize = 16)),
               labels = list(style = list(fontSize = 15)),
               maxPadding = 0.001) %>%
      hc_xAxis(title = list(text = "", style = list(fontSize = 16)),
               labels = list(style = list(fontSize = 15)),
               type = "category")
    
    
    
    
  })  
  
  source("src/graficos.R", local = TRUE)
  source("src/modo_selecao.R", local = TRUE)
  source("src/download.R", local = TRUE)
  
  
}