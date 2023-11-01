

# open data
segments_sf <- setDT(readRDS("data/segments_gtfs_unique.rds"))
segments_speeds <- setDT(readRDS("data/gps_by_segment_unique.rds"))
segments_variables <- setDT(readRDS("data/gps_by_segment_variables.rds"))
stops_routes <- readRDS("data/stops_gtfs_routes_sf.rds")
stops_unique <- readRDS("data/stops_gtfs_sf.rds")

# graphs
data_month_all <- readRDS("data/graphs/graphs_month_all.rds")
data_month <- readRDS("data/graphs/graphs_month.rds")

data_interval_all <- readRDS("data/graphs/graphs_interval_all.rds")
data_interval <- readRDS("data/graphs/graphs_interval.rds")

data_fluxo_all <- readRDS("data/graphs/graphs_fluxo_all.rds")

server <- function(input, output, session) {
  
  
  observeEvent(c(input$submit), {
    
    req(input$submit >= 1)
    
    print("guaaaa")
    
    shinyjs::disable(id = "download_png_bttn")
    shinyjs::disable(id = "download_html_bttn")
    shinyjs::disable(id = "download_gpkg_data")
    
    
  })
  
  # set data
  data <- reactiveValues(start = NULL,
                         segments = NULL,
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
                   label = lapply(label_segment, htmltools::HTML)) %>%
      
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
                title = "Velocidade"
      )
    
    
    map$start <- m
    
    m
    
  })
  
  
  # filter speed
  observeEvent(c(input$submit), {
    
    req(input$submit >= 1)
    
    # print(input$velocidade_maxima)
    # print(input$route)
    # print(input$interval)
    
    if (is.null(input$route)) {
      
      # print(segments_variables)
      # print(input$interval)
      
      if (is.null(input$interval)) intervalo <- unique(segments_variables$interval) else intervalo <- input$interval
      
      segments_data <- segments_variables[velocidade <= input$velocidade_maxima & interval %in% intervalo]
      
      stops <- stops_unique
      
      
    } else if (is.null(input$interval)) {
      
      segments_data <- segments_variables[velocidade <= input$velocidade_maxima & route_id %in% input$route]
      
      stops <- subset(stops_routes, route_id %in% input$route)
      
    } else if (is.null(input$route) & is.null(input$interval)) {
      
      print("aqui2")
      segments_data <- segments_variables[velocidade <= input$velocidade_maxima]
      
    } else {
      
      
      print("aqui3")
      segments_data <- segments_variables[velocidade <= input$velocidade_maxima & interval %in% input$interval & route_id %in% input$route]
      stops <- subset(stops_routes, route_id %in% input$route)
      
    }
    
    segments_data <- segments_data[, .(velocidade = median(velocidade)), by = segment_id]
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
                   label = lapply(label_segment, htmltools::HTML)) %>%
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
      )
    
    
    if (!is.null(input$route)) {
      
      
      bbox <- sf::st_bbox(sf::st_transform(data$segments, 4326))
      
      
      
      m <- m %>%
        flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
      
    }
    
    map$map1 <- m
    
    m
    
  })
  
  
  
  
  
  

# download ------------------------------------------------------------------------------------

  # data
  output$download_png <- downloadHandler(
    
    
    
    # generate button with data
    filename = function() {
      
      
      # sprintf("data_%s_%s.gpkg", "", "")
      "map.png"
      
    },
    content = function(file) {
      
      if (input$submit == 0) {
        
        
        # print(map$m)
        
        mapview::mapshot(map$start, file = file)
        
        
        # sf::st_write(data$start, file)
        
      } else {
        
        print("foi")
        
        # sf::st_write(data$segments, file)
        mapview::mapshot(map$map, file = file)
        
      }
      
    }
    
  )  
  
  
  
  # data
  output$download_html <- downloadHandler(
    
    
    
    # generate button with data
    filename = function() {
      
      
      # sprintf("data_%s_%s.gpkg", "", "")
      "map.html"
      
    },
    content = function(file) {
      
      
      if (input$submit == 0) {
        
        
        # print(map$m)
        
        mapview::mapshot(map$start, file)
        
        
        # sf::st_write(data$start, file)
        
      } else if (input$submit >= 1) {
        
        print("aqui")
        
      # sf::st_write(data$segments, file)
        # mapview::mapshot(input[["map"]], file, cliprect = "viewport")
        # htmlwidgets::saveWidget(input[["map"]], file)
        htmlwidgets::saveWidget(input[["map"]], "temp.html", selfcontained = FALSE)
        webshot::webshot("temp.html", file = file, cliprect = "viewport")
        
      }
      
    }
    
  )  
  
  
  # data
  output$download_data <- downloadHandler(
    
    
    
    # generate button with data
    filename = function() {
      
      
      # sprintf("data_%s_%s.gpkg", "", "")
      "data.gpkg"
      
    },
    content = function(file) {
      
      if (input$submit == 0) {
        
        
        sf::st_write(data$start, file)
        
      } else {
        
        sf::st_write(data$segments, file)
        
      }
      
    }
    
  )  
  
  observeEvent(c(input$interval, input$route), {
    
    
    
    
  })
  

# graphs --------------------------------------------------------------------------------------
  
  
  output$output_graph_month <- renderHighchart({
    
    # graphs
    # data_month_all <- readRDS("data/graphs/graphs_month_all.rds")
    # data_month <- readRDS("data/graphs/graphs_month.rds")
    
    highchart() %>%
      
      hc_add_series(data = data_month_all,
                    hcaes(x = month, y = velocidade),
                    type = "line",
                    color = "#F7B93B",
                    # lineWidth = 5,
                    # opacity = 0.5,
                    name = "Todas",
                    tooltip = list(pointFormat = sprintf("{series.name}: {point.y} km/h"),
                                   valueDecimals = 1)
      ) %>%
      
      hc_title(text = "Velocidade media por mes") %>%
      
      # hc_legend(verticalAlign = "top") %>%
      hc_yAxis(title = list(text = "Velocidade (km/h)", style = list(fontSize = 16)),
               labels = list(style = list(fontSize = 15)),
               maxPadding = 0.001) %>%
      hc_xAxis(title = list(text = "", style = list(fontSize = 16)),
               labels = list(style = list(fontSize = 15)),
               type = "category")
      # hc_add_theme(hc_theme_smpl(
      #   # chart = list(
      #   #   # backgroundColor = "#1C1C1C",
      #   #   # backgroundColor = "white",
      #   #   style = list(fontFamily = "Franklin Gothic Book"))
      #   title = list(style = list(fontFamily = "Franklin Gothic Demi",
      #                             textTransform = "none"))
      #   
      # ))
    
    
    
    
  })
  
  
  

  output$output_graph_interval <- renderHighchart({
    
    # graphs
    # data_month_all <- readRDS("data/graphs/graphs_month_all.rds")
    # data_month <- readRDS("data/graphs/graphs_month.rds")
    
    highchart() %>%
      
      hc_add_series(data = data_interval_all,
                    hcaes(x = interval, y = velocidade),
                    type = "line",
                    color = "#F7B93B",
                    # lineWidth = 5,
                    # opacity = 0.5,
                    name = "Todas",
                    tooltip = list(pointFormat = sprintf("{series.name}: {point.y} km/h"),
                                   valueDecimals = 1)
      ) %>%
      
      hc_title(text = "Velocidade media por intervalo de 15 minutos") %>%
      
      # hc_legend(verticalAlign = "top") %>%
      hc_yAxis(title = list(text = "Velocidade (km/h)", style = list(fontSize = 16)),
               labels = list(style = list(fontSize = 15)),
               maxPadding = 0.001) %>%
      hc_xAxis(title = list(text = "", style = list(fontSize = 16)),
               labels = list(style = list(fontSize = 15)),
               type = "category")
      # hc_add_theme(hc_theme_smpl(
      #   # chart = list(
      #   #   # backgroundColor = "#1C1C1C",
      #   #   # backgroundColor = "white",
      #   #              style = list(fontFamily = "Franklin Gothic Book"))
      #   
      #   title = list(style = list(fontFamily = "Franklin Gothic Demi",
      #                             textTransform = "none"))
      #   
      # ))
    
    
    
    
  })

  output$output_graph_fluxo <- renderHighchart({
    
    # graphs
    # data_month_all <- readRDS("data/graphs/graphs_month_all.rds")
    # data_month <- readRDS("data/graphs/graphs_month.rds")
    
    highchart() %>%
      
      hc_add_series(data = data_fluxo_all,
                    hcaes(x = fluxo_horario, y = velocidade),
                    type = "line",
                    color = "#F7B93B",
                    # lineWidth = 5,
                    # opacity = 0.5,
                    name = "Todas",
                    tooltip = list(pointFormat = sprintf("{series.name}: {point.y} km/h"),
                                   valueDecimals = 1)
      ) %>%
      
      hc_title(text = "Velocidade media por fluxo horario") %>%
      
      # hc_legend(verticalAlign = "top") %>%
      hc_yAxis(title = list(text = "Velocidade (km/h)", style = list(fontSize = 16)),
               labels = list(style = list(fontSize = 15)),
               maxPadding = 0.001) %>%
      hc_xAxis(title = list(text = "[veh/hora]", style = list(fontSize = 16)),
               labels = list(style = list(fontSize = 15)),
               type = "category")
      # hc_add_theme(hc_theme_smpl(
      #   # chart = list(
      #   #   # backgroundColor = "#1C1C1C",
      #   #   # backgroundColor = "white",
      #   #              style = list(fontFamily = "Franklin Gothic Book"))
      #   
      #   title = list(style = list(fontFamily = "Franklin Gothic Demi",
      #                             textTransform = "none"))
      #   
      # ))
    
    
    
    
  })
  
  
  reV_order <- reactiveValues(values_route = NULL, values_max = NULL,
                              values_interval = NULL)
  
  # use reactive to get and sort the selected terms in the order of selection
  ordered_colnames_route <- reactive({
    
    # req(input$city_compare1_initial)
    # print("que1")
    # print(input$city_compare1_initial)
    
    if (length(reV_order$values_route) > length(input$route1)) {
      reV_order$values_route <- reV_order$values_route[reV_order$values_route %in% input$route1]
      # print("que1")
      
    }else {
      reV_order$values_route <- c(reV_order$values_route, input$route1[!input$route1 %in% reV_order$values_route])
      # print("aqui")
      # print(reV_order$values)
    }
    reV_order$values_route
  })
  ordered_colnames_interval <- reactive({
    
    # req(input$city_compare1_initial)
    # print("que1")
    # print(input$city_compare1_initial)
    
    if (length(reV_order$values_interval) > length(input$interval1)) {
      reV_order$values_interval <- reV_order$values_interval[reV_order$values_interval %in% input$interval1]
      # print("que1")
      
    }else {
      reV_order$values_interval <- c(reV_order$values_interval, input$interval1[!input$interval1 %in% reV_order$values_interval])
      # print("aqui")
      # print(reV_order$values)
    }
    reV_order$values_interval
  })
  
  
  
  observe({ ordered_colnames_route() }) # use an observe to update the reactive function above
  observe({ ordered_colnames_interval() }) # use an observe to update the reactive function above
  
  
  observeEvent(c(input$reset_graph1), {
    
    req(input$reset_graph1 >= 1)
    
    highchartProxy("output_graph_interval") %>%
      hcpxy_remove_series(id = c(ordered_colnames_route(), ordered_colnames_interval()))
    
    highchartProxy("output_graph_month") %>%
      hcpxy_remove_series(id = c(ordered_colnames_route(), ordered_colnames_interval()))
    
    updatePickerInput(
      session = session,
      inputId = "route1",
      selected = character(0))
    
    updatePickerInput(
      session = session,
      inputId = "interval1",
      selected = character(0))
    
    
    
  })
  
  observeEvent(c(input$route1), {
    
    # print(input$route)
    
    # req(input$submit >= 1)
    
    # print(tail(ordered_colnames(), 1))
    
    data1 <- data_interval[route_id == tail(ordered_colnames_route(), 1)]
    data1 <- data1[, .(velocidade = round(median(velocidade), 1)), by = .(route_id, interval)]
    setorder(data1, route_id, interval)
    
    
    highchartProxy("output_graph_interval") %>%
      # hcpxy_remove_series(id = "que") %>%
      hcpxy_add_series(data = data1, hcaes(x = interval, y = velocidade),
                       id = tail(ordered_colnames_route(), 1),
                       type = "line",
                       color = "red",
                       name = tail(ordered_colnames_route(), 1),
                       tooltip = list(pointFormat = sprintf("{series.name}: {point.y} km/h"),
                                      valueDecimals = 1)
                       
      )
    
    
    data2 <- data_month[route_id == tail(ordered_colnames_route(), 1)]
    data2 <- data2[, .(velocidade = round(median(velocidade), 1)), by = .(month)]
    setorder(data2, month)
    
    print(data1)
    
    highchartProxy("output_graph_month") %>%
      # hcpxy_remove_series(id = "que") %>%
      hcpxy_add_series(data = data2, hcaes(x = month, y = velocidade),
                       id = tail(ordered_colnames_route(), 1),
                       type = "line",
                       color = "red",
                       name = tail(ordered_colnames_route(), 1),
                       tooltip = list(pointFormat = sprintf("{series.name}: {point.y} km/h"),
                                      valueDecimals = 1)
                       
      )
    
    
  })
  
  observeEvent(c(input$interval1), {
    
    # print(input$route)
    
    # req(input$submit >= 1)
    
    # print(tail(ordered_colnames(), 1))
    
      
    data1 <- data_month[interval == tail(ordered_colnames_interval(), 1)]
      
    data1 <- data1[, .(velocidade = round(median(velocidade), 1)), by = .(month)]
    setorder(data1, month)
    
    
    highchartProxy("output_graph_month") %>%
      # hcpxy_remove_series(id = "que") %>%
      hcpxy_add_series(data = data1, hcaes(x = month, y = velocidade),
                       id = tail(ordered_colnames_interval(), 1),
                       type = "line",
                       color = "red",
                       name = tail(ordered_colnames_interval(), 1),
                       tooltip = list(pointFormat = sprintf("{series.name}: {point.y} km/h"),
                                      valueDecimals = 1)
                       
      )
    
    
  })
  
  
  
  }