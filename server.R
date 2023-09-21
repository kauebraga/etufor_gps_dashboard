

# open data
segments_sf <- setDT(readRDS("data/segments_gtfs_unique.rds"))
segments_speeds <- setDT(readRDS("data/gps_by_segment_unique.rds"))
segments_variables <- setDT(readRDS("data/gps_by_segment_variables.rds"))
stops_routes <- readRDS("data/stops_gtfs_routes_sf.rds")


server <- function(input, output) {
  
  
  # set data
  data <- reactiveValues(segments = NULL,
                         stops = NULL)
  
  output$map <- renderLeaflet({
    
    
    # join the segment data with the sf
    segments_data <- merge(segments_speeds, segments_sf)
    segments_data <- sf::st_sf(segments_data, crs = 4326)
    
    data$segments <- segments_data
    
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = segments_data$velocidade)
    
    # label for clicking on the segment
    label_segment <- paste0("<b>Velocidade:</b>", round(segments_data$velocidade, 1), " km/h")
    
    
    map <- leaflet(data = segments_data, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light", layerId = "epa") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      addPolylines(color = ~pal(velocidade),
                   weight = 2,
                   opacity = 0.8,
                   label = lapply(label_segment, htmltools::HTML)) %>%
      
      addLayersControl(baseGroups = c("Light", "Dark", "Satellite"),
                       # overlayGroups = c("Overlay"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "bottomright") %>%
      addLegend("bottomright", pal = pal, values = ~velocidade,
                title = "Velocidade"
      )
      # setView(lng = 0, lat = 0, zoom = 2) 
    
    
  })
  
  
  # filter speed
  observeEvent(c(input$submit), {
    
    req(input$submit >= 1)
    
    # print(input$velocidade_maxima)
    # print(input$route)
    # print(input$interval)
    
    if (is.null(input$route)) {
      
      segments_data <- segments_variables[velocidade <= input$velocidade_maxima & interval %in% input$interval]
      
    } else if (is.null(input$interval)) {
      
      segments_data <- segments_variables[velocidade <= input$velocidade_maxima & route_id %in% input$route]
      stops <- subset(stops_routes, route_id %in% input$route)
      
    } else if (is.null(input$route) & is.null(input$interval)) {
      
      segments_data <- segments_variables[velocidade <= input$velocidade_maxima]
      
    } else {
      
      
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
    
    # label for clicking on the segment
    label_segment <- paste0("<b>Velocidade:</b>", round(data$segments$velocidade, 1), " km/h")
    
    map <- leafletProxy("map", data = data$segments) %>%
      clearMarkers() %>%
      clearControls() %>%
      clearShapes() %>%
      addPolylines(color = ~pal(velocidade),
                   weight = 3,
                   opacity = 0.8,
                   label = lapply(label_segment, htmltools::HTML)) %>%
      addCircleMarkers(data = data$stops,  
                       stroke = FALSE, fillOpacity = 0.5,
                       radius = 3,
                       color = "black") %>%
      addLegend("bottomright", pal = pal, values = ~velocidade,
                title = "Velocidade"
      )
    
    
    if (!is.null(input$route)) {
      
      
      bbox <- sf::st_bbox(sf::st_transform(data$segments, 4326))
      
      
      
      map <- map %>%
        flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
      
    }
    
    
    
    map
    
  })
  
  
  
  
  
  

# download ------------------------------------------------------------------------------------

  # data
  output$download <- downloadHandler(
    
    # generate button with data
    filename = function() {
      
      
      sprintf("data_%s_%s.gpkg", "", "")
      
    },
    content = function(file) {
      
      sf::st_write(data$segments, file)
      
    }
    
  )  
  
  
  }