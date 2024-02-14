
# setup a counter for the selection mode
counter <- reactiveValues(selection = NULL)


observeEvent(c(input$selection_mode), {
  
  req(isTRUE(input$selection_mode))
  
  shinyjs::show("submit_selection1")
  shinyjs::disable("submit_selection1")
  shinyjs::show("resetar_selecao1")
  
  m <- leafletProxy("map") %>%
    clearMarkers() %>%
    clearControls() %>%
    clearShapes() %>%
    addPolylines(data = data$start,
                 color = "black",
                 weight = 2,
                 opacity = 0.8,
                 layerId = ~segment_id,
                 # label = lapply(label_segment, htmltools::HTML),
                 highlightOptions = highlightOptions(opacity = 1, weight = 10, color = "#F07D17")) %>%
    addCircleMarkers(data = stops_unique,  
                     stroke = FALSE, fillOpacity = 0.5,
                     radius = 2,
                     color = "black",
                     group = "Paradas")
  
  
  m
  
  
})


# first, we should create a vector with the selected elements
element <- reactiveValues(selected = NULL,
                          selected1 = NULL,
                          indicator = NULL,
                          selection = NULL)

observeEvent(c(input$selection_mode), {
  
  element$selection <- 1
  
}, priority = 1000, ignoreInit = TRUE)


# when the selection_mode changed, the vector should be restarted to avoid duplciation
observeEvent(c(input$selection_mode), {
  
  
  req(isFALSE(input$selection_mode), element$selection == 1)
  
  
  shinyjs::hide("submit_selection1")
  shinyjs::hide("resetar_selecao1")
  
  
  
  element$selected <- NULL
  
  
  # restore the map
  pal <- colorNumeric(
    palette = "RdYlBu",
    domain = data$start$velocidade)
  
  # label for clicking on the segment
  label_segment <- paste0("<b>Velocidade:</b>", round(data$start$velocidade, 1), " km/h")
  
  
  m <- 
    leafletProxy("map", data = data$start) %>%    
    clearMarkers() %>%
    clearControls() %>%
    clearShapes() %>%
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
  
  m
  
  
})


observeEvent(c(input$map_shape_click), {
  
  req(isTRUE(input$selection_mode))
  
  shinyjs::enable("submit_selection1")
  
  ui <- input$map_shape_click$id
  
  element$selected <- c(element$selected, ui)
  
  # # as you select a segment, it should filter only for those segments available - DISABLE THIS FOR NOW
  # print(data$start)
  # 
  # 
  # print(input$map_shape_click$id)
  # 
  # a <- subset(trechos_possiveis, grepl(pattern = paste0(input$map_shape_click$id, "-"), x = segments))
  # a <- strsplit(a$segments, input$map_shape_click$id)  
  # # take the second element, which is the one after the segment you just selected
  # b <- lapply(a, function(x) sub(pattern = "^-", replacement = "", x = x[[2]]))
  # # compor segmentos
  # c <- lapply(b, function(x) strsplit(x, "-")) 
  # d <- lapply(c, as.data.table)
  # e <- lapply(d, function(x) x[, uh := paste0(V1, "-", shift(V1, -1))])
  # f <- rbindlist(e)
  # g <- unique(f$uh)
  # 
  # # filter those segments
  # data_new <- subset(data$start, segment_id %in% g)
  
  # display the segments that can be selected
  
  # after you select the first segment, you should be able to select multiple segments and keep them selected
  
  data <- subset(data$start, segment_id == ui)
  
  m <- leafletProxy("map") %>%
    removeShape(layerId = ui) %>%
    addPolylines(data = data,
                 color = "#F07D17",
                 weight = 10,
                 opacity = 0.8,
                 layerId = ~segment_id,
                 # label = lapply(label_segment, htmltools::HTML),
                 highlightOptions = highlightOptions(opacity = 1, weight = 10, color = "#F07D17")) 
  
  
  m
  
  print(element$selected)
  
  
  
  
  
  
})



# observer to filter the selection ------------------------------------------------------------

observeEvent(c(input$submit_selection), {
  
  req(input$submit_selection >= 1)
  
  # filter the data related to the segments
  if (input$submit == 0) {
    
    data_go <- as.data.table(st_set_geometry(data$start, NULL))
    
    data_ok <- data_go[segment_id %in% element$selected]
    # print(data_ok)
    
    info$segment_id <- data_ok$segment_id
    info$speed <- mean(data_ok$velocidade)
    
    # extracts stop from segments
    stops_segments <- unlist(strsplit(data_ok$segment_id, "\\-"))
    # get stop names
    
    stop_go <- as.data.table(st_set_geometry(stops_routes, NULL))
    stops_ok <- unique(with(stop_go, stop_name[match(stops_segments, stop_id)]))
    
    # info$stop_name_initial <- stops_ok[1]
    # info$stop_name_end <- stops_ok[2]
    info$stops <- unique(stops_ok)
    
    # calcular as linhas servidas
    linhas <- unique(stop_go[stop_id == stops_segments[1] & shift(stop_id, 1, type = "lead") == stops_segments[2]]$shape_id)
    linhas <- gsub(pattern = "shape", replacement = "", x = linhas)
    info$linhas <- linhas
    
    
    # by interval
    data$interval <- segments_variables[segment_id %in% element$selected, .(velocidade = mean(velocidade)), by = c("interval")]
    
    
  } else {
    
    
    data_go <- as.data.table(st_set_geometry(data$segments, NULL))
    
    data_ok <- data_go[segment_id %in% element$selected]
    
    info$segment_id <- data_ok$segment_id
    info$speed <- data_ok$velocidade
    
    # extracts stop from segments
    stops_segments <- unlist(strsplit(data_ok$segment_id, "\\-"))
    
    # get stop names
    stop_go <- as.data.table(st_set_geometry(stops_routes, NULL))
    # print(stop_go)
    stops_ok <- unique(with(stop_go, stop_name[match(stops_segments, stop_id)]))
    # print(stops_ok)
    
    info$stop_name_initial <- stops_ok[1]
    info$stop_name_end <- stops_ok[2]
    
    # calcular as linhas servidas
    linhas <- unique(stop_go[stop_id == stops_segments[1] & shift(stop_id, 1, type = "lead") == stops_segments[2]]$shape_id)
    print(linhas)
    linhas <- gsub(pattern = "shape", replacement = "", x = linhas)
    print(linhas)
    info$linhas <- linhas
    
  }
  
  
  print("po")
  # nav_select(
  #   id = "navbar1",
  #   selected = c("map"))
  
  # updateTabsetPanel(session, inputId = 'navbar1', selected = 'map')
  # updateTabsetPanel(session, inputId = 'map', selected = 'tab_trechos')
  # updateTabsetPanel(session, inputId = 'mapnavbar1', selected = 'tab_trechos')
  
  # updateNavbarPage(
  #   session = getDefaultReactiveDomain(),
  #   inputId = "navbar1",
  #   selected = "graphs"
  # )
  updateTabsetPanel(inputId = 'tab-9191-4', selected = 'tab_trechos')
  
  
})


# graph for speed
output$output_graph_speed_segments <- renderHighchart({
  
  col_stops <- data.frame(
    q = c(0.3, 0.5, 0.6),
    c = c('#DF5353', '#DDDF0D', '#55BF3B'),
    stringsAsFactors = FALSE
  )
  
  
  highchart() %>%
    hc_chart(type = "solidgauge") %>%
    hc_pane(
      startAngle = -90,
      endAngle = 90,
      background = list(
        outerRadius = '100%',
        innerRadius = '60%',
        shape = "arc"
      )
    ) %>%
    hc_tooltip(enabled = FALSE) %>% 
    hc_yAxis(
      stops = list_parse2(col_stops),
      lineWidth = 0,
      minorTickWidth = 0,
      tickAmount = 2,
      min = 0,
      max = 30,
      labels = list(y = 26, style = list(fontSize = "22px"))
    ) %>%
    hc_add_series(
      data = round(info$speed, 2),
      dataLabels = list(
        y = -50,
        borderWidth = 0,
        useHTML = TRUE,
        # style = list(fontSize = "40px"),
        format = '<div style="text-align:center"><span style="font-size:25px">{y}</span><br/><span style="font-size:12px;opacity:0.4">km/h</span></div>'
      )
    ) %>% 
    hc_size(height = 300)
  
  
  
})

# graph for interval
output$output_graph_interval_segments <- renderHighchart({
  
  # graphs
  # data_month_all <- readRDS("data/graphs/graphs_month_all.rds")
  # data_month <- readRDS("data/graphs/graphs_month.rds")
  
  setorder(data$interval, interval)
  
  highchart() %>%
    
    hc_add_series(data = data$interval,
                  hcaes(x = interval, y = velocidade),
                  type = "area",
                  color = "#F7B93B",
                  # lineWidth = 5,
                  # opacity = 0.5,
                  name = "Todas",
                  tooltip = list(pointFormat = sprintf("{series.name}: {point.y} km/h"),
                                 valueDecimals = 1)
    ) %>%
    
    # hc_title(text = "Velocidade media por intervalo de 15 minutos") %>%
    
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


# resetar selecao

observeEvent(c(input$resetar_selecao), {
  
  req(isTRUE(input$selection_mode), element$selection == 1)
  
  element$selected <- NULL
  
  m <- leafletProxy("map") %>%
    clearMarkers() %>%
    clearControls() %>%
    clearShapes() %>%
    addPolylines(data = data$start,
                 color = "black",
                 weight = 2,
                 opacity = 0.8,
                 layerId = ~segment_id,
                 # label = lapply(label_segment, htmltools::HTML),
                 highlightOptions = highlightOptions(opacity = 1, weight = 10, color = "#F07D17")) %>%
    addCircleMarkers(data = stops_unique,  
                     stroke = FALSE, fillOpacity = 0.5,
                     radius = 2,
                     color = "black",
                     group = "Paradas")
  
  
  m
  
})




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


output$info_paradas_n <- renderUI({
  
  
  
  tagList(
    div(style = "font-family: Encode Sans; font-size: 90px; font-weight: 700; line-height: 113px; letter-spacing: 0em; text-align: center;",  
        length(info$stops)),
    div(style = "font-family: Encode Sans; font-size: 18px; font-weight: 500; line-height: 30px; letter-spacing: 0em; text-align: center;",
        "pontos de ônibus no trecho")
  )
  
})

output$info_paradas <- renderUI({
  
  
  tagList(
    div(style = "font-family: Encode Sans;font-size: 14px;font-weight: 400;line-height: 25px;letter-spacing: 0em;text-align: left; overflow-y: scroll; max-height: 150px",
        HTML("<ul>"),
        HTML(paste0("<li>", info$stops, "</li>")),
        HTML("</ul>")
    ))
  
  # # gather info
  # value_box(
  #   title = "Paradas do trecho",
  #   HTML(paste0(info$stops, collapse= "<br>")),
  #   # HTML(paste0(info$stop_name_initial, "<br>", info$stop_name_end)),
  #   showcase = bsicons::bs_icon("sign-stop"),
  #   theme_color = "secondary"
  #   # showcase = bsicons::bs_icon("align-bottom")
  # )
  
  
  
  
})

output$info_linhas <- renderUI({
  
  # # gather info
  # value_box(
  #   title = "Linhas servidas",
  #   HTML(paste(info$linhas, collapse = " /// ")),
  #   showcase = bsicons::bs_icon("bus-front"),
  #   theme_color = "secondary"
  #   # showcase = bsicons::bs_icon("align-bottom")
  # )
  
  # idenfy the names
  routes <- subset(routes_shapes, shape_id %in% info$linhas)
  
  sentidos <- unique(routes$direction)
  
  ida <- if ("I" %in% sentidos) {
    
    routes_ok <-subset(routes, direction == "I")
    routes_ok <- paste0(routes_ok$route_id, " - ", routes_ok$route_long_name)
    
    a <- tagList(
      span("Sentido: Ida", style = "font-family: Encode Sans; font-size: 20px; font-weight: 500; line-height: 25px; letter-spacing: 0em; text-align: left;"),
      HTML("<ul>"),
      HTML(paste0("<li style = font-size: 14px>", routes_ok,"</li>")),
      HTML("</ul>")
    )
    
  } else ""
  
  volta <- if ("V" %in% sentidos) {
    
    routes_ok <-subset(routes, direction == "V")
    routes_ok <- paste0(routes_ok$route_id, " - ", routes_ok$route_long_name)
    
    tagList(
      span("Sentido: Volta", style = "font-family: Encode Sans; font-size: 20px; font-weight: 500; line-height: 25px; letter-spacing: 0em; text-align: left;"),
      HTML("<ul>"),
      HTML(paste0("<li style = font-size: 14px>", routes_ok, "</li>")),
      HTML("</ul>")
    )
    
  } else ""
  
  
  tagList(
    div(
      ida,
      volta
      
    ))
  
  
})

