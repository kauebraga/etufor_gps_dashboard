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
