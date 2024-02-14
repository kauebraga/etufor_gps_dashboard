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

