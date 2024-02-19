# download ------------------------------------------------------------------------------------

# data
output$download_png1 <- downloadHandler(
  
  
  
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
output$download_html1 <- downloadHandler(
  
  
  
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
output$download_gpkg1 <- downloadHandler(
  
  
  
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



# download report -----------------------------------------------------------------------------

output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "report.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(n = data$start)
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

