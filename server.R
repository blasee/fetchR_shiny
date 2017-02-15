library(fetchR)
library(sp)

load("data/init.RData")

shinyServer(function(input, output) {
  
  xx = eventReactive(input$submit, {
    fetch(input$lat,
             input$lon,
             max_dist = input$dist,
             n_bearings = input$n_bearings)
    })
  
  output$plot_1 = renderPlot({
    plot(init_fetch_obj, col = "red")
  })
  
  output$plot = renderPlot({
    plot(xx(), col = "red")
    })
  
  output$summary_1 = renderTable(my_fetch.df,
                                 rownames = TRUE, colnames = FALSE)
  
  output$summary = renderTable({
    angles = as.numeric(sapply(slot(xx(), "lines"), slot, "ID"))
    angles_bin = findInterval(angles, seq(45, 315, by = 90))
    angles_bin[angles_bin == 4] = 0
    
    data.frame(Fetch = c(paste0(round(mean(SpatialLinesLengths(xx())/1000), 2), "km"),
                         paste0(round(median(SpatialLinesLengths(xx())/1000), 2), "km"),
                         paste0(round(mean(SpatialLinesLengths(xx())[angles_bin == 0] / 1000), 2), "km"),
                         paste0(round(mean(SpatialLinesLengths(xx())[angles_bin == 1] / 1000), 2), "km"),
                         paste0(round(mean(SpatialLinesLengths(xx())[angles_bin == 2] / 1000), 2), "km"),
                         paste0(round(mean(SpatialLinesLengths(xx())[angles_bin == 3] / 1000), 2), "km")),
               row.names = c("Average fetch:",
                             "Median fetch:",
                             "Average northerly fetch [315, 45):",
                             "Average easterly fetch [45, 135):",
                             "Average southerly fetch [135, 225):",
                             "Average westerly fetch [225, 315):"))
  }, 
  rownames = TRUE, colnames = FALSE)
  
  output$distances_1 = renderDataTable(dist.df)
  
  output$distances = renderDataTable({
    xx.df = as(xx(), "data.frame")
    class(xx.df$direction) = "integer"
    xx.df
  })
  
  output$dl_file_1 = downloadHandler(
    filename = function(){
      paste0(strsplit(input$file_name, ".", fixed = TRUE)[[1]][1],
             switch(input$format,
                    CSV = ".csv", 
                    KML = ".kml", 
                    KMZ = ".kmz"
                    ))
    },
    content = function(file){
      switch(input$format,
             CSV = write.csv(dist.df, file, row.names = FALSE),
             KML = plotKML::kml(init_fetch_obj, file.name = file),
             KMZ = plotKML::kml(init_fetch_obj, file.name = file, kmz = TRUE))
    }
  )
  
  output$dl_file = downloadHandler(
    filename = function(){
      paste0(strsplit(input$file_name, ".", fixed = TRUE)[[1]][1], 
             switch(input$format, 
                    CSV = ".csv", KML = ".kml", KMZ = ".kmz"
      ))
    },
    content = function(file){
      switch(input$format,
             CSV = write.csv(as(xx(), "data.frame"), file, row.names = FALSE),
             KML = plotKML::kml(xx(), file.name = file),
             KMZ = plotKML::kml(xx(), file.name = file, kmz = TRUE))
    }
  )
})