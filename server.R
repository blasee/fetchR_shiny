options(shiny.maxRequestSize = 50 * 1024^2)

shinyServer(function(input, output) {

  rvs = reactiveValues(show_button = FALSE)

  observe({
    rvs$show_button = all(
      !is.null(input$polygon_shape),
      !is.null(input$point_shape),
      !is.null(input$n_dirs),
      !is.null(input$dist)
    )
  }
  )

  observeEvent(input$n_dirs,
               {
                 if (all(!is.null(input$polygon_shape),
                         !is.null(input$point_shape))){
                   enable("submit")
                   rvs$show_button = TRUE
                 }
               })

  observeEvent(rvs$show_button,
               {
                 if (rvs$show_button)
                   enable("submit")
                 else
                   disable("submit")
               })

  observeEvent(input$submit,
               {
                 disable("submit")
                 calc_fetch()
                 rvs$show_button = FALSE
               })

  polyShapeInput = reactive({
    inFile <- input$polygon_shape

    if (is.null(inFile))
      return(NULL)

    validate(need(any(grepl("\\.shp$", inFile$name)),
                  "Please include a shape format file (.shp)."))

    validate(need(any(grepl("\\.prj$", inFile$name)),
                  "Please include a projection format file (.prj)."))

    validate(need(any(grepl("\\.shx$", inFile$name)),
                  "Please include a shape index format file (.shx)."))

    # Names of the uploaded files
    infiles = inFile$datapath

    # Directory containing the files
    dir_name = unique(dirname(inFile$datapath))

    # New names for the files (matching the input names)
    outfiles = file.path(dir_name, inFile$name)
    walk2(infiles, outfiles, ~file.rename(.x, .y))

    x <- try(readOGR(dir_name, strsplit(inFile$name[1], "\\.")[[1]][1]), TRUE)

    validate(need(class(x) != "try-error", "Could not read shapefile."))

    validate(need(is(x, "SpatialPolygons"),
                  "Please provide a 'Polygon' shapefile."))

    validate(need(is.projected(x),
                  "Please project the shapefile onto a suitable map projection."))
    list(x = x,
         dir_name = tail(strsplit(dir_name, "/")[[1]], 1))
  })

  pointShapeInput = reactive({
    inFile <- input$point_shape

    if (is.null(inFile))
      return(NULL)

    validate(need(any(grepl("\\.shp$", inFile$name)),
                  "Require a shape format (.shp)."))

    validate(need(any(grepl("\\.prj$", inFile$name)),
                  "Require a projection format (.prj)."))

    validate(need(any(grepl("\\.shx$", inFile$name)),
                  "Require a shape index format (.shx)."))

    # Names of the uploaded files
    infiles = inFile$datapath

    # Directory containing the files
    dir_name = unique(dirname(inFile$datapath))

    # New names for the files (matching the input names)
    outfiles = file.path(dir_name, inFile$name)
    walk2(infiles, outfiles, ~file.rename(.x, .y))

    x <- try(readOGR(dir_name, strsplit(inFile$name[1], "\\.")[[1]][1]), TRUE)

    validate(need(class(x) != "try-error", "Could not read shapefile."))

    validate(need(is(x, "SpatialPoints"),
                  "Please provide a '[Multi]Point' shapefile."))
    
    validate(need(length(x) < 101,
                  "Please provide up to 100 sites to calculate."))
    list(x = x,
         dir_name = tail(strsplit(dir_name, "/")[[1]], 1))
  })

  output$polygon_map <- renderPlot({

    poly_layer = polyShapeInput()$x
    point_layer = pointShapeInput()$x

    if (is.null(input$polygon_shape) &
        input$submit == 0)
      return(NULL)

    if (is.null(input$point_shape)){

      plot(poly_layer, border = NA, col = "lightgrey")

    } else {

      # If both projected, test for the same map projections here...

      if (all(is.projected(poly_layer),
              !is.projected(point_layer)))
        point_layer = spTransform(point_layer,
                                  CRS(proj4string(poly_layer)))

      validate(need(all(is.na(over(poly_layer, point_layer))),
                    "At least one site location is on land"))

      plot(point_layer, col = "red")
      plot(poly_layer, add = TRUE, border = NA, col = "lightgrey")
    }
  })

  calc_fetch = eventReactive(input$submit, {

    poly_layer = polyShapeInput()$x
    point_layer = pointShapeInput()$x

    validate(need(all(input$n_dirs <= 20,
                      input$n_dirs > 0),
                  "Directions per quadrant: please choose a number between 1 and 20."))
    
    withProgress(message = "Calculating fetch", detail = "", value = 0, {
      
      if (any(grepl("^[Nn]ames{0,1}$", names(point_layer)))) {
        name_col = grep("^[Nn]ames{0,1}$", names(point_layer))
        site_names = as.character(data.frame(point_layer)[, name_col[[1]]])
      } else {
        site_names = paste("Site", seq_along(point_layer))
      }
      
      which_proj = c(is.projected(poly_layer), is.projected(point_layer))
      
      if (all(which_proj) && !identicalCRS(poly_layer, point_layer)) {
        point_layer = spTransform(point_layer, CRS(proj4string(poly_layer)))
      }
      
      if (!which_proj[2]) {
        point_layer = spTransform(point_layer, CRS(proj4string(poly_layer)))
      }
      
      max_dist = input$dist * 1000
      directions = head(seq(0, 360, by = 360 / (input$n_dirs * 4)), -1)
      dirs = as.numeric(directions)
      dirs_bin = findInterval(dirs, seq(45, 315, by = 90))
      quadrant = rep("North", length(dirs))
      quadrant[dirs_bin == 1] = "East"
      quadrant[dirs_bin == 2] = "South"
      quadrant[dirs_bin == 3] = "West"
      directions = unlist(split(directions, directions < 90), use.names = FALSE)
      fetch_list = vector("list", length(point_layer))
      
      inc = 1 / length(point_layer)
      for (i in seq_along(point_layer)){
        
        d_bff = gBuffer(point_layer[i, ], width = max_dist, quadsegs = input$n_dirs)
        fetch_ends = head(coordinates(d_bff@polygons[[1]]@Polygons[[1]]), 
                          -1)
        fetch_ends = fetch_ends[order(directions), ]
        line_list = fetchR:::create_line_list(point_layer[i, ], fetch_ends)
        fetch_sp_lines = fetchR:::create_sp_lines(line_list, sort(directions), 
                                         poly_layer)
        poly_layer_subset = poly_layer[which(!is.na(over(poly_layer, 
                                                            fetch_sp_lines))), ]
        if (length(poly_layer_subset) > 0) {
          hit_land = !sapply(gIntersects(fetch_sp_lines, poly_layer_subset, 
                                         byid = c(TRUE, FALSE), returnDense = FALSE), 
                             is.null)
          ints = gIntersection(fetch_sp_lines[hit_land], poly_layer_subset, 
                               byid = c(TRUE, FALSE))
          fetch_ends[hit_land, ] = t(sapply(ints@lines, function(x) {
            coordinates(x)[[1]][1, ]
          }))
          line_list = fetchR:::create_line_list(point_layer[i, ], fetch_ends)
          fetch_sp_lines = fetchR:::create_sp_lines(line_list, sort(directions), 
                                           poly_layer)
        }
        fetch.df = data.frame(site = site_names[i], fetch = SpatialLinesLengths(fetch_sp_lines)/1000, 
                              direction = sort(directions), quadrant = factor(quadrant, 
                                                                              levels = c("North", "East", "South", "West")))
        fetch_list[[i]] = SpatialLinesDataFrame(fetch_sp_lines, 
                                                fetch.df)
        
        incProgress(inc, "Calculating fetch", paste0("Finished ", site_names[i], 
                                                     " (", round(i / length(site_names) * 100), "%)"))
      }
      
      my_fetch = new("Fetch", fetch_list, names = site_names, 
                     max_dist = max_dist / 1000)
      
      setProgress(1)
      
      list(my_fetch = my_fetch,
           my_fetch_latlon = spTransform(my_fetch, CRS("+init=epsg:4326")))
    })

    # withCallingHandlers({
    #   html("text", "")
    #   my_fetch = fetch(poly_layer,
    #                    point_layer,
    #                    max_dist = input$dist,
    #                    n_directions = input$n_dirs,
    #                    quiet = TRUE)
    #   message("")
    # },
    # message = function(m){
    #   emph_text = paste0("<strong>", m$message, "</strong>")
    #   html(id = "text", html = emph_text)
    # })

    
  })

  output$fetch_plot = renderPlot({
    plot(calc_fetch()$my_fetch, polyShapeInput()$x)
  })

  output$summary = renderTable({
    poly_layer = polyShapeInput()$x
    point_layer = pointShapeInput()$x

    if (is.null(input$polygon_shape) &
        input$submit == 0)
      return(NULL)

    summary(calc_fetch()$my_fetch)
  },
  rownames = TRUE, colnames = TRUE)

  output$distances = DT::renderDataTable({
    poly_layer = polyShapeInput()$x
    point_layer = pointShapeInput()$x

    if (is.null(input$polygon_shape) &
        input$submit == 0)
      return(NULL)

    calc_fetch.df = as(calc_fetch()$my_fetch_latlon, "data.frame")
    class(calc_fetch.df$direction) = "numeric"
    calc_fetch.df
  })

  output$dl_file = downloadHandler(
    filename = function(){
      paste0(strsplit(input$file_name, ".", fixed = TRUE)[[1]][1],
             switch(input$format,
                    CSV = ".csv",
                    KML = ".kml",
                    R = ".zip"
             ))
    },
    content = function(file){
      switch(input$format,
             CSV = write.csv(as(calc_fetch()$my_fetch_latlon, "data.frame"), file, row.names = FALSE),
             KML = kml(calc_fetch()$my_fetch_latlon, file.name = file),
             R = create_zip(
               polyShapeInput()$x,
               pointShapeInput()$x,
               head(strsplit(input$polygon_shape$name, ".", fixed = TRUE)[[1]], -1),
               head(strsplit(input$point_shape$name, ".", fixed = TRUE)[[1]], -1),
               polyShapeInput()$dir_name,
               pointShapeInput()$dir_name,
               input$dist, input$n_dirs, file))
    }
  )
})
