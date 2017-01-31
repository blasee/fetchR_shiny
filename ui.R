shinyUI(fluidPage(
  
  titlePanel("Calculate wind fetch in New Zealand"),
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("lat", 
                   label = "Latitude", 
                   value = -36.40,
                   min = -52,
                   max = -30,
                   step = .01,
                   width = '300px'),
      
      numericInput("lon",
                   label = "Longitude",
                   value = 174.80,
                   min = 166,
                   max = 183,
                   step = .01,
                   width = '300px'),
      
      numericInput("dist",
                   label = "Maximum distance (km)",
                   value = 300,
                   min = 10,
                   max = 500,
                   step = 50,
                   width = '300px'),
      
      numericInput("n_bearings",
                   label = "Bearings per quadrant",
                   value = 9,
                   min = 1,
                   max = 90,
                   step = 1,
                   width = '300px'),
      br(),
      
      actionButton("submit", "Calculate fetch"),
      
      conditionalPanel("input.submit > 0",
                       
                       br(),
                       br(),
                       hr(),
                       textInput("file_name", "Filename:", "my_fetch"),
                       radioButtons('format', 'File format', c('CSV', 'KML', 'KMZ'),
                                    inline = TRUE),
                       downloadButton("dl_file")),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Summary", tableOutput("summary")),
        tabPanel("Distances", tableOutput("distances"))
      )
    )
  )
))
