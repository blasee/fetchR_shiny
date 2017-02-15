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
      
      hr(),
      helpText("Download the data in CSV, KML or KMZ format for use in other software."),
      textInput("file_name", "Filename:", "my_fetch"),
      radioButtons('format', 'File format', c('CSV', 'KML', 'KMZ'),
                   inline = TRUE),
      conditionalPanel("input.submit < 1",
                       downloadButton("dl_file_1")),
      conditionalPanel("input.submit > 1",
                     downloadButton("dl_file")),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 conditionalPanel("input.submit < 1",
                                  plotOutput("plot_1")),
                 conditionalPanel("input.submit > 0",
                                  plotOutput("plot")),
                 h3("What is fetch?"),
                 HTML('<p>Fetch is the unobstructed length of water over which 
wind can blow. It is also commonly used as a measure of wind and wave exposure 
at coastal sites, whereby a large fetch in a certain direction results in a 
higher exposure to wind and waves from that direction  (see the 
                      <a href="http://en.wikipedia.org/wiki/Fetch_(geography)">associated wikipedia page</a>).</p>'),
                 h3("How does this work?"),
                 p("This web application calculates the fetch for any coastal 
site around New Zealand using the",
                   a("fetchR", href = "https://github.com/blasee/fetchR#calculate-average-fetch-for-any-coastal-area-around-new-zealand"), "R package. A coastal location is given in latitude and longitude coordinates, along with a maximum distance and the number of bearings to be calculated per 90 degrees (the default is to calculate 9 bearings per 90 degrees, or equivalently, one per 10 degrees). To calculate the fetch, the lat-lon coordinates are projected onto the", a("NZTM 2000 map projection", href = "http://www.linz.govt.nz/data/geodetic-system/datums-projections-and-heights/projections/new-zealand-transverse-mercator-2000"), "and the fetch is calculated for each requested direction."),
                   
                 p("The", strong("Plot"), "tab shows a plot of the vectors that were used in calculating the fetch for each bearing."),
                 p("The", strong("Summary"), "tab gives a summary of the fetch for the coastal location, including the average fetch for each quadrant. The more angles used per quadrant will lead to better estimates of fetch, although the computation time will increase."), 
                 p("The", strong("Distances"), "tab contains the fetch length for each bearing vector that have gone into the fetch calculations, along with the lat-lon coordinates indicating the point at which they 'hit' land or reach their maximum distance.")
                 ),
        tabPanel("Summary", 
                 conditionalPanel("input.submit < 1",
                                  tableOutput("summary_1")),
                 tableOutput("summary")),
        tabPanel("Distances", 
                 conditionalPanel("input.submit < 1",
                                  dataTableOutput("distances_1")),
                 dataTableOutput("distances"))
      )
    )
  )
))
