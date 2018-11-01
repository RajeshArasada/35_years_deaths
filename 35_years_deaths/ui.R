shinyUI(fluidPage(
  list(tags$head(HTML('<link rel = "icon", href = "https://nycdatascience.com",
                      type = "image/png"/>'))),
  # padding is very important to fit the logo
  div(style = "padding:0px 0px; width: '100%",
      titlePanel(
        title = "35 Years Of American Death", windowTitle = "How Good is your Neighborhood"
      )),
  addDeps(
    tags$body(
      navbarPage(theme = shinytheme("readable"),
                 title = div(
                   div(
                     img(src = "nyc_logo.png", width = 100, height = 100),
                     style = "position: fixed; right: 5px; top: 0px" 
                   )
                 ),
                 fluidRow(
                   column(4, 
                          box(width = NULL, status = "success", title = "Neghborhood Details",
                              selectInput("disease", "Disease Name: ",
                                          selected = "Cardiovascular_diseases",
                                          choices = as.list(allDiseases),
                                          multiple = FALSE
                                          ),
                              selectInput("state", "State: ",
                                          selected = "alabama",
                                          choices = as.list(allStates),
                                          multiple = FALSE
                                          ),
                              br(),
                              sliderInput("year", "Year: ",
                                          min = 1980,
                                          max = 2014,
                                          value = 1980,
                                          step = 5
                                          )
                              )
                          ),
                   column(4, box(width = NULL, status = "success", title = "",
                                 plotOutput("deathRateMap", height = "400px", width = "400px")),
                          box(width = NULL, status = "success", title = "",
                              plotOutput("deathRateTopCountiese", height = "400px", width = "400px"))
                          
                          ),
                   column(4, box(width = NULL, status = "success", title = "",
                                 plotOutput("deathRateUSMap", height = "400px", width = "400px")
                                 ),
                          
                          box(width = NULL, status = "success", title = "",
                                 # plotOutput("deathRateCountyMap", height = "400px", width = "400px")
                                 plotOutput("stateDeathRate", height = "400px", width = "400px")
                              )
                          )
                   )
                 )
                 )
    )
  )
)