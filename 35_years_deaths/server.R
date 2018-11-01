source("global.R", local = TRUE)

shinyServer(function(input, output, session){
  
  output$deathRateMap <- renderPlot({
    plotMap(input$state, input$year, input$disease)
    
  })
  output$deathRateUSMap <- renderPlot({
    plotUSMap(input$disease, input$year)
    
  })
  
  # output$deathRateCountyMap <- renderPlot({
  #   plotCountyMap(input$disease, input$year)
  #   
  # })
  output$deathRateTopCountiese <- renderPlot({
    plotTopTenCounties(input$state, input$disease, input$year)
    
  })
  output$stateDeathRate <- renderPlot({
    plotStatewiseDeathRate(input$disease, input$state)
    
  })
  
})