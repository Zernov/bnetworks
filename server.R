library(shiny)
library(bnlearn)
library(Rgraphviz)
library(shinydashboard)

shinyServer(function(input, output) {
  
  output$selectLearningUI <- renderUI({
    switch (input$selectLearning,
            "Data file" = {},
            "Bif file" = fileInput("uploadBif", "Choose .bif file", accept=c(".bif")),
            "Example: asia" = downloadButton("downloadLearning", "Download")
    )
  })
  
  output$downloadLearning <- downloadHandler(
    filename = function() {
      paste('asia.csv', sep = '')
    },
    content = function(file) {
      write.csv(file, asia)
    }
  )
  
  output$plotLearning <- renderPlot({
    switch (input$selectLearning,
            "Data file" = {},
            "Bif file" = {},
            "Example: asia" = {
              net <- hc(asia)
              arcs(net) <- c(arcs(net), c("A", "T"))
              fitted <- bn.fit(net, asia)
              graphviz.plot(fitted)
            }
    )
  })
  
  # 
  # output$tableRlibs <- renderTable({
  #   tableRlibs <- read.table("Rlibs.txt", row.names = 1, header = TRUE)
  # }, align='rcccccc')
  
})