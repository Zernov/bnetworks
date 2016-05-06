library(shiny)
library(bnlearn)
library(Rgraphviz)
library(shinydashboard)

shinyServer(function(input, output) {
  
  output$selectLearningUI <- renderUI({
    switch (input$selectLearning,
            "Data: .csv file" = {fileInput("uploadCsv", "Choose .csv file", accept=c(".csv"))},
            "Import: .bif file" = fileInput("uploadBif", "Choose .bif file", accept=c(".bif")),
            "Import: .dsc file" = fileInput("uploadDsc", "Choose .dsc file", accept=c(".dsc")),
            "Import: .net file" = fileInput("uploadNet", "Choose .net file", accept=c(".net")),
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
            "Data: .csv file" = {
              if (!is.null(input$uploadCsv)) {
                data <- read.csv(input$uploadCsv$datapath, row.names = 1)
                net <- hc(data)
                write.csv(data, file = "data")
                write(names(data), "dataNames")
                write(arcs(data), "dataArcs")
                graphviz.plot(net)
              }
            },
            "Import: .bif file" = {
              if (!is.null(input$uploadBif)) {
                fitted <- read.bif(input$uploadBif$datapath)
                graphviz.plot(fitted)
              }
            },
            "Import: .dsc file" = {
              if (!is.null(input$uploadDsc)) {
                fitted <- read.dsc(input$uploadDsc$datapath)
                graphviz.plot(fitted)
              }
            },
            "Import: .net file" = {
              if (!is.null(input$uploadNet)) {
                fitted <- read.net(input$uploadNet$datapath)
                graphviz.plot(fitted)
              }
            },
            "Example: asia" = {
              net <- hc(asia)
              arcs(net) <- c(arcs(net), c("A", "T"))
              fitted <- bn.fit(net, asia)
              graphviz.plot(fitted)
            }
    )
  })
  
  output$learningAddArcsInputUI <-renderUI({
    if (input$selectLearning == "Data: .csv file") {
      if (!is.null(input$uploadCsv)) {
        dataNames <- scan("dataNames", what = character())
        learningAddArcsInputUI <- fluidRow(
          column(6,
                 selectInput("learningAddArcsFrom", c(names(data)), selected = names(data)[1])),
          column(6,
                 selectInput("learningAddArcsTo", c(names(data)), selected = names(data)[1]))
        )
      }
    }
  })
  
  output$learningAddArcsButtonUI <-renderUI({
    if (input$selectLearning == "Data: .csv file") {
      if (!is.null(input$uploadCsv)) {
        actionButton("learningAddArcsButton", "Add")
      }
    }
  })
  
  # 
  # output$tableRlibs <- renderTable({
  #   tableRlibs <- read.table("Rlibs.txt", row.names = 1, header = TRUE)
  # }, align='rcccccc')
  
})