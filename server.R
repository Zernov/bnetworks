library(shiny)
library(bnlearn)
library(Rgraphviz)
library(shinydashboard)

shinyServer(function(input, output) {
  
  output$arcsFromManuallyUI <- renderUI({
    plotManually <- renderPlot({
      graph <- empty.graph(as.character(c(1:input$nodesManually)))
    })
    arcsFromManuallyUI <- selectInput("arcsManuallyFrom", "From", c(1:input$nodesManually))
  })
  
  output$arcsToManuallyUI <- renderUI({
    selectInput("arcsManuallyTo", "To", c(1:input$nodesManually))
  })
  
  nodesManually <- eventReactive(input$nodesManually, {
    write(as.character(c(1:input$nodesManually)), "graphManuallyNodes.txt")
    write("","graphManuallyArcs.txt")
  })
  
  arcsManually <- eventReactive(input$arcsAddManually, {
    nodes <- scan(file = "graphManuallyNodes.txt", what = character())
    arcs <- scan(file = "graphManuallyArcs.txt", what = character())
    graph <- empty.graph(nodes)
    if (input$arcsManuallyTo != input$arcsManuallyFrom) {
      arcs(graph) <- matrix(c(arcs, c(input$arcsManuallyFrom, input$arcsManuallyTo)), ncol=2, byrow = TRUE, dimnames = list(c(),c("from", "to")))
    } else {
      arcs(graph) <- matrix(arcs, ncol=2, byrow = TRUE, dimnames = list(c(),c("from", "to")))
    }
    write(as.vector(t(arcs(graph))), "graphManuallyArcs.txt")
  })
  
  output$plotManually <- renderPlot({
    nodesManually()
    arcsManually()
    nodes <- scan(file = "graphManuallyNodes.txt", what = character())
    arcs <- scan(file = "graphManuallyArcs.txt", what = character())
    graph <- empty.graph(nodes)
    arcs(graph) <- arcs
    plot(graph)
  })
  
  output$plotExample <- renderPlot({
    dag <- empty.graph(names(marks))
    arcs(dag) <- matrix(c("VECT", "MECH", "ALG","MECH", "ALG","VECT", "ANL", "ALG", "STAT","ALG","STAT","ANL"), ncol=2, byrow = TRUE, dimnames=list(c(),c("from","to")))
    plot(dag)
  })
  
  output$plotUpload <- renderPlot({
    if (!is.null(input$upload)) {
      mat <- read.table(input$upload$datapath, row.names = 1, header = TRUE)
      mat <-data.matrix(mat)
      graph <- empty.graph(colnames(mat))
      amat(graph) <- mat
      graphviz.plot(graph) 
    }  
  })
  
  output$tableArcsUpload <- renderTable({
    if (!is.null(input$upload)) {
      textArcs <- read.table(input$upload$datapath, row.names = 1, header = TRUE)
    }
  }, caption="Adjacency matrix of the uploaded graph")
  
  output$tableRlibs <- renderTable({
    tableRlibs <- read.table("Rlibs.txt", row.names = 1, header = TRUE)
  }, align='rcccccc')
  
})