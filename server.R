library(shiny)
library(bnlearn)
library(Rgraphviz)
library(shinydashboard)

shinyServer(function(input, output) {
  
  output$plotExample <- renderPlot({
    dag <- empty.graph(names(marks))
    arcs(dag) <- matrix(c("VECT", "MECH", "ALG","MECH", "ALG","VECT", "ANL", "ALG", "STAT","ALG","STAT","ANL"), ncol=2, byrow = TRUE, dimnames=list(c(),c("from","to")))
    graphviz.plot(dag)
  })
  
  output$plotCreate <- renderPlot({
      
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