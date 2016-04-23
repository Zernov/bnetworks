library(shiny)
library(bnlearn)
library(Rgraphviz)

shinyServer(function(input, output) {
  
  output$example <- renderPlot({
    
    dag <- empty.graph(names(marks))
    arcs(dag) <- matrix(c("VECT", "MECH", "ALG","MECH", "ALG","VECT", "ANL", "ALG", "STAT","ALG","STAT","ANL"), ncol=2, byrow = TRUE, dimnames=list(c(),c("from","to")))
    graphviz.plot(dag)
    
  })
  
  output$plot <- renderPlot({
    
    if (!is.null(input$upload)) {
    
      mat <- read.table(input$upload$datapath, row.names = 1, header = TRUE)
      mat <-data.matrix(mat)
      graph <- empty.graph(colnames(mat))
      amat(graph) <- mat
      graphviz.plot(graph)
      
    }
    
  })
  
  output$textArcs <- renderTable({
    
    if (!is.null(input$upload)) {
      
      textArcs <- read.table(input$upload$datapath, row.names = 1, header = TRUE)
      
    }
    
  })
  
  
})