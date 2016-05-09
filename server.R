library(shiny)
library(bnlearn)
library(Rgraphviz)
library(shinydashboard)

shinyServer(function(input, output, session) {
  
  output$graph01 <- renderImage({
    require(png)
    pic <- readPNG("images/graph01.png")
    pic.size <- dim(pic)
    pic.ratio <- pic.size[1]/pic.size[2]
    imgwidth  <- min(session$clientData$output_graph01_width, pic.size[2])
    imgheight <- imgwidth * pic.ratio
    list(src = "images/graph01.png",
         width = imgwidth,
         height = imgheight,
         alt = "images/graph01.png")
  }, 
  deleteFile = FALSE)
  
  output$graph02 <- renderImage({
    require(png)
    pic <- readPNG("images/graph02.png")
    pic.size <- dim(pic)
    pic.ratio <- pic.size[1]/pic.size[2]
    imgwidth  <-min(session$clientData$output_graph01_width, pic.size[2])
    imgheight <- imgwidth * pic.ratio
    list(src = "images/graph02.png",
         width = imgwidth,
         height = imgheight,
         alt = "images/graph02.png")
  }, 
  deleteFile = FALSE)
  
  output$r01 <- renderImage({
    require(png)
    pic <- readPNG("images/r01.png")
    pic.size <- dim(pic)
    pic.ratio <- pic.size[1]/pic.size[2]
    imgwidth  <-min(session$clientData$output_graph01_width, pic.size[2])
    imgheight <- imgwidth * pic.ratio
    list(src = "images/r01.png",
         width = imgwidth,
         height = imgheight,
         alt = "images/r01.png")
  }, 
  deleteFile = FALSE)
  
  output$r02 <- renderImage({
    require(png)
    pic <- readPNG("images/r02.png")
    pic.size <- dim(pic)
    pic.ratio <- pic.size[1]/pic.size[2]
    imgwidth  <-min(session$clientData$output_graph01_width, pic.size[2])
    imgheight <- imgwidth * pic.ratio
    list(src = "images/r02.png",
         width = imgwidth,
         height = imgheight,
         alt = "images/r02.png")
  }, 
  deleteFile = FALSE)
  
  output$exercises01 <- renderImage({
    require(png)
    pic <- readPNG("images/exercises01.png")
    pic.size <- dim(pic)
    pic.ratio <- pic.size[1]/pic.size[2]
    imgwidth  <- min(session$clientData$output_graph01_width, pic.size[2])
    imgheight <- imgwidth * pic.ratio
    list(src = "images/exercises01.png",
         width = imgwidth,
         height = imgheight,
         alt = "images/exercises01.png")
  }, 
  deleteFile = FALSE)
  
  output$selectInferenceUI <- renderUI({
    switch (input$selectInference,
            "Import: .bif file" = { fileInput("uploadInferenceBif", "Choose .bif file", accept=c(".bif")) },
            "Import: .dsc file" = { fileInput("uploadInferenceDsc", "Choose .dsc file", accept=c(".dsc")) },
            "Import: .net file" = { fileInput("uploadInferenceNet", "Choose .net file", accept=c(".net")) },
            "Example: asia" = { downloadButton("downloadInference", "Download") }
    )
  })
  
  output$eventInferenceUI <- renderUI({
    switch (input$selectInference,
            "Import: .bif file" = {
              if (!is.null(input$uploadInferenceBif)) {
                fitted <- read.bif(input$uploadInferenceBif$datapath)
                fluidRow(
                  column(7,
                         selectInput("eventInferenceBif", "Event:", names(fitted))),
                  column(5,
                         textInput("eventInferenceBifValue", "Value:"))
                )
              }
            },
            "Import: .dsc file" = {
              if (!is.null(input$uploadInferenceDsc)) {
                fitted <- read.dsc(input$uploadInferenceDsc$datapath)
                fluidRow(
                  column(7,
                         selectInput("eventInferenceDsc", "Event:", names(fitted))),
                  column(5,
                         textInput("eventInferenceDscValue", "Value:"))
                )
              }
            },
            "Import: .net file" = {
              if (!is.null(input$uploadInferenceNet)) {
                fitted <- read.net(input$uploadInferenceNet$datapath)
                fluidRow(
                  column(7,
                         selectInput("eventInferenceNet", "Event:", names(fitted))),
                  column(5,
                         textInput("eventInferenceNetValue", "Value:"))
                )
              }
            },
            "Example: asia" = {
              fluidRow(
                column(7,
                       selectInput("eventInferenceExampleAsia", "Event:", names(asia))),
                column(5,
                       textInput("eventInferenceExampleAsiaValue", "Value:"))
              )
            }
    )
  })
  
  output$evidenceInferenceUI <- renderUI({
    switch (input$selectInference,
            "Import: .bif file" = {
              if (!is.null(input$uploadInferenceBif)) {
                textInput("evidenceInferenceBif", "Evidence:")
              }
            },
            "Import: .dsc file" = {
              if (!is.null(input$uploadInferenceDsc)) {
                textInput("evidenceInferenceDsc", "Evidence:")
              }
            },
            "Import: .net file" = {
              if (!is.null(input$uploadInferenceNet)) {
                textInput("evidenceInferenceNet", "Evidence:")
              }
            },
            "Example: asia" = {
              
              textInput("evidenceInferenceExampleAsia", "Evidence:")
            }
    )
  })
  
  output$makeInferenceUI <- renderUI({
    switch (input$selectInference,
            "Import: .bif file" = {
              if (!is.null(input$uploadInferenceBif)) {
                actionButton("makeInferenceButton", "Inference")
              }
            },
            "Import: .dsc file" = {
              if (!is.null(input$uploadInferenceDsc)) {
                actionButton("makeInferenceButton", "Inference")
              }
            },
            "Import: .net file" = {
              if (!is.null(input$uploadInferenceNet)) {
                actionButton("makeInferenceButton", "Inference")
              }
            },
            "Example: asia" = {
              actionButton("makeInferenceButton", "Inference")
            }
    )
  })
  
  textInference <- eventReactive(input$makeInferenceButton, {
    switch (input$selectInference,
            "Import: .bif file" = {
              if (!is.null(input$uploadInferenceBif)) {
                fitted <- read.bif(input$uploadInferenceBif$datapath)
                event <- paste(as.character(input$eventInferenceBif), " == ", as.character(input$eventInferenceBifValue))
                evidence <- as.character(input$evidenceInferenceBif)
                inference <- paste("cpquery(fitted, ", event, ", ", evidence, ", n = 10^6)", sep = "")
                textInference <- eval(parse(text = inference))
              }
            },
            "Import: .dsc file" = {
              if (!is.null(input$uploadInferenceDsc)) {
                fitted <- read.dsc(input$uploadInferenceDsc$datapath)
                event <- paste(as.character(input$eventInferenceDsc), " == ", as.character(input$eventInferenceDscValue))
                evidence <- as.character(input$evidenceInferenceDsc)
                inference <- paste("cpquery(fitted, ", event, ", ", evidence, ", n = 10^6)", sep = "")
                textInference <- eval(parse(text = inference))
              }
            },
            "Import: .net file" = {
              if (!is.null(input$uploadInferenceNet)) {
                fitted <- read.net(input$uploadInferenceNet$datapath)
                event <- paste(as.character(input$eventInferenceNet), " == ", as.character(input$eventInferenceNetValue))
                evidence <- as.character(input$evidenceInferenceNet)
                inference <- paste("cpquery(fitted, ", event, ", ", evidence, ", n = 10^6)", sep = "")
                textInference <- eval(parse(text = inference))
              }
            },
            "Example: asia" = {
              net <- hc(asia)
              arcs(net) <- c(arcs(net), c("A", "T"))
              fitted <- bn.fit(net, asia)
              event <- paste(as.character(input$eventInferenceExampleAsia), " == ", as.character(input$eventInferenceExampleAsiaValue))
              evidence <- as.character(input$evidenceInferenceExampleAsia)
              inference <- paste("cpquery(fitted, ", event, ", ", evidence, ", n = 10^6)", sep = "")
              textInference <- eval(parse(text = inference))
            }
    )
  })
  
  output$outputInference <- renderText({
    textInference()
  })
  
  output$plotInference <- renderPlot({
    switch (input$selectInference,
            "Import: .bif file" = {
              if (!is.null(input$uploadInferenceBif)) {
                fitted <- read.bif(input$uploadInferenceBif$datapath)
                graphviz.plot(fitted)
              }
            },
            "Import: .dsc file" = {
              if (!is.null(input$uploadInferenceDsc)) {
                fitted <- read.dsc(input$uploadInferenceDsc$datapath)
                graphviz.plot(fitted)
              }
            },
            "Import: .net file" = {
              if (!is.null(input$uploadInferenceNet)) {
                fitted <- read.net(input$uploadInferenceNet$datapath)
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
  
  output$downloadInference <- downloadHandler(
    filename = function() {
      paste('asia.csv', sep = '')
    },
    content = function(file) {
      write.csv(file, asia)
    }
  )
  
  # 
  # output$tableRlibs <- renderTable({
  #   tableRlibs <- read.table("Rlibs.txt", row.names = 1, header = TRUE)
  # }, align='rcccccc')
  
})