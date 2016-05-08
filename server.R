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
    imgwidth  <- session$clientData$output_graph01_width
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
    imgwidth  <- session$clientData$output_graph01_width
    imgheight <- imgwidth * pic.ratio
    list(src = "images/graph02.png",
         width = imgwidth,
         height = imgheight,
         alt = "images/graph02.png")
  }, 
  deleteFile = FALSE)
  
  output$selectLearningUI <- renderUI({
    switch (input$selectLearning,
            "Import: .bif file" = {fileInput("uploadLearningBif", "Choose .bif file", accept=c(".bif"))},
            "Import: .dsc file" = {fileInput("uploadLearningDsc", "Choose .dsc file", accept=c(".dsc"))},
            "Import: .net file" = {fileInput("uploadLearningNet", "Choose .net file", accept=c(".net"))},
            "Example: asia" = {downloadButton("downloadLearning", "Download")}
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
            "Import: .bif file" = {
              if (!is.null(input$uploadLearningBif)) {
                fitted <- read.bif(input$uploadLearningBif$datapath)
                graphviz.plot(fitted)
              }
            },
            "Import: .dsc file" = {
              if (!is.null(input$uploadLearningDsc)) {
                fitted <- read.dsc(input$uploadLearningDsc$datapath)
                graphviz.plot(fitted)
              }
            },
            "Import: .net file" = {
              if (!is.null(input$uploadLearningNet)) {
                fitted <- read.net(input$uploadLearningNet$datapath)
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
      if (!is.null(input$uploadLearningCsv)) {
        dataNames <- scan("dataNames", what = character())
        learningAddArcsInputUI <- fluidRow(
          column(6,
                 selectInput("learningAddArcsFrom", "From:", c(names(data)), selected = names(data)[1])),
          column(6,
                 selectInput("learningAddArcsTo", "To:", c(names(data)), selected = names(data)[1]))
        )
      }
    }
  })
  
  output$learningAddArcsButtonUI <-renderUI({
    if (input$selectLearning == "Data: .csv file") {
      if (!is.null(input$uploadLearningCsv)) {
        actionButton("learningAddArcsButton", "Add")
      }
    }
  })
  
  dataArcsFile <- eventReactive(input$learningAddArcsButton, {
    write(c(input$learningAddArcsFrom, input$learningAddArcsTo), file = "dataArcs", append = TRUE)
  })
  
  output$selectInferenceUI <- renderUI({
    switch (input$selectInference,
            "Import: .bif file" = {fileInput("uploadInferenceBif", "Choose .bif file", accept=c(".bif"))},
            "Import: .dsc file" = {fileInput("uploadInferenceDsc", "Choose .dsc file", accept=c(".dsc"))},
            "Import: .net file" = {fileInput("uploadInferenceNet", "Choose .net file", accept=c(".net"))},
            "Example: asia" = downloadButton("downloadInference", "Download")
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
                write(as.character(input$evidenceInferenceBif), file = "temp")
                inference <- paste("cpquery(fitted, ", event, ", ", evidence, ")", sep = "")
                textInference <- eval(parse(text = inference))
              }
            },
            "Import: .dsc file" = {
              if (!is.null(input$uploadInferenceDsc)) {
                fitted <- read.dsc(input$uploadInferenceDsc$datapath)
                event <- paste(as.character(input$eventInferenceDsc), " == ", as.character(input$eventInferenceDscValue))
                evidence <- as.character(input$evidenceInferenceDsc)
                write(as.character(input$evidenceInferenceDsc), file = "temp")
                inference <- paste("cpquery(fitted, ", event, ", ", evidence, ")", sep = "")
                textInference <- eval(parse(text = inference))
              }
            },
            "Import: .net file" = {
              if (!is.null(input$uploadInferenceNet)) {
                fitted <- read.net(input$uploadInferenceNet$datapath)
                event <- paste(as.character(input$eventInferenceNet), " == ", as.character(input$eventInferenceNetValue))
                evidence <- as.character(input$evidenceInferenceNet)
                write(as.character(input$evidenceInferenceNet), file = "temp")
                inference <- paste("cpquery(fitted, ", event, ", ", evidence, ")", sep = "")
                textInference <- eval(parse(text = inference))
              }
            },
            "Example: asia" = {
              net <- hc(asia)
              arcs(net) <- c(arcs(net), c("A", "T"))
              fitted <- bn.fit(net, asia)
              event <- paste(as.character(input$eventInferenceExampleAsia), " == ", as.character(input$eventInferenceExampleAsiaValue))
              evidence <- as.character(input$evidenceInferenceExampleAsia)
              write(as.character(input$evidenceInferenceExampleAsia), file = "temp")
              inference <- paste("cpquery(fitted, ", event, ", ", evidence, ")", sep = "")
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