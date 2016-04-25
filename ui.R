library(shiny)
library(bnlearn)
library(Rgraphviz)
library(shinydashboard)

header <- dashboardHeader(title = "Bayesian networks in R")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", selected = TRUE),
    menuItem("Basics", tabName = "basics"),
    menuItem("Try for youself", tabName = "tryforyourself",
             menuSubItem("How to", tabName = "tryforyourselfhowto"),
             menuSubItem("Create", tabName = "tryforyourselfcreate"),
             menuSubItem("Upload", tabName = "tryforyourselfupload")
    )
  )
)

body <- dashboardBody(
  tabItems(
    #OVERVIEW
    tabItem(tabName = "overview",
            fluidPage(
              titlePanel("Overview"),
              p(
                strong("Abstract"), " Data recorded across multiple variables of interest for a given phenomenon often do not contain any explicit temporal information.
                In the absence of such information, the data essentially represent a static snapshot of the underlying phenomenon at a particular moment in time. 
                For this reason, they are sometimes referred to as ", em("static data"), "."),
              p(
                "Static Bayesian networks, commonly known simply as Bayesian networks, provide an intuitive and comprehensive framework to model the dependencies between the variables in static data. 
                ."
              )
              )
              )
    ,
    #BAICS
    tabItem(tabName = "basics",
            fluidPage(
              titlePanel("Basics"),
              p(
                strong("Bayesian networks"), " are a class of graphical models that allow a concise representation of the probabilistic dependecies between a given set of random variables as a directed acyclic graph. 
                Each node corresponds to a random variable."),
              p("Consider a data set consisting of the exam scores of 88 students across five different topics, namely, ", strong(em("mechanics, ")), strong(em("vectors, ")), strong(em("algebra, ")), strong(em("analysis, ")), "and ", strong(em("statistics."))),
              plotOutput("plotExample")
              )
    ),
    #HOWTO
    tabItem(tabName = "tryforyourselfhowto",
            fluidPage(
              titlePanel("How to"),
              p("some text")
            )
    ),
    #CREATE
    tabItem(
      tabName = "tryforyourselfcreate",
      fluidPage(
        titlePanel("Create"),
        sidebarLayout(
          sidebarPanel(
            #fileInput("upload", "Choose a file:", accept = c(".txt"))
          ),
          mainPanel(
            plotOutput("plotCreate"),
            tableOutput("tableArcsCreate")
          )
        )
      )
    ),
    #UPLOAD
    tabItem(
      tabName = "tryforyourselfupload",
      fluidPage(
        titlePanel("Upload"),
        sidebarLayout(
          sidebarPanel(
            fileInput("upload", "Choose a file:", accept = c(".txt"))
          ),
          mainPanel(
            plotOutput("plotUpload"),
            box(
              tableOutput("tableArcsUpload")
            )
          )
        )
      )
    )
              )
              )

dashboardPage(header, sidebar, body)