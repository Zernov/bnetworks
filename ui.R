library(shiny)
library(bnlearn)
library(Rgraphviz)
library(shinydashboard)

header <- dashboardHeader(title = "Bayesian Networks")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", selected = TRUE),
    menuItem("Basics", tabName = "basics",
             menuSubItem("Graph theory", tabName = "basicsgraphtheory"),
             menuSubItem("Introduction to R", tabName = "basicsintroductiontor"),
             menuSubItem("Exercises", tabName = "basicsexercises")
    ),
    menuItem("Creating graph", tabName = "creating",
             menuSubItem("How to", tabName = "creatinghowto"),
             menuSubItem("Manually", tabName = "creatingmanually"),
             menuSubItem("Learning", tabName = "creatinglearning")
    )
  )
)

body <- dashboardBody(
  withMathJax(),
  tags$script("
                          MathJax.Hub.Config({
                          tex2jax: {
                          inlineMath: [['$','$']],
                          processEscapes: true
                          }
                          });"
  ),
  tabItems(
    #OVERVIEW
    tabItem(tabName = "overview",
            fluidPage(
              titlePanel("Overview"),
              p(strong("Abstract"), " Data recorded across multiple variables of interest for a given phenomenon often do not contain any explicit temporal information.
                In the absence of such information, the data essentially represent a static snapshot of the underlying phenomenon at a particular moment in time. 
                For this reason, they are sometimes referred to as ", strong(em("static data")), "."),
              p("Static Bayesian networks, commonly known simply as Bayesian networks, provide an intuitive and comprehensive framework to model the dependencies between the variables in static data."),
              p("In this application a brief introduction to the terminology and the basic properties of graphs, with particular attention to directed graphs, is provided."),
              p("A brief introduction to the $R$ environment and basir $R$ programming is also provided.")
              )
            ),
    #BASICS:GRAPHTHEORY
    tabItem(tabName = "basicsgraphtheory",
            fluidPage(
              titlePanel("Graph theory"),
              h3("Graph, Nodes, and Arcs"),
              p("A graph $G = (V, A)$ consists of a nonempty set $V$ of ", strong(em("nodes")), " or ", strong(em("vertices")), "and a finite (but possibly empty) set $A$ of pairs of vertices called ", strong(em("arcs")), ", ", strong(em("links")), ", or ", strong(em("edges")), "."),
              p("Each arc $a = (u, v)$ can be defined either as an ordered or an unordered pair of nodes, which are said to be ", strong(em("connected")), "by and ", strong(em("incident")), " on the arc and to be ", strong(em("adjacent")), "to each other.
                ")
              )
    ),
    #CREATING:HOWTO
    tabItem(tabName = "creatinghowto",
            fluidPage(
              titlePanel("How to"),
              p("Consider a data set consisting of the exam scores of 88 students across five different topics, namely, mechanics, vectors, algebra, analysis, and statistics.
                The scores are bounded in the interval [0, 100].
                This data set was originally investigated by Mardia. et al. (1979) and subsequently in classic books on graphical models such as Whittaker (1990) and Edwards (2000).
                A copy of the data is included in ", strong("bnlearn"), " under the name ", code("marks"), "."),
              pre(includeText("examples/creating01")),
              p("Upon loading the data, we can create an empty network with the nodes corresponding to the variables in ", code("marks"), " using the ", code("empty.graph"), "function."),
              pre(includeText("examples/creating02"))
              )
            ),
    #CREATING:MANUALLY
    tabItem(
      tabName = "creatingmanually",
      fluidPage(
        titlePanel("Manually"),
        sidebarLayout(
          sidebarPanel(
            sliderInput("nodesManually", 
                        "Number of nodes:", 
                        min = 1,
                        max = 10, 
                        value = 5),
            uiOutput("arcsFromManuallyUI"),
            uiOutput("arcsToManuallyUI"),
            actionButton("arcsAddManually", "Add")
          ),
          mainPanel(
            plotOutput("plotManually"),
            tableOutput("tableArcsManually")
          )
        )
      )
    ),
    #CREATING:LEARNING
    tabItem(
      tabName = "creatinglearning",
      fluidPage(
        titlePanel("Learning"),
        sidebarLayout(
          sidebarPanel(
            fileInput("uploadLearning", "Choose a file:", accept = c(".txt"))
          ),
          mainPanel(
            plotOutput("plotLearning"),
            box(
              tableOutput("tableArcsLearning")
            )
          )
        )
      )
    )
    )
)

dashboardPage(header, sidebar, body)