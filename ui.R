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
              p("A brief introduction to the R environment and basic R programming is also provided.")
              )
            ),
    #BASICS:GRAPHTHEORY
    tabItem(tabName = "basicsgraphtheory",
            fluidPage(
              
              titlePanel("A Brief Introduction to Graph Theory"),
              h3("Graph, Nodes, and Arcs"),
              p("A graph $G = (\\mathbf{V}, A)$ consists of a nonempty set $\\mathbf{V}$ of ", strong(em("nodes")), " or ", strong(em("vertices")), "and a finite (but possibly empty) set $A$ of pairs of vertices called ", strong(em("arcs")), ", ", strong(em("links")), ", or ", strong(em("edges")), "."),
              p("Each arc $a = (u, v)$ can be defined either as an ordered or an unordered pair of nodes, which are said to be ", strong(em("connected")), "by and ", strong(em("incident")), " on the arc and to be ", strong(em("adjacent")), "to each other.
                Since they are adjacent, $u$ and $v$ are also said to be ", strong(em("neighbors")), ".
                If $(u,v)$ is an ordered pair, $u$ is said to be the ", strong(em("tail")), " of the arc and $v$ the ", strong(em("head")), ";
                then the arc is said to be ", strong(em("directed")), " from $u$ to $v$ and is usually represented with an arrowhead in $v$ $(u \\rightarrow v)$
                It is also said that the arc ", strong(em("leaves")), " or is ", strong(em("outgoing")), " for $u$ and that it ", strong(em("enters")), " or is ", strong(em("incoming")), " for $v$. 
                If $(u, v)$ is unordered, $u$ and $v$ are simply said to be incident on the arc without any further distinction. In this case, they are commonly referred to as ", strong(em("undirected arcs")), " or ", strong(em("edges")), ", denoted with $e \\in E$ and represented with a simple line $(u - v)$."),
              p("The characterization of arcs as directed or undirected induces an equivalent characterization of the graphs themselves, which are said to be ", strong(em("directed graphs")), " (denoted with $G = (\\mathbf{V}, A))$ if all arcs are directed, ", strong(em("undirected graphs")), " (denoted with $G = (\\mathbf{V}, E))$ if all arcs are undirected, and ", strong(em("partially directed")), " or ", strong(em("mixed graphs")), " (denoted with $G = (\\mathbf{V}, A, E)$) if they contain both directed and undirected arcs.
                An undirected graph can always be constructed from a directed or partially directed one by substituting all the directed arcs with undirected ones; 
                such a graph is called the ", strong(em("skeleton")), " or the ", strong(em("underlying undirected graph")), " of the original graph.")
              ),
              h3("The Structure of a Graph"),
              p("The pattern..."),
              h3("Further Reading"),
              p("For a broader...")
    ),
    #BASICS:INTRODUCTIONTOR
    tabItem(tabName = "basicsintroductiontor",
            fluidPage(
              titlePanel("The R Environment for Statistical Computing"),
              p("R (R Development Core Team, 2012) is a programming language..."),
              h3("Base Distribution and Contributed Packages"),
              p("The R environment..."),
              h3("A Quick Introduction to R"),
              p("We will now illustrate..."),
              h3("Further Reading"),
              p("Providing...")
            )
            ),
    #BASICS:EXERCISES
    tabItem(tabName = "basicsexercises",
            fluidPage(
              titlePanel("Exercises")
            )),
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