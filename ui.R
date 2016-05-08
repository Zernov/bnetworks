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
             menuSubItem("R", tabName = "basicsr"),
             menuSubItem("Exercises >DEV<", tabName = "basicsexercises")
    ),
    menuItem("Creating graph >DEV<", tabName = "creating",
             menuSubItem("Manually >DEV<", tabName = "creatingmanually"),
             menuSubItem("Learning >DEV<", tabName = "creatinglearning")
    ),
    menuItem("Inference", tabName = "inference")
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
              p("The characterization of arcs as directed or undirected induces an equivalent characterization of the graphs themselves, which are said to be ", strong(em("directed graphs")), " (denoted with $G = (\\mathbf{V}, A))$ if all arcs are directed, ", strong(em("undirected graphs")), " (denoted with $G = (\\mathbf{V}, E))$ if all arcs are undirected, and ", strong(em("partially directed")), " or ", strong(em("mixed graphs")), " (denoted with $G = (\\mathbf{V}, A, E)$) if they contain both directed and undirected arcs."
                ),
              imageOutput("graph01", height = "auto"),
              fluidRow(
                align = "center",
                p(strong("Fig. 1: "), "An undirected graph (", em("left"), ") a directed graph (", em("center"), ") and a partially directed graph (", em("right"), ")")
              ),
              p("Examples of ", strong(em("directed")), ", ", strong(em("undirected")), ", and mixed ", strong(em("partially directed")), "graphs are shown in ", strong("Fig. 1"), "in that order.
                For the undirected graph, ", strong("Fig. 1"), ":"),
              tags$ul(
                tags$li("The node set is $\\mathbf{V} = \\{\\text{A},\\text{B},\\text{C},\\text{D},\\text{E}\\}$ and the edge set is $E = \\{(\\text{A}-\\text{B}), (\\text{A}-\\text{C}), (\\text{A}-\\text{D}), (\\text{B}-\\text{D}), (\\text{C}-\\text{E}), (\\text{D}-\\text{E})\\}$."), 
                tags$li("Arcs are undirected, so, i.e., $(\\text{A} - \\text{B})$ and $(\\text{B} - \\text{A})$ are equivalent and identify the same edge."), 
                tags$li("Likewise, $\\text{A}$ is connected to $\\text{B}$, $\\text{B}$ is connected to $\\text{A}$, and $\\text{A}$ and $\\text{B}$ are adjacent.")
              ),
              p("For the directed graph, ", strong("Fig. 1"), ":"
                ),
              tags$ul(
                tags$li("The node set is $\\mathbf{V} = \\{\\text{A},\\text{B},\\text{C},\\text{D},\\text{E}\\}$ and the graph is characterized by and arc set $A = \\{(\\text{A}\\rightarrow\\text{B}), (\\text{C}\\rightarrow\\text{A}), (\\text{D}\\rightarrow\\text{B}), (\\text{C}\\rightarrow\\text{D}), (\\text{C}\\rightarrow\\text{E})\\}$ instead of an edge set $E$."), 
                tags$li("Arcs are directed, so, i.e. $(\\text{A}\\rightarrow\\text{B})$ and $(\\text{B}\\rightarrow\\text{A})$ identify different arcs.
                        For instance, $(\\text{A}\\rightarrow\\text{B})\\in A$ while $(\\text{B}\\rightarrow\\text{A})\\notin A$.
                        Under the additional constraint of acyclicity, it is not possible for both arcs to be present in the graph because there can be at most one arc between each pair of nodes."),
                tags$li("Also, $\\text{A}$ and $\\text{B}$ are adjacent, as there is an arc $(\\text{A}\\rightarrow\\text{B})$ from $\\text{A}$ to $\\text{B}$. $(\\text{A}\\rightarrow\\text{B})$ is an outgoing arc for $\\text{A}$ (the tail), an incoming arc for $\\text{B}$ (the head), and an incident arc for both $\\text{A}$ and $\\text{B}$.")
              ),
              p("On the other hand, the partially directed graph, ", strong("Fig. 1"), " is characterized by the combination of an edge set $E = \\{(\\text{A}-\\text{C}), (\\text{A}-\\text{D}), (\\text{C}-\\text{D})\\}$ and an arc set $A = \\{(\\text{D}\\rightarrow\\text{E}), (\\text{E}\\rightarrow\\text{B})\\}$."),
              p("An undirected graph can always be constructed from a directed or partially directed one by substituting all the directed arcs with undirected ones; 
                such a graph is called the ", strong(em("skeleton")), " or the ", strong(em("underlying undirected graph")), " of the original graph."
                ),
              h3("The Structure of a Graph"),
              p("The pattern with which the arcs appear in a graph is referred to as either the ", strong(em("structure")), " of the graph or the ", strong(em("configuration")), " of the arcs.",
                "In the context of this application it is assumed that the vertices ", strong(em("u")), " and ", strong(em("v")), "incident on each arc are distinct and that there is at most one arc between them so that $(u,v)$ uniquely identifies an arc.",
                "This definition also implicity excludes presence of a ", strong(em("loop")), " that can occur when $u = v$."
                ),
              p("The simpliest structure is an ", strong(em("empty graph")), ", i.e., a graph with no arcs.
                On the other end of the spectrum are ", strong(em("saturated graphs")), ", in which each node is connected to every other node.
                Read-world graphical abstractions usually fall between these two extremes and can be either ", strong(em("sparse")), " or ", strong(em("dense")), ". 
                While the distinction between these two classes of graphs is rather vague, a graph is usually considered sparse if $O(|E|+|A|)=O(|\\mathbf{V}|)$."
                ),
              p("The structure of a graph can reveal interesting statistical properties.
                Some of the most important ones deal with ", strong(em("paths")), ".
                Paths are essentially sequences of arcs or edges ", strong(em("connecting")), " two nodes, called ", strong(em("end-vertices")), " or ", strong(em("end-nodes")), ".
                Paths are denoted with the sequence of vertices $(v_1, v_2, \\ldots, v_n)$ incident on those arcs.
                The arcs connecting the vertices $v_1, v_2, \\ldots, v_n$ are assumed to be unique, so that a path passes through each only once.
                In directed graphs it is also assumed that all the arcs in a path follow the same direction, and we say that a path ", strong(em("leads from $v_1$")), " (i.e., the tail of the first arc in the path) ", strong(em("to $v_n$")), " (i.e., the head of the last arc in the path).
                In undirected graphs and mixed graphs (and in general when referring to a graph regardless which class it belongs to), arcs in a path can point in either direction or be undirected.
                Paths in which $v_1 = v_n$ are called ", strong(em("cycles")), " and are treated with particular care in Bayesian network theory."
                ),
              p("The structure of a directed graph defines a partial ordering of the nodes if the graph is", strong(em("acyclic")), ", that is, if it does not contain any cycle or loop.
                This ordering is called an ", strong(em("acyclic")), " or ", strong(em("topological ordering")), " and is induced by the direction of the arcs. 
                It is defined as follows: if a node $v_i$ precedes $v_j$, there can be no arc from $v_j$ to $v_i$.
                According to this definition the first nodes are the ", strong(em("root nodes")), ", which have no incoming arcs, and the last ones are the ", strong(em("leaf nodes")), ", which have at least one incoming arc but no outgoing ones.
                Furthermore, if there is a path leading from $v_i$ to $v_j$, $v_i$ precedes $v_j$ in the sequence of the ordered nodes.
                In this case $v_i$ is called an ", strong(em("ancestor")), "of $v_j$ and $v_j$ is called a ", strong(em("descendant")), "of $v_i$.
                If the path is composed by a single arc, by analogy $x_i$ is a ", strong(em("parent")), " of $v_j$ and $v_j$ is a ", strong(em("child")), "of $v_i$."
                ),
              imageOutput("graph02", height = "auto"),
              fluidRow(
                align = "center",
                p(strong("Fig. 2: "), "Parents, children, ancestors, descendants, and neighbors of node $\\text{A}$ in a directed graph")
              ),
              p("Consider, for instance, node $\\text{A}$ in the directed acyclic graph shown in ", strong("Fig. 2"), ".
                Its neighborhood is the union of the parents and children;
                adjacent nodes necessarily fall into one of these two categories.
                Its parents are also ancestors, as they necessarily precede $\\text{A}$ in the topological ordering.
                Likewise, children are also descendants."
                ),
              p("The topological ordering induced by the graph structure is $$(\\{\\text{F},\\text{G},\\text{H}\\},\\{\\text{C},\\text{B}\\},\\{\\text{A}\\},\\{\\text{D},\\text{E}\\},\\{\\text{L},\\text{K}\\})$$."
                ),
              p("The nodes are only ", strong(em("partially ordered")), ";
                for example, no ordering can be established among root nodes or leaf nodes.
                As a result, in practice the topological ordering of a directed acyclic graph is defined over a set of unordered sets of nodes, denoted with $V_i = \\{v_{i_1}, \\ldots, v_{i_k}\\}$, defining a partition of $\\textbf{V}$."
                ),
              h3("Further Reading"),
              p("For a broader coverage of the properties of directed and mixed graphs, we refer the reader to the monograph by Bang-Jensen and Gutin (2009), which at the time of this writing is the most complete reference on the subject. 
                For undirected graphs, we refer to the classic book of Diestel (2005)."
                )
            )
    ),
    #BASICS:R
    tabItem(tabName = "basicsr",
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
              pre(includeText("pre/creating01")),
              p("Upon loading the data, we can create an empty network with the nodes corresponding to the variables in ", code("marks"), " using the ", code("empty.graph"), "function."),
              pre(includeText("pre/creating02"))
              )
            ),
    #INFERENCE
    tabItem(
      tabName = "inference",
      fluidPage(
        titlePanel("Inference"),
        sidebarLayout(
          sidebarPanel(
            selectInput("selectInference", "Inference from:", c("Import: .bif file", "Import: .dsc file", "Import: .net file", "Example: asia"), selected = "Import: .bif file"),
            uiOutput("selectInferenceUI"),
            uiOutput("eventInferenceUI"),
            uiOutput("evidenceInferenceUI"),
            uiOutput("makeInferenceUI")
          ),
          mainPanel(
            plotOutput("plotInference"),
            textOutput("outputInference")
          )
        )
      )
    )
    )
)

dashboardPage(header, sidebar, body)