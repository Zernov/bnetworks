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
             menuSubItem("Exercises", tabName = "basicsexercises")
    ),
    menuItem("Bayesian networks", tabName = "bnetworks"),
    menuItem("Inference", tabName = "inference")
  )
)

body <- dashboardBody(
  withMathJax(),
  tags$script("MathJax.Hub.Config({ tex2jax: { inlineMath: [['$','$']] } });"),
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
              fluidRow(
                align = "center",
                imageOutput("graph01", height = "auto"),
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
              fluidRow(
                align = "center",
                imageOutput("graph02", height = "auto"),
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
              p("R (R Development Core Team, 2012) is a programming language and an environment targeted at statistical computing, released as an open-source software under GNU General Public License (GPL).
                The main Web site of the R Project is ", a("http://www.r-project.org", href="http://www.r-project.org"), "."
              ),
              p("R supports all common operating systems (Windows, MacOS X and Linux) in addition to several Unix variants and has been constantly updated and improved over the years to become a standart choice for data analysis and the development of new statistical techniques."
              ),
              h3("Base Distribution and Contributed Packages"),
              p("The R environment consists of a ", strong(em("base distribution")), ", maintained and developed by the R Core Team, and a constantly growing set of ", strong(em("contributed packages")), ".
                Both are distributed through a network of servers called \"The Comprehensive R Archive Network\" (CRAN), which provides up-to-date mirrors of the main site, located at ", a("http://cran.r-project.org", href="http://cran.r-project.org"), "."
              ),
              p("The base distribution provides a set of standart packages implementing the basic functionality of R, including the following:"
              ),
              tags$ul(
                tags$li("Probability, density, distribution, and quantile functions for commonly used probability distributions"),
                tags$li("Functions to produce nicely formatted plots such as boxplots, histograms, and scatterplots"),
                tags$li("Statistical models such as linear and generalized linear models as well as functions for statistical hypothesis testing"),
                tags$li("Several reference data sets from literature"),
                tags$li("Utilities to import and export data in various formats (e.g., space- and tab-separated text;
                        comma-separated values (CSV);
                        files saves from other statistical software such as STATA, SPSS, and Octave; and many more).")
              ),
              p("Contributed packages are implemented by independent developers and then submitted to CRAN, which provides a unified distribution network and basic quality checking.
                In recent years it has become increasingly common to provide reference implementations of new methodologies as R packages.
                This trend ahs improved the reproducibility of scientific results presented in literature and, at the same time, has increased dramatically the number of fields in which R is a valuable data analysis tool."
              ),
              h3("A Quick Introduction to R"),
              p("We will now illustrate some basic R commands for importing, exploring, summarizing, and plotting data.
                For this purpose, we will use the ", code("lizards"), " data set included in the ", strong("bnlearn"), " package because of its simple structure.
                This data set was originally published in Schoener (1968) and has been used by Fienberg (1980) and more recently by Edwards (2000) as an example in the respective books."
              ),
              p("First of all, we need to install the ", strong("bnlearn"), " package from one of the mirrors of the CRAN network. After launching R, we can type the following command after the \">\" prompt:"
              ),
              pre(includeText("pre/r01")),
              p("An up-to-date list of mirrors to choose from will be displayed as either a pop-up window or a text prompt.
                Once ", strong("bnlearn"), " has been installed, it can be loaded with"
              ),
              pre(includeText("pre/r02")),
              p("Clearly, ", code("install.package"), " need to be called only once for any given package, while loading the package with ", code("library"), " is required at every new R session even when the workspace of the last session has beed restored at start-up."
              ),
              p("The ", code("lizards"), " data set can then be loaded from ", strong("bnlearn"), " with"
              ),
              pre(includeText("pre/r03")),
              p("since the package is now loaded in the R session.
                If the data were stored in a text file, we could have imported them into R using the ", code("read.table"), "function as follows:"
              ),
              pre(includeText("pre/r04")),
              p("Setting the ", code("header"), " argument to ", code("TRUE"), " tells ", code("read.table"), " that the first line of the file ", code("lizards.txt"), " contains the variable names.
                Each observation must be written in a single line, and the values assumed by the variables for that observation correspond to the fields (separated by spaces or tabulations) present in that line."
              ),
              p("In both cases, the data is stored in a ", strong(em("data frame")), " called ", code("lizards"), ", whose structure can be examined with the ", code("str"), " function."
              ),
              pre(includeText("pre/r05")),
              p("Like most programming languages, R defines a large set of ", strong(em("classes")), " of objects, which represent and provide an interface to different types of variables.
                Some of these classes correspond to various kinds of variables used in statistical modeling:"
              ),
              tags$ul(
                tags$li(strong(em("Logical"), ":"), " indicator variables, e.g., either ", span("TRUE"), " or ", span("FALSE")),
                tags$li(strong(em("Integer"), ":"), " natural numbers, e.g., $1, 2, \\ldots, n \\in\\mathbb{N}$"),
                tags$li(strong(em("Numeric"), ":"), " real numbers, such as $1.2, \\pi, \\sqrt{2}$"),
                tags$li(strong(em("Character"), ":"), " character string, such as $\\text{\"a\"},\\text{\"b\"},\\text{\"c\"}$"),
                tags$li(strong(em("Factor"), ":"), " categorical variables, defined over a finite set of ", strong(em("levels")), " identified by character strings"),
                tags$li(strong(em("Ordered"), ":"), " ordered categorical variables, similar to factors but with and explicit ordering of the levels, e.g., ", span("\"LOW\" < \"AVERAGE\" < \"HIGH\""))
              ),
              p("Other classes correspond to more complex data types, such as multidimensional or heterogeneous data:"
              ),
              tags$ul(
                tags$li(strong(em("List"), ":"), " a collection of arbitrary objects, often belonging to different classes"),
                tags$li(strong(em("Vector"), ":"), " a mathematical vector of elements belonging to the same class (i.e., all integers, all factors with the same levels, etc.) with and arbitrary number of dimensions"),
                tags$li(strong(em("Matrix"), ":"), " a matrix (i.e., a 2-dimensional vector) of elements belonging to the same class"),
                tags$li(strong(em("Data frame"), ":"), " a list of objects with the same length but possibly different classes. 
                        It is usually displayed and manipulated in the same way as matrix.")
                ),
              p("As we can see from the output of ", code("str"), ", ", code("read.table"), " saves the data read from ", code("lizards.txt"), " in a data frame to allow each variable to be stored as an object of the appropriate class.
                The labels of the possible values of each variable, which are character strings in ", code("lizards.txt"), " are automatically used as levels and the variables conderted to factors."
              ),
              p("We can further investigate the characteristics of the ", code("lizards"), " data frame with the ", code("summary"), " and ", code("dim"), " functions."
              ),
              pre(includeText("pre/r06")),
              p("From the output of ", code("str"), ", ", code("summary"), ", and ", code("dim"), ", we can see that the data frame contains 409 observations and 3 variables names ", code("Species"), ", ", code("Diameter"), ", and ", code("Height"), ".
                Each observations refers to a single lizard and describes its species (either ", strong(em("sagrei")), " or ", strong(em("distichus")), ") and the height and width of the branch it was perched on when sighted.
                All the variables are categorical and therefore are stored as ", strong(em("factors")), ";
                the values they can assume can be listed with the ", code("levels"), "function."
              ),
              pre(includeText("pre/r07")),
              p("An alternative, useful way of displaying these data is a contingency table, which can be built using the ", code("table"), " function."
              ),
              pre(includeText("pre/r08")),
              p("The order in which the two-dimensional contigency tables are listed depends on the order of the variables in the data frame;
                in this case it is useful to have them split by specie first, so the columns of ", code("lizards"), "were rearranged appropriately."
              ),
              p("Exploratory data analysis often includes some form of graphical data visualization, especially when dealing with low-dimensional data sets such as the one we are considering.
                A simple way to plot the frequencies associated with ", code("Height"), " and ", code("Diameter"), " for each species is to use a ", strong(em("barplot")), "."
              ),
              pre(includeText("pre/r09")),
              p(strong("Fig. 3"), " shows the plot generated by the commands above. 
                The first two commands extract from the data set the subsets of observations corresponding to each species. 
                ", code("par"), " is then used to split the plot area into four quadrants, arranged in a layout with 2 rows and 2 columns.
                Each quadrant holds one of the barplots, which are generated by the ", code("plot"), " function. 
                ", code("plot"), " is a generic function for data visualization that chooses a suitable plot depending on the data, in this case, barplots for factors.
                The ", code("main"), " argument specifies the title of each plot, while ", code("xlab"), " and ", code("ylab"), " specify the labels to the horizontal and vetrical axes, respectively."
              ),
              fluidRow(
                align = "center",
                imageOutput("r01", height = "auto"),
                p(strong("Fig. 3: "), "Barplots for the perch height and diameter of ", strong(em("sagrei")), " and ", strong(em("distichus")), " lizards")
              ),
              p("Exploring numeric data requires many of the R functions illustrated above for categorical data.
                According to the description of the ", code("lizards"), " data provided in Schoener (1968), a branch is classified as ", code("narrow"), " if its diameter is lesser or equal than 4 inches and ", code("wide"), " otherwise.
                For the sake of the example, we can generate some random values according to these specifications and associate them with the ", code("Species"), "."
              ),
              pre(includeText("pre/r10")),
              p("First, we create a new vector caled ", code("diam"), " with one entry for each observation (the number of rows (", code("nrow"), ") of ", code("lizards"), ").
                Then we create two logical vectors with indicator variables identifying which branches are ", code("narrow"), " and which are ", code("wide"), " and use it to correctly assign the generated random diameters.
                The ", code("runif"), " function generates independent random values from a uniform distribution in the range $(2,4)$ for ", code("narrow"), " branches and in $(4,6)$ for ", code("wide"), " branches.
                The (now populated) vector ", code("diam"), " is then stored as ", code("Sim.Diameter"), " in a new data frame called ", code("new.data"), " along with the ", code("Species"), " variable from the original data."
              ),
              p("The behavior of ", code("Sim.Diameter"), " can again be examined using ", code("summary"), ", which in this case reports the mean and the quantiles of the new variable."
              ),
              pre(includeText("pre/r11")),
              p("It is also interesting to investigate how the diameter differs between the two ", code("Species"), ", both in location (with ", code("summary"), " again) and variability (with ", code("var"), ")."
              ),
              pre(includeText("pre/r12")),
              pre(includeText("pre/r13")),
              p("The same comparison can be performed graphically by plotting the ", strong(em("boxplots")), " corresponding to the diameters of the branches for each specie next to each other;
                the resulting plot is displayed in ", strong("Fig. 4"), "."
              ),
              fluidRow(
                align = "center",
                imageOutput("r02", height = "auto"),
                p(strong("Fig. 4: "), "Boxplots for the simulated branch diameters for ", strong(em("sagrei")), " and ", strong(em("distichus")), " lizards")
              ),
              pre(includeText("pre/r14")),
              p("The call to ", code("abline"), " adds a reference line separating ", code("narrow"), " branches from ", code("wide"), " ones, which helps relating the behaviour of the new data to the original ones.
                The ", code("lty"), " parameter specifies the line type, which in this case is dashed."
              ),
              h3("Further Reading"),
              p("Providing a complete introduction to the R language in this page is clearly impossible, and it is outside the scope of this application.
                R and CRAN provie a comprehensive documentation in the form of several manuals available from CRAN and help pages distributed with the base and contributed packages describing the characteristics of each function and data set.
                Help pages can be accessed from within R using the", code("help"), ", ", code("help.search"), " functions and the ", code("?"), " operator (e.g., ", code("?runif"), ")."
              ),
              p("We refer the user to the excellent \"Modern Applied Statistics with S\" by Venables and Ripley (2002) and to \"Data Manipulation with R\" by Spector (2009) for an in-depth coverage of R's capabilities. 
                Users interested in understanding the finer points of R programming, such as perfomance tuning and integrating compiled code, should also check \"S Programming\" by Venables and Ripley (2000)."
              )
              )
              ),
    #BASICS:EXERCISES
    tabItem(tabName = "basicsexercises",
            fluidPage(
              titlePanel("Exercises"),
              h4(strong("1."), " Consider a directed acyclic graph with $n$ nodes."),
              tags$ol(
                type = "a",
                tags$li("Show that at least one node must not have any incoming arc, i.e., the graph must contain at least one root node."),
                tags$li("Show that such a graph can have at most $\\frac{1}{2}n(n-1)$ arcs."),
                tags$li("Show that a path can span at most $n-1$ arcs."),
                tags$li("Describe an algogithm to determine the topological ordering of the graph.")
              ),
              h4(strong("2."), " Consider the graphs below."),
              fluidRow(
                align = "center",
                imageOutput("exercises01", height = "auto")
              ),
              tags$ol(
                type = "a",
                tags$li("Obtain the skeleton of the partially directed and directed graphs."),
                tags$li("Enumerate the acyclic graphs that can be obtained by orienting the undirected arcs of the partially directed graph."),
                tags$li("List the arcs that can be reversed (i.e., turned in the opposite direction), one at a time, without introducing cycles in the directed graph.")
              ),
              h4(strong("3."), " The (famous) ", code("iris"), " data set reports the measurements in centimeters of the sepal length and width and the petal length and width for 50 flowers from each of 3 species of iris (\"setosa\", \"versicolor\", and \"virginica\")."),
              tags$ol(
                type = "a",
                tags$li("Load the ", code("iris"), " data set (it is included in the ", strong("datasets"), " package, which is part of the base R distribution and does not need to be loaded explicitly) and read its manual page."),
                tags$li("Investigate the structure of the data set."),
                tags$li("Compare the sepal length among the three species by plotting histograms side by side."),
                tags$li("Repeat the previous point using boxplots")
              ),
              h4(strong("4."), " Consider again the ", code("iris"), " data set from ", strong("Exercise 3"), "."),
              tags$ol(
                type = "a",
                tags$li("Write the data frame holding ", code("iris"), " data frame into a space-separated text file named \"iris.txt\", and read it back into a second data frame called ", code("iris2"), "."),
                tags$li("Check that ", code("iris"), " and ", code("iris2"), " are identical."),
                tags$li("Repeat the previous two steps with a file compressed with ", code("bnzip2"), " named \"iris.txt.bz2\"."),
                tags$li("Save ", code("iris"), " directly (e.g., without converting it to a text table) into a file called \"iris.rda\", and read it back."),
                tags$li("List all R objects in the global environment and remove all of them apart from ", code("iris"), "."),
                tags$li("Exit the R saving the contents of the current session.")
              ),
              h4(strong("5."), " Consider the ", code("gaussian.test"), " data set included in ", strong("bnlearn"), "."),
              tags$ol(
                type = "a",
                tags$li("Print the column names."),
                tags$li("Print the range and the quartiles of each variable."),
                tags$li("Print all the observations for which $\\text{A}$ falls in the interval $[3,4]$ and $\\text{B}$ in $(-\\infty,-5]\\cup[10,\\infty)$."),
                tags$li("Sample 50 rows without replacement."),
                tags$li("Draw a bootsrap sample (e.g., sample 5,000 observations with replacement) and compute the mean of each variable."),
                tags$li("Standardize each variable.")
              ),
              h4(strong("6."), " Generate a data frame with 100 observations for the following variables:"),
              tags$ol(
                type = "a",
                tags$li("A categorical variable with two levels, ", code("low"), " and ", code("high"), ".
                        The first 50 observations should be set to ", code("low"), " the others to ", code("high"), "."),
                tags$li("A categorical variable with two levels, ", code("good"), " and ", code("bad"), ", nested within the first variable, i.e., the first 25 observations should be set to ", code("good"), ", the second 25 to ", code("bad"), ", and so on."),
                tags$li("A continious, numerical variable following a Gaussian distribution with mean 2 and variance 4 when the first variable is equal to ", code("low"), " and with mean 4 and variance 1 if the first variable is equal to ", code("high"), ".")
              )
            )),
    #BAYESIANNETWORKS
    tabItem(tabName = "bnetworks",
            fluidPage(
              titlePanel("Bayesian Networks Theory"),
              p("Bayesian networks are a class of ", strong(em("graphical models")), " that allow a concise representation of the probabolistic dependencies between a given set of random variables $\\mathbf{X} = \\{X_1, X_2, \\ldots, X_p\\}$ as a ", strong(em("directed acyclic graph")), " (DAG) $G = (\\mathbf{V},A)$.
                Each node $v_i\\in\\mathbf{V}$ corresponds to a random variable $X_i$."
                ),
              titlePanel("Static Bayesian Networks Modeling with R"),
              p(""
                )
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
      titlePanel("cpquery"),
      p("The conditional propability queries implemented in ", strong("bnlearn"), " in ", code("cpdist"), " and ", code("cpquery"), " functions."
      ),
      pre(includeText("pre/inference01")),
      p(code("cpquery"), " returns a numeric value, the conditional probability of ", code("event"), " conditional on ", code("evidence"), "."
      ),
      p(code("fitted"), " is an object of class ", code("bn.fit"), "."
      ),
      p("You can see how it works with the mechanism below."
      ),
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