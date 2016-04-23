library(shiny)
library(bnlearn)
library(Rgraphviz)

shinyUI(navbarPage("Bayesian networks", id="nav",
                   
                   tabPanel("Overview",
                            
                            fluidPage(titlePanel("Bayesian networks"),
                                      
                                      p(strong("Bayesian networks"), " are a class of graphical models that allow a concise representation of the probabilistic dependecies betweek a given set of random variables as a directed acyclic graph. Each node corresponds to a random variable."),
                                      
                                      p("Consider a data set consisting of the exam scores of 88 students across five different topics, namely, ", strong(em("mechanics")), ", ", strong(em("vectors")), ", ", strong(em("algebra")), ", ", strong(em("analysis")), ", and ", strong(em("statistics")), "."),
                                      
                                      plotOutput("example")
                              
                            )
                   ),
                   
                   tabPanel("Upload graph",
                            
                            fluidPage(titlePanel("Upload"),
                                      
                                      sidebarLayout(
                                        
                                        sidebarPanel(
                                          
                                          fileInput('upload', 'Choose a file:', accept = c('.txt'))
                                          
                                        ),
                                        
                                        mainPanel(
                                          
                                          plotOutput('plot'),
                                          tableOutput('textArcs')
                                          
                                        )
                                      )
                            )
                   )
))