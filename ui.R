# DATA423 - Data Science in Industry
# Assignment 2 - Xiaoxi Guo
# UI

domChoices <- c("l","f","r","t","i","p")
shinyUI(
    fluidPage(
        
        titlePanel(h3("Assignment 2 - Xiaoxi Guo")),
        h4(hr(),"Dataset - Ass2Data"),
        splitLayout(
            sliderInput(inputId = "Varthre", label = "Missingness Threshold - Variables: ", min = 0.1, max = 100, step = 1, value = 50),
            sliderInput(inputId = "Obsthre", label = "Missingness Threshold - Observations: ", min = 0.1, max = 100, step = 1, value = 42)
            ),
        
        splitLayout(
            h4("Variables to remove: "),
            h4("Observations to remove: ")
            ),
        
        splitLayout(
            textOutput("VarRemove"),
            textOutput("ObsRemove")
        ),
        
        hr(),
        h4("Final dimensions are: "),
        textOutput("Finaldim"),
        hr(),
        

        
        tabsetPanel(
            tabPanel(h4("DATA"),
                     tabsetPanel(
                         
                         tabPanel(
                             h4("Datatable"),
                             #DT::dataTableOutput(outputId = "cldat()"),
                             sidebarLayout(
                                 sidebarPanel(
                                     checkboxInput("rownames", "Show row names", value= T),
                                     checkboxInput("order", "Column ordering", value=T),
                                     #selectInput("selection", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                                     selectInput("filter", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                                     selectInput("dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices),
                                     
                                 ),
                                 
                                 mainPanel(
                                     DT::dataTableOutput("tableX"),
                                 )
                             )
                         ),
                        
                         tabPanel(h4("Summary"),
                                verbatimTextOutput(outputId = "SummaryA1")
                                      ),

                         tabPanel(h4("DfSummary"),
                                withSpinner(tableOutput(outputId = "SummaryA2")
                                    ))
                        
                     )
            ),
            
            tabPanel(h4("MISSING"),
                     tabsetPanel(
                         tabPanel(h4("Missing-value Chart"),
                                  withSpinner(
                                      plotOutput(outputId = "Missing1")
                                  )
                         ),
                         
                         tabPanel(h4("Missing-value Chart(cluster)"),
                                  withSpinner(
                                      plotOutput(outputId = "Missing2")
                                  ),
                                  checkboxInput(inputId = "cluster", label = "Clusters missingness",value = T)
                         ),
                        
                          tabPanel(h4("Missing-value Combination Chart"),
                                   checkboxGroupInput(inputId = "VariablesA", label = "Variables: ", choices = choices, selected = choices[c(2:4)], inline = T),
                                  withSpinner(
                                      plotOutput(outputId = "Missing3")
                                  )
                         ),
                
                         tabPanel(h4("Missing-value Correlations"),
                                  withSpinner(
                                      plotOutput(outputId = "Missing4")
                                  ),
                                  checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                                  selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                                  selectInput(inputId = "Group", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                         ),

                         tabPanel(h4("Tree"),
                                  withSpinner(
                                      plotOutput(outputId = "Missing5")
                                  ))                                                  
                         
                         
                     )
            ),
            
            tabPanel(h4("BOXPLOT"),
                     checkboxGroupInput(inputId = "VariablesB", label = "Variables:", choices = choicesB, selected = choicesB[-3], inline = T),
                     withSpinner(
                         plotOutput(outputId = "Boxplot")
                     ),
                     checkboxInput(inputId = "standardise", label = "Show standardized", value = TRUE),
                     checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                     sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
            ),
            
            tabPanel(h4("CORRELATIONS"),
                     checkboxGroupInput(inputId = "VariablesC", label = "Variables:", choices = choicesB, selected = choicesB[-3], inline = T),
                     withSpinner(
                         plotOutput(outputId = "Corrgram")
                     ),
                     checkboxInput(inputId = "abs1", label = "Uses absolute correlation", value = TRUE),
                     selectInput(inputId = "CorrMeth1", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                     selectInput(inputId = "Group1", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
            ),

            tabPanel(h4("MODEL"),
                     tabsetPanel(
                         tabPanel(h4("Model-based Outliers"),
                                  withSpinner(
                                      plotOutput(outputId = "Accuracy")
                                  )
                         ),
                         
                         tabPanel(h4("Residuals Boxplot"),
                                  sliderInput(inputId = "Coef", label ="IQR Multpile", min=0, max=5,step=0.1,value = 1.5),
                                  withSpinner(
                                      plotOutput(outputId = "Residuals")
                                  )
                         )
                         
                        
                         
                     )
            
                     
                    
            )
            
            
            
        )
    )
)
