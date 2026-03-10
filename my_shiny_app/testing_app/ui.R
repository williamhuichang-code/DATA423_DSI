
shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("Visualising a dataset"),
    
    tabsetPanel(
      tabPanel("Categorical",
               h3("Dataset - MASS::Titanic"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId = "SummaryA1"),
                          verbatimTextOutput(outputId = "SummaryA2")
                 ),
                 tabPanel("Raw Data",
                          DT::dataTableOutput(outputId = "titanic")
                 ),
                 tabPanel("Visualisation",
                          selectizeInput(inputId = "VariablesA", label = "Show variables:", choices = choicesA, multiple = TRUE, selected = choicesA),
                          withSpinner(
                            plotOutput(outputId = "Mosaic")
                          )
                 )
               )
      ),
      tabPanel("Numeric",
               h3("Dataset - datasets::airquality"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId = "SummaryB1"),
                          verbatimTextOutput(outputId = "SummaryB2")
                 ),
                 tabPanel("Raw Data",
                          DT::dataTableOutput(outputId = "airquality")
                 ),
                 tabPanel("Visualisation", 
                          withSpinner(
                            plotOutput(outputId = "Boxplot")
                          ),
                          checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                          checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                          sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                          hr(),
                          withSpinner(
                            plotOutput(outputId = "Corrgram")
                          ),
                          checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                          selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                          selectInput(inputId = "Group", label = "Grouping method", choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), selected = "OLO"),
                          hr(),
                          withSpinner(
                            plotOutput(outputId = "Missing")
                          ),
                          checkboxInput(inputId = "cluster", label = "Cluster missingness", value = FALSE),
                          hr(),
                          withSpinner(
                            plotOutput(outputId = "Pairs")
                          )
                 )
               )
      ),
      tabPanel("Temporal",
               h3("Dataset - datasets::EuStockMarkets"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId = "SummaryC1"),
                          verbatimTextOutput(outputId = "SummaryC2")
                 ),
                 tabPanel("Visualisation", 
                          withSpinner(
                            plotOutput(outputId = "TimeSeries")
                          ),
                          withSpinner(
                            plotOutput(outputId = "AutoCorr")
                          )
                 )
               )
      ),
      tabPanel("Textual",
               h3("Dataset - janeaustiner, Pride & Prejudice"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId = "SummaryD")
                    ),
                 tabPanel("Visualisation", 
                          withSpinner(
                            plotOutput(outputId = "Sentiment")
                            ),
                          withSpinner(
                            wordcloud2Output(outputId = "Cloud")
                          )
                 )
               )
      ),
      tabPanel("Spectra",
               h3("Dataset - pls::gasoline"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId = "SummaryE")
                 ),
                 tabPanel("Visualisation", 
                          withSpinner(
                            plotOutput(outputId = "Spectra", height = "600px")
                          ),
                          withSpinner(
                            plotOutput(outputId = "StanDevSpectra", height = "600px")
                          )
                 )
               )
      ),
      tabPanel("Mixed numeric - categorical",
               h3("Dataset - datasets::iris"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId = "SummaryF")
                 ),
                 tabPanel("Raw Data", 
                          DT::dataTableOutput(outputId = "iris")
                 ),
                 tabPanel("Visualisation", 
                          withSpinner(
                            plotOutput(outputId = "tableplot")
                          ),
                          hr(),
                          withSpinner(
                            plotOutput(outputId = "MixedPairs")
                          ),
                          hr(),
                          withSpinner(
                            plotOutput(outputId = "Corrgram2")
                          ),
                          checkboxInput(inputId = "abs2", label = "Uses absolute correlation", value = TRUE),
                          selectInput(inputId = "CorrMeth2", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                          selectInput(inputId = "Group2", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                 )
               )
      ),
      tabPanel("Spatial",
               leafletOutput("mymap"),
               p(),
               actionButton("recalc", "New students"),
               selectInput("provider","Provider", choices=providers, selected="OpenStreetMap")
      ),
      tabPanel("Graph",
               titlePanel("Data429 Topics"),
               checkboxInput("physics", label = "Use physics", value = FALSE),
               checkboxInput("smooth", label = "Floppy", value = FALSE),
               visNetworkOutput("Plot", height = "700px")
      )
    )
  )
)
