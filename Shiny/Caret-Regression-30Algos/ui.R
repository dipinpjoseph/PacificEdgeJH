shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Dipin P Joseph (72746678)"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             
             tabsetPanel(
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NullGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe")
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("naomit","dummy")),
                                 bsTooltip(id = "GlmnetPreprocess", 
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "PlsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RpartPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe")
               ),
            
               ######################################################### maintenance point ####################################################
               
               
               tabPanel("Random Regression Model - RF",
                        verbatimTextOutput(outputId = "RFModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RFPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","knnimpute")),
                                 bsTooltip(id = "RFPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RFGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RFGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RFMetrics"),
                        hr(),
                        plotOutput(outputId = "RFModelPlots"),
                        verbatimTextOutput(outputId = "RFRecipe")
               ),
               tabPanel("Linear Regression - lm",
                        verbatimTextOutput(outputId = "LMModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "LMPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","knnimpute")),
                                 bsTooltip(id = "LMPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "LMGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "LMGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "LMMetrics"),
                        hr(),
                        #plotOutput(outputId = "LMModelPlots"),
                        verbatimTextOutput(outputId = "LMRecipe")
               ),
               tabPanel("svmRadial",
                        verbatimTextOutput(outputId = "SRModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "SRPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","knnimpute")),
                                 bsTooltip(id = "SRPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "SRGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "SRGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "SRMetrics"),
                        hr(),
                        plotOutput(outputId = "SRModelPlots"),
                        verbatimTextOutput(outputId = "SRRecipe")
               ),
               tabPanel("cubist",
                        verbatimTextOutput(outputId = "CBModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "CBPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","knnimpute")),
                                 bsTooltip(id = "CBPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "CBGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "CBGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "CBMetrics"),
                        hr(),
                        plotOutput(outputId = "CBModelPlots"),
                        verbatimTextOutput(outputId = "CBRecipe")
               ),
               tabPanel("Bagged MARS",
                        verbatimTextOutput(outputId = "BEModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "BEPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "BEPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "BEGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BEGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BEMetrics"),
                        hr(),
                        plotOutput(outputId = "BEModelPlots"),
                        verbatimTextOutput(outputId = "BERecipe")
               ),
               tabPanel("eXtreme Gradient Boosting",
                        verbatimTextOutput(outputId = "XTModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "XTPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "XTPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "XTGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "XTGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "XTMetrics"),
                        hr(),
                        plotOutput(outputId = "XTModelPlots"),
                        verbatimTextOutput(outputId = "XTRecipe")
               ),
               tabPanel("Model Averaged Neural Network",
                        verbatimTextOutput(outputId = "MANNModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "MANNPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "MANNPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "MANNGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "MANNGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "MANNMetrics"),
                        hr(),
                        plotOutput(outputId = "MANNModelPlots"),
                        verbatimTextOutput(outputId = "MANNRecipe")
               ),
               tabPanel("The Bayesian lasso",
                        verbatimTextOutput(outputId = "TBLModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "TBLPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "TBLPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "TBLGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "TBLGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "TBLMetrics"),
                        hr(),
                        plotOutput(outputId = "TBLModelPlots"),
                        verbatimTextOutput(outputId = "TBLRecipe")
               ),
               tabPanel("Bayesian Regularized Neural Networks",
                        verbatimTextOutput(outputId = "BRNNModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "BRNNPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "BRNNPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "BRNNGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BRNNGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BRNNMetrics"),
                        hr(),
                        plotOutput(outputId = "BRNNModelPlots"),
                        verbatimTextOutput(outputId = "BRNNRecipe")
               ),
               tabPanel("Elasticnet",
                        verbatimTextOutput(outputId = "ENTModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "ENTPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "ENTPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "ENTGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "ENTGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "ENTMetrics"),
                        hr(),
                        plotOutput(outputId = "ENTModelPlots"),
                        verbatimTextOutput(outputId = "ENTRecipe")
               ),
               tabPanel("k-Nearest Neighbors",
                        verbatimTextOutput(outputId = "KNNModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "KNNPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                                 bsTooltip(id = "KNNPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "KNNGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "KNNGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "KNNMetrics"),
                        hr(),
                        plotOutput(outputId = "KNNModelPlots"),
                        verbatimTextOutput(outputId = "KNNRecipe")
               ),
               tabPanel("Bayesian Additive Regression Trees",
                        verbatimTextOutput(outputId = "bartMachineModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "bartMachinePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "bartMachinePreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "bartMachineGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "bartMachineGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bartMachineMetrics"),
                        hr(),
                        plotOutput(outputId = "bartMachineModelPlots"),
                        verbatimTextOutput(outputId = "bartMachineRecipe")
               ),
               tabPanel("Ridge Regression with Variable Selection",
                        verbatimTextOutput(outputId = "fobaModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "fobaPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "fobaPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "fobaGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "fobaGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "fobaMetrics"),
                        hr(),
                        plotOutput(outputId = "fobaModelPlots"),
                        verbatimTextOutput(outputId = "fobaRecipe")
               ),
               tabPanel("Stochastic Gradient Boosting",
                        verbatimTextOutput(outputId = "gbmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gbmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute")),
                                 bsTooltip(id = "gbmPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gbmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gbmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gbmMetrics"),
                        hr(),
                        plotOutput(outputId = "gbmModelPlots"),
                        verbatimTextOutput(outputId = "gbmRecipe")
               ),
               tabPanel("Model Tree	RWeka ",
                        verbatimTextOutput(outputId = "M5ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "M5Preprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","YeoJohnson","pls")),
                                 bsTooltip(id = "M5Preprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "M5Go", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "M5Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "M5Metrics"),
                        hr(),
                        plotOutput(outputId = "M5ModelPlots"),
                        verbatimTextOutput(outputId = "M5Recipe")
               ),
               tabPanel("eXtreme Gradient Boosting - Linear ",
                        verbatimTextOutput(outputId = "xgbLinearModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "xgbLinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","YeoJohnson","pls")),
                                 bsTooltip(id = "xgbLinearPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "xgbLinearGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "xgbLinearGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "xgbLinearMetrics"),
                        hr(),
                        plotOutput(outputId = "xgbLinearModelPlots"),
                        verbatimTextOutput(outputId = "xgbLinearRecipe")
               ),
               tabPanel("Multi-Step Adaptive MCP-Net",
                        verbatimTextOutput(outputId = "msaenetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "msaenetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","YeoJohnson","pls")),
                                 bsTooltip(id = "msaenetPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "msaenetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "msaenetGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "msaenetMetrics"),
                        hr(),
                        plotOutput(outputId = "msaenetModelPlots"),
                        verbatimTextOutput(outputId = "msaenetRecipe")
               ),
               tabPanel("Generalized Linear Model with Stepwise Feature Selection",
                        verbatimTextOutput(outputId = "glmStepAICModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "glmStepAICPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","YeoJohnson","pls")),
                                 bsTooltip(id = "glmStepAICPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "glmStepAICGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "glmStepAICGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "glmStepAICMetrics"),
                        hr(),

                        verbatimTextOutput(outputId = "glmStepAICRecipe")
               )
               
               
               
               
             )
             ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             plotOutput(outputId = "TestPlot")
    )
  )
))
