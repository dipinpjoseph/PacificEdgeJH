
shinyUI(fluidPage(
    useShinyjs(),
    titlePanel("Assignment 1 - Dipin P Joseph"),
    
    tabsetPanel(
        # Dataset SUmmary
        tabPanel(
            "Summary",
            sidebarLayout(
                sidebarPanel(
                    multiInput(
                        inputId = "field_name",
                        label = "Choose a set of Predictors",
                        choices = choices_all,
                        selected = choices_all[1:5]
                    ),
                ),
                
                mainPanel(verbatimTextOutput(outputId = "Summary"),),
            ),
            h4("Data at a Glance"),
            verbatimTextOutput(outputId = "StrData")
            
            
        ),
        # Datatable display
        tabPanel("Raw Data",
                 DT::dataTableOutput(outputId = "RawData")),
        tabPanel(
            "Visualization",
            tabsetPanel(
                # NA values plot
                tabPanel(
                    "Missing Values",
                    plotOutput(outputId = "MissingValues"),
                    checkboxInput(
                        inputId = "cluster",
                        label = "Cluster Missingness",
                        value = TRUE
                    ),
                ),
                # Mosaic plot for categorical variables
                tabPanel("Categorical",
                         tabPanel(
                             "Mosaic Plot",
                             sidebarLayout(sidebarPanel(
                                 multiInput(
                                     inputId = "vars_mosaic",
                                     label = "Choose a set of Categorical Variables",
                                     choices = choices_cat,
                                     selected = choices_cat[1:2]
                                 ),
                             ),
                             
                             mainPanel(plotOutput(outputId = "Mosaic")), ),
                         )),
                # Boxplot for numeric variables
                tabPanel(
                    "Numerical",
                    tabPanel(
                        "Plots",
                        plotOutput(outputId = "Boxplot"),
                        checkboxInput(
                            inputId = "standardise",
                            label = "Show standardized",
                            value = TRUE
                        ),
                        checkboxInput(
                            inputId = "outliers",
                            label = "Show outliers",
                            value = TRUE
                        ),
                        sliderInput(
                            inputId = "range",
                            label = "IQR Multiplier",
                            min = 0,
                            max = 5,
                            step = 0.1,
                            value = 1.5
                        ),
                        hr(),
                        # Correlograms for correlation matrices
                        multiInput(
                            inputId = "corr_ip",
                            label = "Choose a set of Variables",
                            choices = c(choices_num, "Y"),
                            selected = c(choices_num[1:3], "Y")
                        ),
                        plotOutput(outputId = "Corrgram"),
                        checkboxInput(
                            inputId = "abs",
                            label = "Grouping uses absolute correlation",
                            value = TRUE
                        ),
                        hr(),
                        # Pairs plot for variables
                        multiInput(
                            inputId = "pairs_ip",
                            label = "Choose a set of Variables",
                            choices = c(choices_num, "Y"),
                            selected = c(choices_num[1:3], "Y")
                        ),
                        plotOutput(outputId = "Pairs"),
                        hr(),
                        # Rising order chart 
                        multiInput(
                            inputId = "rise_ip",
                            label = "Choose a set of Variables",
                            choices = c(choices_num, "Y"),
                            selected = c(choices_num[1:3], "Y")
                        ),
                        checkboxInput(
                            inputId = "scale_rising",
                            label = "Show standardized",
                            value = TRUE
                        ),
                        plotOutput(outputId = "RisingChart"),
                        hr(),
                        # PCA plot
                        plotOutput(outputId = "PCA"),
                        hr(),
                        # TImeseries plot
                        multiInput(
                            inputId = "time_ip",
                            label = "Choose a set of Variables",
                            choices = c(choices_num, "Y"),
                            selected = c(choices_num[1:3], "Y")
                        ),
                        plotOutput(outputId = "TimeSeries")
                    )
                )
            )
        )
    )
))
