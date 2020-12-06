# Loading UI specific libraries
library(shinyjs)
library(shinyWidgets)
library(shinythemes)

shinyUI(fluidPage(
    theme = shinytheme("lumen"),
    h1(strong("Outlier Detection - SaratogaHouses"), align = "center"),
    h4(strong("Dipin P Joseph - 72746678"), align = "center"),
    br(),
    
    tabsetPanel(
        tabPanel(
            "Summary",
            tabsetPanel(
                tabPanel(
                    "Info",
                    br(),
                    h3(strong("Dataset Description"), align = "center"),
                    br(),
                    
                    column(width = 2),
                    column(
                        width = 8,
                        h4(strong("AIM:")),
                        strong(
                            "The Shiny application mainly focuses on outlier detection on the given dataset. This work is done as part of course - 'Data Science in Industry'(DATA423-20S1)."
                        ),
                        h4(strong("DATASET:")),
                        strong("Dataset under consideration - SaratogaHouses."),
                        br(),
                        strong(
                            "'SaratogaHouses' dataset is part of R package 'mosaicData' which is available on R's environment. It is a collection of information about 1728 houses in 'Saratoga County, New York' in 2006. The data has 16 house related variables. The dataset is created by 'Candice Corvetti'. More details about 'SaratogaHouses' can be found on https://rdrr.io/cran/mosaicData/man/SaratogaHouses.html"
                        ),
                        h4(strong("APPROACH:")),
                        strong(
                            "Outliers/Novelties were detected with help of various techniques and visualizations."
                        ),
                        br(),br(),
                        strong("Methods used in Raw Data,"),
                        br(),
                        strong("5-point summary"),
                        br(),
                        strong("Histogram"),
                        br(),
                        strong("Z-score"),
                        br(),
                        strong("Scatter Plots"),
                        br(),
                        strong("Bag Plots"),
                        br(),
                        strong("Mosaic Plots"),
                        br(),
                        br(),
                        strong("Methods used in Train/Test split Data,"),
                        br(),
                        strong("Box Plots"),
                        br(),
                        strong("Yeo-Johnson power transformation"),
                        br(),
                        strong("Density Plots"),
                        br(),
                        strong("Mahalanobis Distance"),
                        br(),
                        strong("Cook's Distance"),
                        br(),
                        strong("DBScan"),
                        br(),
                        strong("Local Outlier Factors"),
                        br(),
                        strong("Support Vector Machines"),
                        br(),
                        br(),
                    ),
                    column(width = 2)
                ),
                tabPanel(
                    "Glimpse",
                    column(width = 2),
                    column(
                        width = 8,
                        br(),
                        h3(strong("Variable Types and Sample Values"), align = "center"),
                        br(),
                        strong(
                            "The dataset consists of 1728 observations and 16 variables. First 10 variables belongs to numeric class and remaining are part of nominal class. "
                        ),
                        br(),br(),br(),
                        withSpinner(verbatimTextOutput("Glimpse")),
                    ),
                ),
                tabPanel(
                    "Summary",
                    column(width = 2),
                    column(
                        width = 8,
                        br(),
                        h3(strong("Summary Statistics for Variables"), align = "center"),
                        br(),
                        strong(
                            "Presence of outliers can be confirmed by observing maximum and minimum values of variables. By looking into the non-negative minimum values of numeric variables and their possible range of maximum values chances of systematic outliers could be ruled out. Same applies for categorical variables."
                        ),
                        br(),br(),br(),
                        withSpinner(verbatimTextOutput("Summary")),
                    ),
                    
                ),
                tabPanel(
                    "Raw Data",
                    br(),
                    h3(strong("Dataset Representation"), align = "center"),
                    br(),
                    strong(
                        "The whole dataset is shown in tabular form with pagination, sort and search capabilities. With help of sort functionality each variable's extreme points was analyzed. "
                    ),
                    br(),
                    br(),
                    withSpinner(DT::dataTableOutput(outputId = "RawData")),
                    br(),

                )
                
            )
        ),
        tabPanel(
            "Pre-Processing",
            tabsetPanel(
                tabPanel(
                    "Missing Values",
                    column(width = 2),
                    column(
                        width = 8,
                        br(),
                        h3(strong("Visualization of Missing Values"), align = "center"),
                        br(),
                        withSpinner(plotOutput(outputId = "MissingValues")),
                        br(),
                        strong(
                            "Missing values were represented using 'visdat' library. From the visualization no missingness was observed in the dataset. Hence any NA resolving step is ommitted."
                        ),
                    )
                ),
                tabPanel(
                    "Test Data",
                    br(),
                    h3(strong("Test Data after Test/Train Split"), align = "center"),
                    br(),
                    strong(
                        "Entire dataset was split on ratio 0.7. 70% data is given for training and remaining for testing. The split was done randomly."
                    ),
                    br(),br(),br(),
                    DT::dataTableOutput(outputId = "te_dt"),
                    br(),
                ),
                tabPanel(
                    "YJ tansformed Test Data",
                    br(),
                    h3(strong(
                        "Test Data after Yeo-Johnson Transformation"
                    ), align = "center"),
                    br(),
                    strong(
                        "Most of the outlier detection methods work along with normally distributed data. Here Yeo-Johnson power transformation was used to make variables more normally distributed."
                    ),
                    br(),br(),br(),
                    withSpinner(DT::dataTableOutput(outputId = "te_yj")),
                    br(),
                )
            )
        ),
        tabPanel(
            "Univariate Outliers",
            tabsetPanel(
                tabPanel(
                    "Density Plot",
                    fluidRow(
                        br(),
                        h3(
                            strong("Denisty Visualization for Numeric Variables in Test Data"),
                            align = "center"
                        ),
                        br(),
                        column(
                            width = 2,
                            br(),
                            br(),
                            strong("Filter"),
                            br(),
                            pickerInput(
                                inputId = "den_ip",
                                label = "",
                                choices = cols_num_all
                            )
                        ),
                        column(width = 5,
                               withSpinner(plotlyOutput(outputId = "Density"))),
                        column(width = 5,
                               withSpinner(plotlyOutput(outputId = "YJT")), br(), br(),),
                        column(width = 2),
                        strong(
                            "For the density plots it was oberved that power transformation makes data more normally distributed. Hence, transformed variables are fed into sensitive methods."
                        )
                    )
                ),
                tabPanel(
                    "Box Plot and Stats",
                    fluidRow(
                        column(
                            width = 2,
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            strong("Filter"),
                            br(),
                            br(),
                            checkboxInput(
                                inputId = "norm_dist",
                                label = "Transformed",
                                value = TRUE
                            ),
                            checkboxInput(
                                inputId = "standardise",
                                label = "Standardized",
                                value = TRUE
                            ),
                            checkboxInput(
                                inputId = "notch",
                                label = "Notches",
                                value = FALSE
                            ),
                            # Not possible in plotly at the moment
                            # sliderInput(
                            #     inputId = "range",
                            #     label = "IQR Multiplier",
                            #     min = 0,
                            #     max = 5,
                            #     step = 0.1,
                            #     value = 1.5
                            # )
                        ),
                        column(
                            width = 10,
                            br(),
                            h3(strong("Boxplots of Numeric Variables for 1.5*IQR"), align =
                                   "center"),
                            br(),
                            withSpinner(plotlyOutput(outputId = "Boxplot")),
                            br(),
                            strong(
                                "Boxplots is a visual representation of 5-point summary statistics. In visualization, any point outside whiskers((Min/Max)-/+1.5*IQR) is a suspected outlier. As an additional information, a dynamic table which shows outliers based on 5-point summary was created. "
                            ),
                            br(),
                        ),
                        br(),
                        
                        br(),
                        br(),
                        
                    ),
                    h3(strong("Outliers based on 5-Point Summary"), align =
                           "center"),
                    br(),
                    br(),
                    withSpinner(DT::dataTableOutput(outputId = "Five_Point")),
                    br(),
                    br(),
                ),
                tabPanel("Histogram",            br(),
                         fluidRow(
                             column(
                                 width = 2,
                                 br(),
                                 br(),
                                 strong("Filter"),
                                 br(),
                                 br(),
                                 pickerInput(inputId = "hist_ip",
                                             choices = cols)
                             ),
                             column(
                                 width = 10,
                                 br(),
                                 h3(
                                     strong("Histogram for Variables in Raw Data"), align =
                                                "center"
                                 ),
                                 br(),
                                 withSpinner(plotlyOutput(outputId = "Hist")),
                                 br(),
                                 strong(
                                     "A histogram is a frequency distribution of given data. It's major purpose here is to get a visual idea about skewness, range, value distribution of each numeric variable. "
                                 ),
                             )
                         )),
                tabPanel(
                    "Z-Score",
                    br(),
                    h4(strong("Outlier Detection with Z-Score for Raw Data"), align =
                           "center"),
                    br(),
                    strong(
                        "Z-score is a measure of relationship between an observation with mean and standard deviation of group of observations. Usually, a z-score outside (-3,+3) is considered as a novelty. The above table points out observations that could be considered as outliers based on this approach. "
                    ),
                    br(),
                    br(),
                    withSpinner(DT::dataTableOutput(outputId = "z_out")),
                )
            ),
            
        ),
        tabPanel("Bivariate Outliers",
                 tabsetPanel(
                     tabPanel("Scatter Plot", fluidRow(
                         column(
                             width = 2,
                             br(),
                             br(),
                             br(),
                             br(),
                             strong("Filter"),
                             br(),
                             br(),
                             pickerInput(inputId = "sc_ip1",
                                         choices = cols_num_all),
                             pickerInput(
                                 inputId = "sc_ip2",
                                 choices = cols_num_all,
                                 selected = cols_num_all[2]
                             )
                         ),
                         column(
                             width = 10,
                             br(),
                             h3(strong(
                                 "Scatter Plot for Numeric Variables in Raw Data"
                             ), align =
                                 "center"),
                             br(),
                             withSpinner(plotlyOutput(outputId = "Scatter")),
                             br(),
                             strong(
                                 "From the scatter plot one can manually identify outliers by considering two vairables."
                             )
                         )
                     )),
                     tabPanel("Bag Plot",
                              fluidRow(
                                  column(
                                      width = 2,
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      strong("Filter"),
                                      br(),
                                      br(),
                                      pickerInput(inputId = "bag_ip1",
                                                  choices = cols_num_all),
                                      pickerInput(
                                          inputId = "bag_ip2",
                                          choices = cols_num_all,
                                          selected = cols_num_all[3]
                                      )
                                  ),
                                  column(
                                      width = 10,
                                      br(),
                                      h3(strong("Bag Plot for Numeric Variables in Raw Data"), align =
                                             "center"),
                                      br(),
                                      withSpinner(plotOutput(outputId = "Bag", hover = "bag_hover")),
                                      verbatimTextOutput("Bag_info"),
                                      br(),
                                      br(),
                                      strong(
                                          "Bag plot is an improved version of box plot capable of finding outliers in 2/3 dimension data. Using package aplpack::bagplot, it was possible to visualize and fetch outliers in selected variables."
                                      ),
                                      br(),
                                      h3(
                                          strong("Tabular Representation of Detected Outliers using BagPlot"),
                                          align = "center"
                                      ),
                                      br(),
                                      DT::dataTableOutput(outputId = "Bag_Out"),
                                      br(),
                                      br()
                                  )
                              ))
                     
                 ),),
        tabPanel("Multivariate Outliers",
                 tabsetPanel(
                     tabPanel("Mosaic Plot(Nominal Data)",
                              fluidRow(
                                  column(
                                      width = 3,
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      strong("Filter"),
                                      br(),
                                      multiInput(
                                          inputId = "vars_mosaic",
                                          label = "",
                                          choices = cols_cat,
                                          selected = cols_cat[1:2]
                                      )
                                  ),
                                  column(
                                      width = 9,
                                      br(),
                                      h3(
                                          strong("Mosaic Plot for Categorical Variables in Raw Data"),
                                          align =
                                              "center"
                                      ),
                                      br(),
                                      plotOutput(outputId = "Mosaic"),
                                      br(),
                                      strong(
                                          "Mosaic plot is a graphical representation of frequencies of nominal data variables. If the area of intersection is smaller it suggests possibility of novelty."
                                      )
                                  )
                              )),
                     tabPanel(
                         "Mahalanobis Dist(Numeric Data)",
                         br(),
                         h3(strong(
                             "Outlier Detection with Mahalanobis Distance"
                         ), align =
                             "center"),
                         br(),
                         withSpinner(plotlyOutput(outputId = "M_dist")),
                         br(),
                         br(),
                         br(),
                         strong(
                             "In this method distance between points and distributions are calculated and assumes outlier observations are the distances greater than a threshold value(97.5% - optimum value for this case). The outlier observations were found and displayed below in tabular form."
                         ),
                         br(),
                         br(),
                         br(),
                         DT::dataTableOutput(outputId = "Mh_Out"),
                         br(),
                         br(),
                     )
                 )),
        tabPanel(
            "Model Based Outliers",
            tabsetPanel(
                tabPanel(
                    "Cook's Distance",
                    br(),
                    h3(strong("Outlier Detection with Cook's Distance"), align =
                           "center"),
                    br(),
                    withSpinner(plotlyOutput(outputId = "Cook")),
                    br(),
                    br(),
                    br(),
                    strong(
                        "Cook's distance is a measure of an observation's influence to change regression model. Here, we are calculating distance wrt linear model and assumes observations with distances greater than 4*mean value is highly influential. These observations are recorded as table below, "
                    ),
                    br(),
                    br(),
                    br(),
                    DT::dataTableOutput(outputId = "Ck_Out"),
                    br(),
                    br(),
                ),
                tabPanel(
                    "DBScan",
                    h3(
                        strong("Outlier detection based on Density Based Clustering - DBScan"),
                        align = "center"
                    ),
                    h4(strong("1-NN Distance Plot", align = "center")),
                    withSpinner(plotOutput(outputId = "Knn")),
                    br(),
                    strong(
                        "In density based clustering an outlier/noise is identified as the points that lack minimum neighbours(4) in a fixed radius. The above plot of 1-Nearest Neighbour gives the value of 25000(by elbow method) as eps(epsilon-radius of neighbourhood) parameter for dbscan. The noises found in DBScan is represented below."
                    ),
                    br(),
                    br(),
                    br(),
                    DT::dataTableOutput(outputId = "Db_out"),
                    br(),
                    br(),
                ),
                tabPanel(
                    "LOF",
                    h3(strong("Local Outlier Factors"), align = "center"),
                    br(),
                    strong(
                        "In this algorithm, local density of a point is compared to that of it's neighbour and if the local density is way less then the point is classified as an outlier. Outlier observations found using this method is shown below."
                    ),
                    br(),
                    br(),
                    br(),
                    DT::dataTableOutput(outputId = "LOF")
                ),
                tabPanel(
                    "Support Vector Machines",
                    fluidRow(
                        br(),
                        h3(
                            strong("Outlier detection using One-Class Support Vector Machines"),
                            align = "center"
                        ),
                        br(),
                        column(
                            width = 2,
                            br(),
                            strong("Filter"),
                            br(),
                            br(),
                            pickerInput(inputId = "svm_ip1",
                                        choices = cols_num_all),
                            pickerInput(
                                inputId = "svm_ip2",
                                choices = cols_num_all,
                                selected = cols_num_all[3]
                            )
                        ),
                        column(
                            width = 10,
                            withSpinner(plotlyOutput(outputId = "SVM_Plot")),
                            br(),
                            br(),
                            strong(
                                "One-Class SVM is an unsupervised learning algorithm utilized for novelty detection. On training it develop a function that can distinguish regions of high density points and low density points. Test data is applied to this model and points in low density regions is shown as outliers. As a preliminary step, outliers in training data was found by z-score method to calculate parameter 'nu'. The outlier observations are employed in the table below."
                            ),
                            br(),
                            br()
                        )
                    ),
                    
                    DT::dataTableOutput(outputId = "SVM_Out")
                )
            ),
        ),
        tabPanel(
            "Robust Methods",
            br(),
            h3(strong("Outlier Insensitive Methods"), align = "center"),
            br(),
            strong(
                "Robust methods are defined as the modelling techniques that are tolerant to outlier presence. Above is a list of classification and regression methods from CARET with ability to tolerate outliers."
            ),
            br(),br(),br(),
            DT::dataTableOutput(outputId = "Rb_M"),
        ),
        tabPanel(
            "Conclusion",
            br(),
            h3(strong("Conclusion"), align = "center"),
            br(),
            br(),
            strong(
                "In this work, various outlier detection methods were utilized to detect novelties in dataset 'SaratogaHouses'. An intersection of statistical outliers from various methods was done programmatically. Table below shows the observations which have most probability to be an outlier in Test Data (Intersection between results of 5-Point Summary, Mahalanobis Distance, Cook's Distance, DBScan, LOF, 1-class SVM.)."
            ),
            br(),
            br(),
            br(),
            DT::dataTableOutput(outputId = "Out_Final"),
            br(),br(),
            strong(
                "For detecting outliers in Raw Data and investigating systematicness methods Box Plot, Histogram, Bag Plot, Z-Score, 5 Point Summary were used. There was no traces of systematic outlier in data based on variable range, values, relationships and possibility. Table below shows the observations which have chance to be an outlier in Raw Data (Intersection between Z-Score and 5-Point Summary observations.)."
            ),
            br(),
            br(),
            DT::dataTableOutput(outputId = "Out_Raw"),
            br(),
            br(),
        )
    )
))
