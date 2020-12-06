shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  ############################################################ NULL ########################################################
  
  
  
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  ##############################################################################  
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################  
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  
############################################################ GLMNET ########################################################
  
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ##############################################################################  
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  ############################################################################## 
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
############################################################ PLS ########################################################
  
  
    
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ##############################################################################  
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
############################################################ RPART ########################################################
  
  
    
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rpart")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ##############################################################################  
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel)
  })     
  

  
  
######################################################### maintenance point ####################################################
  
############################################################ RF ########################################################
  
  
  ##############################################################################  
  getRFRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RFPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RFGo,
    {
      library(randomForest)
      library(e1071)
      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRFRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rf")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$RFModelSummary0 <- renderText({
    description("rf")
  })
  
  ##############################################################################  
  output$RFMetrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RFRecipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })  
  
  ############################################################################## 
  output$RFModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })
  
  ############################################################################## 

  ############################################################ LM ########################################################
  
  
  ##############################################################################  
  getLMRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$LMPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$LMGo,
    {

      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getLMRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "lm")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$LMModelSummary0 <- renderText({
    description("lm")
  })
  
  ##############################################################################  
  output$LMMetrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$LMRecipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  
  
  ############################################################################## 
  #output$LMModelPlots <- renderPlot({
  #  req(models$lm)
  #  plot(models$lm)
  #})
  
  ############################################################################## 
  
  
  
  ############################################################ svmRadial ########################################################
  
  
  ##############################################################################  
  getSRRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$SRPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$SRGo,
    {
      library(kernlab)
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getSRRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "svmRadial")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$SRModelSummary0 <- renderText({
    description("svmRadial")
  })
  
  ##############################################################################  
  output$SRMetrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$SRRecipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  ############################################################################## 
  output$SRModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })
  
  ############################################################################## 
  
  ############################################################ cubist ########################################################
  
  
  ##############################################################################  
  getCBRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$CBPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$CBGo,
    {
      library(Cubist)
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getCBRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "cubist")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$CBModelSummary0 <- renderText({
    description("cubist")
  })
  
  ##############################################################################  
  output$CBMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$CBRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  ############################################################################## 
  output$CBModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  ############################################################################## 
  
  
  
  ############################################################ Bagged MARS ########################################################
  
  
  ##############################################################################  
  getBERecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BEPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$BEGo,
    {
      library(earth)
      method <- "bagEarth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getBERecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "bagEarth")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$BEModelSummary0 <- renderText({
    description("bagEarth")
  })
  
  ##############################################################################  
  output$BEMetrics <- renderTable({
    req(models$bagEarth)
    models$bagEarth$results[ which.min(models$bagEarth$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BERecipe <- renderPrint({
    req(models$bagEarth)
    models$bagEarth$recipe
  })  
  
  ############################################################################## 
  output$BEModelPlots <- renderPlot({
    req(models$bagEarth)
    plot(models$bagEarth)
  })
  
  ############################################################################## 
  
  ############################################################ eXtreme Gradient Boosting ########################################################
  
  
  ##############################################################################  
  getXTRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$XTPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$XTGo,
    {
      library(xgboost)
      method <- "xgbTree"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getXTRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "xgbTree")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$XTModelSummary0 <- renderText({
    description("xgbTree")
  })
  
  ##############################################################################  
  output$XTMetrics <- renderTable({
    req(models$xgbTree)
    models$xgbTree$results[ which.min(models$xgbTree$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$XTRecipe <- renderPrint({
    req(models$xgbTree)
    models$xgbTree$recipe
  })  
  
  ############################################################################## 
  output$XTModelPlots <- renderPlot({
    req(models$xgbTree)
    plot(models$xgbTree)
  })
  
  ############################################################################## 
  
  ############################################################ Model Averaged Neural Network ########################################################
  
  
  ##############################################################################  
  getMANNRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$MANNPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$MANNGo,
    {
      library(nnet)
      method <- "avNNet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getMANNRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "avNNet")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$MANNModelSummary0 <- renderText({
    description("avNNet")
  })
  
  ##############################################################################  
  output$MANNMetrics <- renderTable({
    req(models$avNNet)
    models$avNNet$results[ which.min(models$avNNet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$MANNRecipe <- renderPrint({
    req(models$avNNet)
    models$avNNet$recipe
  })  
  
  ############################################################################## 
  output$MANNModelPlots <- renderPlot({
    req(models$avNNet)
    plot(models$avNNet)
  })
  
  ############################################################################## 
  
  ############################################################ The Bayesian lasso ########################################################
  
  
  ##############################################################################  
  getTBLRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$TBLPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$TBLGo,
    {
      library(monomvn)
      method <- "blasso"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getTBLRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "blasso")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$TBLModelSummary0 <- renderText({
    description("blasso")
  })
  
  ##############################################################################  
  output$TBLMetrics <- renderTable({
    req(models$blasso)
    models$blasso$results[ which.min(models$blasso$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$TBLRecipe <- renderPrint({
    req(models$blasso)
    models$blasso$recipe
  })  
  
  ############################################################################## 
  output$TBLModelPlots <- renderPlot({
    req(models$blasso)
    plot(models$blasso)
  })
  
  ############################################################################## 
  ############################################################ Bayesian Regularized Neural Networks ########################################################
  
  
  ##############################################################################  
  getBRNNRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BRNNPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$BRNNGo,
    {
      library(brnn)
      method <- "brnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getBRNNRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "brnn")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$BRNNModelSummary0 <- renderText({
    description("brnn")
  })
  
  ##############################################################################  
  output$BRNNMetrics <- renderTable({
    req(models$brnn)
    models$brnn$results[ which.min(models$brnn$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BRNNRecipe <- renderPrint({
    req(models$brnn)
    models$brnn$recipe
  })  
  
  ############################################################################## 
  output$BRNNModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn)
  })
  
  ############################################################################## 
  
  ############################################################ Elastic Net ########################################################
  
  
  ##############################################################################  
  getENTRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$ENTPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$ENTGo,
    {
      library(elasticnet)
      method <- "enet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getENTRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "enet")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$ENTModelSummary0 <- renderText({
    description("enet")
  })
  
  ##############################################################################  
  output$ENTMetrics <- renderTable({
    req(models$enet)
    models$enet$results[ which.min(models$enet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$ENTRecipe <- renderPrint({
    req(models$enet)
    models$enet$recipe
  })  
  
  ############################################################################## 
  output$ENTModelPlots <- renderPlot({
    req(models$enet)
    plot(models$enet)
  })
  
  ############################################################################## 
  
  ############################################################ k-Nearest Neighbors ########################################################
  
  
  ##############################################################################  
  getKNNRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$KNNPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$KNNGo,
    {
      
      method <- "kknn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getKNNRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "kknn")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$KNNModelSummary0 <- renderText({
    description("kknn")
  })
  
  ##############################################################################  
  output$KNNMetrics <- renderTable({
    req(models$kknn)
    models$kknn$results[ which.min(models$kknn$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$KNNRecipe <- renderPrint({
    req(models$kknn)
    models$kknn$recipe
  })  
  
  ############################################################################## 
  output$KNNModelPlots <- renderPlot({
    req(models$kknn)
    plot(models$kknn)
  })
  
  ############################################################################## 
  
  ############################################################ Bayesian Additive Regression Trees ########################################################
  
  
  ##############################################################################  
  getbartMachineRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$bartMachinePreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$bartMachineGo,
    {
      library(bartMachine)
      method <- "bartMachine"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getbartMachineRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "bartMachine")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$bartMachineModelSummary0 <- renderText({
    description("bartMachine")
  })
  
  ##############################################################################  
  output$bartMachineMetrics <- renderTable({
    req(models$bartMachine)
    models$bartMachine$results[ which.min(models$bartMachine$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$bartMachineRecipe <- renderPrint({
    req(models$bartMachine)
    models$bartMachine$recipe
  })  
  
  ############################################################################## 
  output$bartMachineModelPlots <- renderPlot({
    req(models$bartMachine)
    plot(models$bartMachine)
  })
  
  ############################################################################## 

  ############################################################ Ridge Regression with Variable Selection ########################################################
  
  
  ##############################################################################  
  getfobaRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$fobaPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$fobaGo,
    {
      library(foba)
      method <- "foba"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getfobaRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "foba")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$fobaModelSummary0 <- renderText({
    description("foba")
  })
  
  ##############################################################################  
  output$fobaMetrics <- renderTable({
    req(models$foba)
    models$foba$results[ which.min(models$foba$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$fobaRecipe <- renderPrint({
    req(models$foba)
    models$foba$recipe
  })  
  
  ############################################################################## 
  output$fobaModelPlots <- renderPlot({
    req(models$foba)
    plot(models$foba)
  })
  
  ############################################################################## 

 ############################################################ Stochastic Gradient Boosting ########################################################
  
  
  ##############################################################################  
  getgbmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gbmPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$gbmGo,
    {
      library(gbm)
      library(plyr)
      method <- "gbm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgbmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "gbm")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$gbmModelSummary0 <- renderText({
    description("gbm")
  })
  
  ##############################################################################  
  output$gbmMetrics <- renderTable({
    req(models$gbm)
    models$gbm$results[ which.min(models$gbm$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$gbmRecipe <- renderPrint({
    req(models$gbm)
    models$gbm$recipe
  })  
  
  ############################################################################## 
  output$gbmModelPlots <- renderPlot({
    req(models$gbm)
    plot(models$gbm)
  })
  
  ############################################################################## 

  ############################################################ Model Tree	RWeka  ########################################################
  
  
  ##############################################################################  
  getM5Recipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$M5Preprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$M5Go,
    {
      library(RWeka)
      method <- "M5"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getM5Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "M5")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$M5ModelSummary0 <- renderText({
    description("M5")
  })
  
  ##############################################################################  
  output$M5Metrics <- renderTable({
    req(models$M5)
    models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$M5Recipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })  
  
  ############################################################################## 
  output$M5ModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })
  
  ############################################################################## 
   ############################################################ eXtreme Gradient Boosting - Linear  ########################################################
      
      
      ##############################################################################  
      getxgbLinearRecipe <- reactive({
        recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$xgbLinearPreprocess)
      })
      
      ##############################################################################
      observeEvent(
        input$xgbLinearGo,
        {
          library(xgboost)
          method <- "xgbLinear"
          models[[method]] <- NULL
          showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
          clus <- startMode(input$Parallel)
          tryCatch({
            models[[method]] <- caret::train(getxgbLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
            saveToRds(models[[method]], "xgbLinear")
          }, 
          finally = {
            removeNotification(id = method)
            stopMode(clus)
          })
        }
      )
      
      ############################################################################## 
      output$xgbLinearModelSummary0 <- renderText({
        description("xgbLinear")
      })
      
      ##############################################################################  
      output$xgbLinearMetrics <- renderTable({
        req(models$xgbLinear)
        models$xgbLinear$results[ which.min(models$xgbLinear$results[, "RMSE"]), ]
      })
      
      ############################################################################## 
      output$xgbLinearRecipe <- renderPrint({
        req(models$xgbLinear)
        models$xgbLinear$recipe
      })  
      
      ############################################################################## 
      output$xgbLinearModelPlots <- renderPlot({
        req(models$xgbLinear)
        plot(models$xgbLinear)
      })
      
      ############################################################################## 

     ############################################################ Multi-Step Adaptive MCP-Net  ########################################################
      
      
      ##############################################################################  
      getmsaenetRecipe <- reactive({
        recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$msaenetPreprocess)
      })
      
      ##############################################################################
      observeEvent(
        input$msaenetGo,
        {
          library(xgboost)
          method <- "msaenet"
          models[[method]] <- NULL
          showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
          clus <- startMode(input$Parallel)
          tryCatch({
            models[[method]] <- caret::train(getmsaenetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
            saveToRds(models[[method]], "msaenet")
          }, 
          finally = {
            removeNotification(id = method)
            stopMode(clus)
          })
        }
      )
      
      ############################################################################## 
      output$msaenetModelSummary0 <- renderText({
        description("msaenet")
      })
      
      ##############################################################################  
      output$msaenetMetrics <- renderTable({
        req(models$msaenet)
        models$msaenet$results[ which.min(models$msaenet$results[, "RMSE"]), ]
      })
      
      ############################################################################## 
      output$msaenetRecipe <- renderPrint({
        req(models$msaenet)
        models$msaenet$recipe
      })  
      
      ############################################################################## 
      output$msaenetModelPlots <- renderPlot({
        req(models$msaenet)
        plot(models$msaenet)
      })
      
      ############################################################################## 
  
      ############################################################ Generalized Linear Model with Stepwise Feature Selection ########################################################
      
      
      ##############################################################################  
      getglmStepAICRecipe <- reactive({
        recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$glmStepAICPreprocess)
      })
      
      ##############################################################################
      observeEvent(
        input$glmStepAICGo,
        {
          library(MASS)
          method <- "glmStepAIC"
          models[[method]] <- NULL
          showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
          clus <- startMode(input$Parallel)
          tryCatch({
            models[[method]] <- caret::train(getglmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
            saveToRds(models[[method]], "glmStepAIC")
          }, 
          finally = {
            removeNotification(id = method)
            stopMode(clus)
          })
        }
      )
      
      ############################################################################## 
      output$glmStepAICModelSummary0 <- renderText({
        description("glmStepAIC")
      })
      
      ##############################################################################  
      output$glmStepAICMetrics <- renderTable({
        req(models$glmStepAIC)
        models$glmStepAIC$results[ which.min(models$glmStepAIC$results[, "RMSE"]), ]
      })
      
      ############################################################################## 
      output$glmStepAICRecipe <- renderPrint({
        req(models$glmStepAIC)
        models$glmStepAIC$recipe
      })  
      
      
      ############################################################################## 
  
  
#####################################################################################################################  
  
  
    
  
  
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    print(results)
    NullModel <- "Null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)

    
})
