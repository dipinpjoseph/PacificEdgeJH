
shinyServer(function(input, output, session) {
  
  # Displaying glimpse of Dataset
  output$StrData <- renderPrint({
    glimpse(dat)
  })
  
  # Displaying dataset summary
  output$Summary <- renderPrint({
    summary(dat[input$field_name])
  })
  
  # Datatable generation
  output$RawData <- DT::renderDataTable({
    DT::datatable(data = dat)
  })
  
  # Missing value visualization using vis_miss()
  output$MissingValues <- renderPlot({
    vis_miss(dat, cluster = input$cluster) +
      labs(title = "Missing Values in the DataSet")
  })
  
  # Code snippet for Mosaic plot
  output$Mosaic <- renderPlot({
    formula <-
      as.formula(paste("~", paste(input$vars_mosaic, collapse = " + ")))
    vcd::mosaic(
      formula,
      data = dat,
      main = "Mosaic Plot for Categorical Variables" ,
      legend = TRUE,
      shade = TRUE
    )
  })
  
  # Boxplot generation
  output$Boxplot <- renderPlot({
    data <-
      scale(
        select(dat, choices_num, "Y"),
        center = input$standardise,
        scale = input$standardise
      )
    boxplot(
      x = data,
      use.cols = TRUE,
      notch = FALSE,
      varwidth = FALSE,
      horizontal = FALSE,
      outline = input$outliers,
      col = brewer.pal(n = dim(select(
        dat, choices_num, "Y"
      ))[2], name = "RdBu"),
      range = input$range,
      main = "Boxplots of Numerical Data"
    )
  })
  
  # Correlograms plot for numerical variables
  output$Corrgram <- renderPlot({
    corrgram(
      dat[input$corr_ip],
      order = "OLO",
      abs = input$abs,
      text.panel = panel.txt,
      main = "Correlograms between Data Columns"
    )
  })
  
  # ggpairs plot generation
  output$Pairs <- renderPlot({
    GGally::ggpairs(data = select(dat, choices_num, "Y")[input$pairs_ip], title = "Pairs in Dataset")
  })
  
  # Code for Rising Order Chart
  output$RisingChart <- renderPlot({
    num_data <-
      scale(x = dat[, input$rise_ip],
            center = TRUE,
            scale = input$scale_rising)
    mypalette <- rainbow(ncol(num_data))
    matplot(
      y = num_data,
      type = "l",
      xlab = "Observations",
      ylab = "Values",
      lty = 1,
      lwd = 1,
      col = mypalette,
      main = "Rising Order chart for Numerical Variables"
    )
    legend(
      legend = colnames(num_data),
      x = "topleft",
      y = "top",
      lty = 1,
      lwd = 1,
      col = mypalette,
      ncol = round(ncol(num_data) ^ 0.3)
    )
  })
  
  # PCS plot generation
  output$PCA <- renderPlot({
    ch <- as.formula(paste("~", paste(choices_num, collapse = " + ")))
    pca <- prcomp(ch, dat, center = TRUE, scale. = TRUE)
    plot(pca$x[, 1:2]) # plot only the first two principle components
    title("PCA for Sensor Columns")
  })
  
  # Snippet for generating Time Series Plot
  output$TimeSeries <- renderPlot({
    ch_time <- input$time_ip
    dat_time <- select(dat, ch_time, "Date")
    dat_time <- melt(dat_time, id.vars="Date")
      ggplot(dat_time, aes(Date,value, col=variable)) + 
      geom_point() + labs(x="Date Range",y="Values",title="Times Series Chart for Numerical Variables") + 
      stat_smooth() + theme_light()
  })
  
})