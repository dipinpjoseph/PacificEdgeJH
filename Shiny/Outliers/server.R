library(recipes)
library("RColorBrewer")


shinyServer(function(input, output) {
    # Displaying glimpse of Dataset
    output$Summary <- renderPrint({
        summary(SaratogaHouses)
    })
    
    # Displaying dataset summary
    output$Glimpse <- renderPrint({
        str(SaratogaHouses)
    })
    
    # Datatable generation
    output$RawData <- DT::renderDataTable({
        DT::datatable(data = SaratogaHouses, options = list(scrollX = T))
    })
    
    # Datatable generation YJ
    output$te_yj <- DT::renderDataTable({
        DT::datatable(data = yj_t, options = list(scrollX = T))
    })
    
    # Datatable generation Test
    output$te_dt <- DT::renderDataTable({
        DT::datatable(data = te, options = list(scrollX = T))
    })
    
    # Missing value visualization using vis_dat()
    output$MissingValues <- renderPlot({
        vis_dat(SaratogaHouses)
    })
    
    # Displaying density plot of variables
    output$Density <- renderPlotly({
        den_op = density(unlist(te[input$den_ip]))
        den <- data.frame(x = unlist(den_op$x),
                          y = unlist(den_op$y))
        xa <- list(title = paste(
            "Obs = ",
            den_op$n,
            "Bandwidth = ",
            round(den_op$bw, digits = 3)
        ))
        ya <- list(title = "Density")
        fig <- plot_ly(den, x = ~ x, y = ~ y)
        layout(
            add_lines(fig),
            title = "Density plot of non-transformed variable",
            yaxis = ya,
            xaxis = xa,
            margin = list(t = 80)
        )
        
    })
    
    # Displaying density plot of variables
    output$YJT <- renderPlotly({
        den_op = density(unlist(yj_t[input$den_ip]))
        den <- data.frame(x = unlist(den_op$x),
                          y = unlist(den_op$y))
        xa <- list(title = paste(
            "Obs = ",
            den_op$n,
            "Bandwidth = ",
            round(den_op$bw, digits = 3)
        ))
        ya <- list(title = "Density")
        fig <- plot_ly(den, x = ~ x, y = ~ y)
        layout(
            add_lines(fig),
            title = "Density plot of transformed variable",
            yaxis = ya,
            xaxis = xa,
            margin = list(t = 80)
        )
    })
    
    # Boxplot generation
    output$Boxplot <- renderPlotly({
        box_dat = select(te, cols_num_all)
        if (input$norm_dist) {
            box_dat = yj_t
        }
        box_dat <-
            data.frame(scale(
                box_dat,
                center = input$standardise,
                scale = input$standardise
            ))
        bp <- plot_ly(data = box_dat, type = 'box')
        for (k in 1:length(cols_num_all)) {
            dfk <- data.frame(y = box_dat[[cols_num_all[k]]])
            bp <-
                add_trace(
                    bp,
                    y = ~ y,
                    data = dfk,
                    name = cols_num_all[k],
                    notched = input$notch,
                    range = input$range,
                    text =  ~ y
                )
        }
        
        layout(bp,
               yaxis = list(title = "Value"),
               margin = list(t = 80))
    })
    
    # Five Point Summary - Outliers
    output$Five_Point <- DT::renderDataTable({
        b_df = select(te, cols_num_all)
        if (input$norm_dist) {
            b_df = yj_t
        }
        b_df <-
            data.frame(scale(
                b_df,
                center = input$standardise,
                scale = input$standardise
            ))
        rows_five <- c()
        for (k in 1:length(cols_num_all)) {
            dfk <- data.frame(y = b_df[[cols_num_all[k]]])
            fnum = (fivenum(dfk$y))
            low = fnum[2] - 1.5 * (fnum[4] - fnum[2])
            high = fnum[4] + 1.5 * (fnum[4] - fnum[2])
            for (i in 1:nrow(dfk)) {
                if (dfk$y[i] > high | dfk$y[i] < low) {
                    rows_five = c(rows_five, i)
                }
            }
        }
        out_rows_five = unique(rows_five)
        DT::datatable(data = te[out_rows_five, ], options = list(scrollX = T))
    })
    
    # Histogram for numeric variables
    output$Hist <- renderPlotly({
        hist <-
            plot_ly(
                data = df_t,
                x = ~ get(input$hist_ip),
                type = "histogram"
            )
        hist
        layout(hist,
               xaxis = list(title = input$hist_ip),
               margin = list(t = 80))
    })
    
    # Z score for Numeric Variables
    output$z_out <- DT::renderDataTable({
        DT::datatable(data = df_t[rows_z, ], options = list(scrollX = T))
    })
    
    # Scatter plot for numeric variables
    p_data = df_t
    h_dat = paste(
        "Obs No :",
        rownames(p_data),
        "<br> Price :",
        p_data$price,
        "<br> Age :",
        p_data$age,
        "<br> LotSize :",
        p_data$lotSize,
        "<br> LandValue :",
        p_data$landValue,
        "<br> LivingArea :",
        p_data$livingArea,
        "<br> PctCollege :",
        p_data$pctCollege,
        "<br> Bedrooms :",
        p_data$bedrooms,
        "<br> FirePlaces :",
        p_data$fireplaces,
        "<br> Bathrooms :",
        p_data$bathrooms,
        "<br> Rooms :",
        p_data$rooms,
        "<br> Heating :",
        p_data$heating,
        "<br> Fuel :",
        p_data$fuel,
        "<br> Sewer :",
        p_data$sewer,
        "<br> WaterFront :",
        p_data$waterfront,
        "<br> NConstruction :",
        p_data$newConstruction,
        "<br> CentralAir :",
        p_data$centralAir
    )
    output$Scatter <- renderPlotly({
        sc <-
            plot_ly(
                data = p_data,
                x = ~ get(input$sc_ip1),
                y = ~ get(input$sc_ip2),
                hover_data = 'text',
                text = h_dat
            )
        layout(
            sc,
            yaxis = list(title = input$sc_ip2),
            xaxis = list(title = input$sc_ip1),
            margin = list(t = 80)
        )
    })
    
    # Bag Plot
    p_data = df_t
    #h_dat = paste("Obs No :",rownames(p_data),"<br> Price :", p_data$price,"<br> Age :", p_data$age,"<br> LotSize :", p_data$lotSize,"<br> LandValue :", p_data$landValue,"<br> LivingArea :", p_data$livingArea,"<br> PctCollege :", p_data$pctCollege,"<br> Bedrooms :", p_data$bedrooms,"<br> FirePlaces :", p_data$fireplaces,"<br> Bathrooms :", p_data$bathrooms,"<br> Rooms :", p_data$rooms,"<br> Heating :", p_data$heating,"<br> Fuel :", p_data$fuel,"<br> Sewer :", p_data$sewer,"<br> WaterFront :", p_data$waterfront,"<br> NConstruction :", p_data$newConstruction,"<br> CentralAir :", p_data$centralAir)
    output$Bag <- renderPlot(bagplot(p_data[input$bag_ip1], p_data[input$bag_ip2]),)
    output$Bag_info <- renderText({
        paste0("Hover: x=",
               input$bag_hover$x,
               "\ty=",
               input$bag_hover$y)
    })
    
    # Outliers in Bag Plot
    output$Bag_Out <- DT::renderDataTable({
        DT::datatable(data = data.frame((
            compute.bagplot(p_data[input$bag_ip1], p_data[input$bag_ip2])["pxy.outlier"]
        )), options = list(scrollX = T))
    })
    
    # Code snippet for Mosaic plot
    output$Mosaic <- renderPlot({
        formula <-
            as.formula(paste("~", paste(input$vars_mosaic, collapse = " + ")))
        vcd::mosaic(formula,
                    data = df_t,
                    legend = TRUE,
                    shade = TRUE)
    })
    
    # Mahalanobis Distance plot
    output$M_dist <- renderPlotly({
        mh <-
            ggplot(mapping = aes(
                y = md2,
                x = (1:length(md2)) / length(md2),
                text = sprintf("Observation: %s", (1:length(md2)))
            )) +
            geom_point() +
            scale_y_continuous(limits = c(0, max(md2) * 1.1)) +
            labs(y = "Mahalanobis distance squared", x = "Complete Observations") +
            geom_abline(slope = 0,
                        intercept = threshold,
                        color = "red") +
            scale_x_continuous(
                breaks = c(0, 0.25, 0.5, 0.75, 1),
                labels = c("0%", "25%", "50%", "75%", "100%")
            ) +
            theme(legend.position = "bottom")
        ggplotly(mh)
    })
    output$Mh_Out <- DT::renderDataTable({
        DT::datatable(data = te_mh, options = list(scrollX = T))
    })
    
    # Cook's Distance 
    output$Cook <- renderPlotly({
        ckd <- ggplot(data = dfcd, mapping = aes(y = dc, x = id)) +
            geom_point() +
            scale_y_continuous(limits = c(0, max(dfcd$dc) * 1.1)) +
            labs(y = "Cook's distance",
                 x = "Complete Observations",
                 title = "Outlier pattern") +
            geom_abline(slope = 0,
                        intercept = thresh,
                        color = "red") +
            scale_x_continuous(breaks = c(0, 0.5, 1),
                               labels = c("0%", "50%", "100%")) +
            theme(legend.position = "bottom")
        ggplotly(ckd)
        
    })
    output$Ck_Out <- DT::renderDataTable({
        DT::datatable(data = te_ck, options = list(scrollX = T))
    })
    
    # DB Scan
    output$Knn <- renderPlot({
        dbscan::kNNdistplot(pr_db, k = 1)
        # Knee is at 25000
        abline(h = 25000, lty = 2)
    })
    output$Db_out <- DT::renderDataTable({
        DT::datatable(data = pr_db_te, options = list(scrollX = T))
    })
    
    # LOF
    output$LOF <- DT::renderDataTable({
        DT::datatable(data = pr_lof, options = list(scrollX = T))
    })
    
    # Robust Methods
    output$Rb_M <- DT::renderDataTable({
        DT::datatable(data = rb_methods, options = list(scrollX = T))
    })
    
    # 1-class SVM
    output$SVM_Out <- DT::renderDataTable({
        DT::datatable(data = svm_out, options = list(scrollX = T))
    })
    
    svm_te = te
    svm_te["col"] <- "Normal"
    output$SVM_Plot <- renderPlotly({
        svm_ip1 <- input$svm_ip1
        svm_ip2 <- input$svm_ip2
        
        svm_out_col = svm_out
        svm_out_col["col"] <- "Outlier"
        svm_plot_dat <- rbind(svm_te, svm_out_col)
        sc <-
            plot_ly(
                data = svm_plot_dat,
                x = ~ get(svm_ip1),
                y = ~ get(svm_ip2),
                color = ~ factor(col),
                colors = c("#1f77b4", "#d62728")
            )
        layout(
            sc,
            yaxis = list(title = input$svm_ip2),
            xaxis = list(title = input$svm_ip1),
            margin = list(t = 80)
        )
    })
    
    # Outlier Intersection
    output$Out_Final <- DT::renderDataTable({
        DT::datatable(data = te_out_final, options = list(scrollX = T))
    })
    # Outlier Intersection - Raw
    output$Out_Raw <- DT::renderDataTable({
        DT::datatable(data = df_t_out, options = list(scrollX = T))
    })
})
