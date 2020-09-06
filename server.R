# Server

shinyServer(function(input, output, session) {
    
     cldat <- reactive({
        d <- dat
        vratio <- apply(d, MARGIN= 2,FUN=pmiss)
        d <- d[,vratio < input$Varthre]

        rratio <- apply(d, MARGIN= 1,FUN=pmiss)
        d <- d[rratio < input$Obsthre, ]
        d
    })
    
    output$tableX <- DT::renderDataTable({
        DT::datatable(cldat(),
                      rownames = input$rownames,
                      #selection = input$selection,
                      filter = list(position = input$filter),
                      options = list(searching = TRUE,
                                     pageLength = 10,
                                     lengthMenu = c(10, 30, 100),
                                     dom = paste(input$dom, collapse = ""),
                                     ordering = input$order
                      )
                      
        )  %>%
            formatStyle(columns = c("DEATHRATE"), backgroundColor = "skyblue")
    })
    
    output$VarRemove <- renderPrint({
        d <- dat
        vratio <- apply(d, MARGIN= 2,FUN=pmiss)
        cat(colnames(dat)[vratio >= input$Varthre])
    })
    
    output$ObsRemove <- renderPrint({
        d <- dat[,vratio < input$Varthre]
        rratio <- apply(d, MARGIN= 1,FUN=pmiss)
        cat(rownames(dat)[rratio >= input$Obsthre])
    })
    
    output$Finaldim <- renderPrint({
        cat(dim(cldat()))
    })
    
    output$SummaryA1 <- renderPrint({
        summary(cldat())
    })
    
    output$SummaryA2 <- renderPrint({
        print(dfSummary(cldat()), method = 'render')
    })
    
    output$Missing1 <- renderPlot({
        vis_dat(cldat()) +
            labs(main = "Missingness of Ass2Data")
    })
    
    output$Missing2 <- renderPlot({
        vis_miss(cldat(), cluster = input$cluster) +
            labs(main = "Missingness of Ass2Data(cluster)")
    })
    
    output$Missing3 <- renderPlot({
        variable = input$VariablesA
        naniar::gg_miss_upset(data = cldat()[,variable])
    })
    
    output$Missing4 <- renderPlot({
        m <- is.na(cldat()) + 0 
        cm <- colMeans(m)
        m <- m[, cm > 0 & cm < 1, drop = FALSE]
        corrgram::corrgram(x = cor(m),
                           order = input$Group,
                           abs = input$abs,
                           cor.method = input$CorrMeth,
                           #text.panel = panel.txt,
                           main = "Correlation of Missing-values")
    })
    
    
    output$Missing5 <- renderPlot({
        d<- cldat()
        d$MISSINGNESS <- apply(X = is.na(d), MARGIN = 1, FUN = sum)
        tree <- train(MISSINGNESS ~ . -COUNTRY, data = d, method = "rpart", na.action = na.rpart)
        rpart.plot(tree$finalModel,
                   main = "TUNED: Predicting the number of missing variables in an observation", 
                   roundint = TRUE, 
                   clip.facs = TRUE)
    })
    
    output$Boxplot <- renderPlot({
        data <- cldat()
        variable = input$VariablesB
        data <- as.matrix(data[,variable])
        data <- scale(data, center = input$standardise, scale = input$standardise)
        car::Boxplot(formula = ~., data = data, use.cols = TRUE, notch = FALSE, varwidth = FALSE,
                     horizontal = FALSE, outline = input$outliers, ylab = NA, las = 2,
                     col = brewer.pal(n = dim(dat), name = "RdBu"),
                     range = input$range, main = "Boxplots of Ass1Data")
    })
    
    output$Corrgram <- renderPlot({
        d <- cldat()
        variable = input$VariablesC
        corrgram::corrgram(x = d[,variable],
                           order = input$Group1,
                           abs = input$abs1,
                           cor.method = input$CorrMeth1,
                           text.panel = panel.txt,
                           main = "Correlation of Ass2Data")
    })
    
    output$Accuracy <- renderPlot({
        set.seed(1)
        d <- cldat()
        subIndex <- caret::createDataPartition(y = d$DEATHRATE, p = 0.7, list = FALSE)
        train <- d[subIndex,]
        test <- d[-subIndex,]
        
        set.seed(1)
        rec <- recipes::recipe(DEATHRATE ~., data = train) %>%
            update_role("COUNTRY", new_role = "id") %>% #id is not a predictor
            step_knnimpute(all_predictors(), neighbours = 5) %>%
            step_center(all_numeric(), -has_role("outcome")) %>%
            step_scale(all_numeric(), -has_role("outcome")) %>%
            step_dummy(all_nominal())
        
        set.seed(1)
        model <- caret::train(rec, data = train, method = "glmnet")
        yhat <- predict(model, newdata = test)
        test$Residuals <- test$DEATHRATE - yhat
        rang <- range(c(test$DEATHRATE, yhat))
        ggplot(data=test, mapping = aes(x = yhat, y = DEATHRATE, label = Residuals)) +
            geom_point() +
            geom_abline(slope = 1, col = "blue") +
            labs(title = "Missingness prediction model test", y = "actual", x = "predicted") +
            coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
        
        
    })
    
    
    output$Residuals <- renderPlot({
        set.seed(1)
        d <- cldat()
        subIndex <- caret::createDataPartition(y = d$DEATHRATE, p = 0.7, list = FALSE)
        train <- d[subIndex,]
        test <- d[-subIndex,]
        coef <- input$Coef
        limits <- boxplot.stats(x = test$Residuals, coef = coef)$stats
        data$label <- ifelse(test$Residuals < limits[1] | test$Residuals > limits[5], rownames(test), NA)
        
        ggplot(data = test, mapping = aes(x = Residuals, y = 1, label = label)) +
            ggrepel::geom_text_repel() +
            geom_boxplot(coef = coef) +
            labs(title = paste("Y Boxplot using", coef, "as IQR Multplier")) +
            theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
        
        
    })
    
})
