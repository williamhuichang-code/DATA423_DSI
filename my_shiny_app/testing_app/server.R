shinyServer(function(input, output, session) {

  output$titanic <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(Titanic))
  })
  
  output$Mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
    vcd::mosaic(formula, data = Titanic,
           main = "Survival on the Titanic", shade = TRUE, legend = TRUE)
  })

  output$Plot <- renderPlot({
    plot(Titanic)
  })
  
  output$SummaryA1 <- renderPrint({
    summary(Titanic)
  })
  
  output$SummaryA2 <- renderPrint({
    summary(as.data.frame(Titanic))
  })
  
  output$SummaryB1 <- renderPrint({
    str(airquality)
  })
  
  output$SummaryB2 <- renderPrint({
    summary(airquality)
  })
  
  output$airquality <- DT::renderDataTable({
    DT::datatable(data = airquality)
  })

  output$Boxplot <- renderPlot({
    data <- as.matrix(airquality)
    data <- scale(data, center = input$standardise, scale = input$standardise)
    car::Boxplot(y = data, ylab = NA, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
            horizontal = FALSE, outline = input$outliers, 
            col = brewer.pal(n = dim(airquality)[2], name = "RdBu"),
            range = input$range, main = "Boxplots of Airquality data", 
            id = ifelse(input$outliers, list(n = Inf, location = "avoid"), FALSE)) 
            
            
            
  })
  
  output$Corrgram <- renderPlot({
    corrgram(airquality, 
             order = input$Group, 
             abs = input$abs, 
             cor.method = input$CorrMeth,
             text.panel = panel.txt,
             main = "Correlation of Airquality data")
  })

  output$Missing <- renderPlot({
    vis_miss(airquality, cluster = input$cluster) +
      labs(title = "Missingness of Airquality data")
  })
  
  output$Pairs <- renderPlot({
    GGally::ggpairs(data = airquality, title = "Pairs of Airquality data")
  })
  
  output$SummaryC1 <- renderPrint({
    str(EuStockMarkets)
  })
  
  output$SummaryC2 <- renderPrint({
    summary(EuStockMarkets)
  })
  
  output$TimeSeries <- renderPlot({
    autoplot(EuStockMarkets)
  })
  
  output$AutoCorr <- renderPlot({
    acf(EuStockMarkets, plot = TRUE, lag.max = 100)
  })

  getPrideAndPrejudice <- reactive({
    austen_books() %>%
      filter(book == "Pride & Prejudice") %>%
      mutate(linenumber = row_number(),
             chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE))),
             book = NULL) %>%
      ungroup() %>%
      unnest_tokens(word, text)
  })
  
  output$SummaryD <- renderPrint({
    pp <- getPrideAndPrejudice()
    cat(paste(sep = "\n", "Pride and Prejudice", 
          paste("Words: ", dim(pp)[1]),
          paste("Chapters: ", max(pp$chapter))))
  })

  getWords <- reactive({
    wordscores <- getPrideAndPrejudice() %>%
      inner_join(lexicon, by = "word")    
  })
  
  output$Sentiment <- renderPlot({
    wordscores <- getWords() %>%
      group_by(page = linenumber %/% 100) %>%
      count(value) %>%
      summarise(value = sum(value*n))
    ggplot(data = wordscores, mapping = aes(x = page, y = value, fill = 1)) +
      geom_bar(alpha = 0.7, stat = "identity", show.legend = F) +
      labs(title = "Sentiment by page", x = "Page", y = "Positivity ")
  })
  
  output$Cloud <- renderWordcloud2({
    words <- getWords() %>%
      dplyr::count(sort = TRUE, name = "freq", word, value) %>%
      dplyr::select(word, freq, value) %>%
      dplyr::mutate(colour = ifelse(value > 0, "green", "red"))
    wordcloud2(data = words, color = words$colour)
  })

  output$SummaryE <- renderPrint({
    glimpse(gasoline)
    cat(paste("Wavelengths in NIR:",dim(gasoline$NIR)[2]))
  })

  output$SummaryF <- renderPrint({
    summary(iris)
  })
  
  output$Spectra <- renderPlot({
    matplot(t(gasoline$NIR), type = "l", ylab = "-log(R)", xaxt = "n", main = "NIR spectra")
    ind <- pretty(seq(from = 900, to = 1700, by = 2))
    ind <- ind[ind >= 900 & ind <= 1700]
    ind <- (ind - 898) / 2
    axis(1, ind, colnames(gasoline$NIR)[ind])
  })

  output$StanDevSpectra <- renderPlot({
    std <- apply(t(gasoline$NIR), MARGIN = 1, sd)
    matplot(std, type = "l", ylab = "-log(R)", xaxt = "n", main = "NIR Std Deviation spectra")
    ind <- pretty(seq(from = 900, to = 1700, by = 2))
    ind <- ind[ind >= 900 & ind <= 1700]
    ind <- (ind - 898) / 2
    axis(1, ind, colnames(gasoline$NIR)[ind])
  })
  
  output$iris <- DT::renderDataTable({
    DT::datatable(data = iris)
  })

  output$tableplot <- renderPlot({
    #Add a sort column if there is not an appropriate one already available
    iris2 <- data.frame(row = 1:nrow(iris), iris)  
    tabplot::tableplot(iris2, sortCol = "row", decreasing = FALSE)
  })
  
  output$MixedPairs <- renderPlot({
    GGally::ggpairs(data = iris,  mapping = ggplot2::aes(colour = Species), title = "Pairs of Iris data")
  })

  output$Corrgram2 <- renderPlot({
    corrgram(iris, 
             order = input$Group2, 
             abs = input$abs2, 
             cor.method = input$CorrMeth2,
             text.panel = panel.txt,
             main = "Correlation of iris data")
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40, mean=0, sd=0.0012) + 172.5839, rnorm(40,mean=0, sd=0.0017) -43.5235)
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = input$provider, options = providerTileOptions(noWrap = FALSE)) %>%
      addMarkers(data = points(), group="students") %>% 
      addTerminator(resolution=10, time = Sys.time(), group = "night-day") %>%
      addLayersControl(overlayGroups = c("night-day","students")) %>%
      addEasyButton(easyButton(icon="fa-globe", title="Zoom to high level", onClick=JS("function(btn, map){ map.setZoom(1.5); }")))
  })
  
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click
    text <- paste("Student", round(runif(1,10000,10000000)),"at", "9:43am")
    leafletProxy("mymap") %>% 
      clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  output$Plot <- renderVisNetwork({
    visNetwork(nodes = nodes, edges = edges) %>%
      visIgraphLayout(layout = "layout_in_circle", physics = input$physics) %>% 
      visEdges(arrows = "to", shadow = T, smooth=input$smooth) %>% 
      visEdges(dashes = TRUE)
  })
  
})

#runApp(appDir = ".", display.mode = "showcase")
