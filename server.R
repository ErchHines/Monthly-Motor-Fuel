function(input, output, session){

  ## Interactive Map ###########################################
  
  # creates the colors for the map and places them in bins
  bins <- c(-20,-10, -5, -1, 0, 1, 5, 10, 20)
  pal <- colorBin(
    "RdYlBu", domain = PercentChange[,ncol(PercentChange)], bins = bins
  )
  
  # creates the hover over text to display percent change 
  # since the previous year, change for month
  labels <- sprintf(
    "<strong>%s</strong><br/>%g&#x00025 <br> Sep 17 versus Sep 16",
    states$name, PercentChange[,ncol(PercentChange)]
  ) %>% lapply(htmltools::HTML)
  
  #stockmap, this just creates our view for the mapnik map
  output$map <- renderLeaflet({
    leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addPolygons(
        group = states$name,
        fillColor = ~pal(PercentChange[,ncol(PercentChange)]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          fillOpacity = 1,
          bringToFront = FALSE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~PercentChange[,ncol(PercentChange)], opacity = 0.7, 
                title = "Percent Change Gasoline Consumption" , 
                labFormat = labelFormat(between = "% - ", suffix = "%"),
                position = "bottomleft")
  })
  
  #Takes input from user (see ui.R) and returns a 
  #line graph of fuel consumption by month for the year
  
  stateReact <- reactive({
    subset(MGrossVolGas, StateName == input$State) 
  })
  
  stateReactSF <- reactive({
    subset(meltMF33SF17, State == input$State) 
  })
  
  
  boldTxt11 <- element_text(face = "bold", size = 11)
  
  
  output$FuelbyMonth <- renderPlot({
    stateReact1 <- stateReact()
    
    ggplot(stateReact1, aes(x=variable, y=value, group=StateName)) + 
      geom_line() + geom_point() +
      scale_y_continuous(name="Gallons", labels = comma) +
      scale_x_discrete(name= "Month") +
      theme(axis.text = boldTxt11)
    
  })
  
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    p <- input$map_shape_click
    if(!is.null(p$group)){
      if(is.null(input$State) || input$State!=p$group) updateSelectInput(session, "State", selected=p$group)
    }
  })
  
  # Creates a table to show the current month's fuel numbers on the map page
  output$FuelFigures <- renderUI({
    stateReact2 <- stateReact()
    stateReactSF2 <- stateReactSF()
    stateReact2$value <- prettyNum(stateReact2$value, big.mark=",")
    stateReactSF2$value <- prettyNum(stateReactSF2$value, big.mark=",")
    HTML(paste0("<div style=","background-color:#2060A8;color:white;padding:20px;",">
                <h3>", input$State,"</h3>
                <table cellspacing=20>
                <tr>
                <th>Month</th>
                <th>Fuel</th> 
                <th>Gallons</th>
                </tr>
                <tr>
                <td>",reportMonth,"</td>
                <td>Gasoline and Gasahol</td>
                <td>",stateReact2[nrow(stateReact2),3],"</td>
                </tr>
                <tr>
                <td>",reportMonth,"</td>
                <td>Diesal and Special Fuel</td>
                <td>",stateReactSF2[nrow(stateReactSF2),3],"</td>
                </tr>
                </table>
                </br>*data in thousands of gallons
                <br>
                
                ")
         )
  })
  
  ## Time Series ###############################################
  output$plotTs1 <- renderPlot({
    
    #Create a time series table for the selected state
    timeSeries <- subset(raw551, STATE_NAME == input$StateTs)
    
    timeSeries <- subset(
      timeSeries, Date >= input$DateTs[1] & Date <= input$DateTs[2]
    )
    
    timeSeries <- timeSeries[order(timeSeries$Date),]
    
    ggsdc(timeSeries, aes(x = Date, y = gas),
          method = "stl", s.window = "periodic", frequency = 12,
          facet.titles = c("The original series", "The underlying trend", 
                           "Regular seasonal patterns", "All the randomness left")) +
      geom_line() +
      labs(y = "Thousands of Gallons", x = "Date") +
      ggtitle("Time Series of Gasoline and Gasohol Gallons by Month") +
      theme_hc(base_size = 16) 
    
    
  })
  
  output$plotTs2 <- renderPlot({
    
    #Create a time series table for the selected state
    timeSeries <- subset(raw551, STATE_NAME == input$StateTs)
    
    timeSeries <- subset(
      timeSeries, Date >= input$DateTs[1] & Date <= input$DateTs[2]
    )
    
    timeSeries <- timeSeries[order(timeSeries$Date),]
    
    ggsdc(timeSeries, aes(x = Date, y = dieselLPG),
          method = "stl", s.window = "periodic", frequency = 12,
          facet.titles = c("The original series", "The underlying trend", 
                           "Regular seasonal patterns", "All the randomness left")) +
      geom_line() +
      labs(y = "Thousands of Gallons", x = "Date") +
      ggtitle("Time Series of Diesel and Special Fuel Gallons by Month") +
      theme_hc(base_size = 16) 
    
  })
  

  ## Chart #####################################################
  

  #This is the first plot looking at overall gallonage, note: should consider
  #taking some of these operations and putting them in the global file, would 
  #require splitting data frame raw551 for regular charts and the time series
  output$plot1 <- renderPlot({
    plot2 <- subset(raw551, STATE_NAME == input$StateTs)
    
    plot2 <- subset(
      plot2, Date >= input$DateTs[1] & Date <= input$DateTs[2]
    )
    
    plot2 <- plot2[order(plot2$Date),]
    
    plot2 <- melt(plot2, id.vars = c(1:6,11))
    
    plot2 <- subset(plot2, variable == "gas" | variable == "dieselLPG")
    
    ggplot(plot2, aes(Date , value, col=variable)) + 
      geom_line(size = 1.2) + 
      scale_color_discrete(
        name  ="Fuel Type",
        breaks=c("gas", "dieselLPG"),
        labels=c("Gasoline and Gasohol", "Diesel and Special Fuels")) +
      scale_y_continuous(labels = comma) +
      labs(y = "Thousands of Gallons") +
      ggtitle("Motor Fuel Gallons by Month") +
      theme_hc(base_size = 17) 
  })
  
  #This plot is gallonage per day to account for the different number of days
  #in each month and it also better matches with EIA data
  output$plot2 <- renderPlot({
    plot3 <- subset(raw551Days, STATE_NAME == input$StateTs)
    
    plot3 <- subset(
      plot3, Date >= input$DateTs[1] & Date <= input$DateTs[2]
    )
  
    plot3 <- plot3[order(plot3$Date),]
    
    plot3 <- melt(plot3, id.vars = c(1:6,11))
    
    plot3 <- subset(plot3, variable == "gas" | variable == "dieselLPG")
    
    ggplot(plot3, aes(Date , value, col=variable)) + 
      geom_line(size = 1.2) + 
      scale_color_discrete(
        name  ="Fuel Type",
        breaks=c("gas", "dieselLPG"),
        labels=c("Gasoline and Gasohol", "Diesel and Special Fuels")) +
      scale_y_continuous(labels = comma) +
      labs(y = "Thousands of Gallons") +
      ggtitle("Motor Fuel Gallons per Day by Month") +
      theme_hc(base_size = 17) 
  })
  
  #This is a reactive function to be used with volume plots in order to display
  #the values for each clicked point
  selectedV <- reactive({
    
    #Determine whther we need to use raw551 or raw551Days based on button
    #selection
    switch (input$col,
      "Volume" = plot2 <- subset(raw551, STATE_NAME == input$StateTs),
      "Volume per Day"= plot2 <- subset(raw551Days, STATE_NAME == input$StateTs)
    )
    
    plot2 <- subset(
      plot2, Date >= input$DateTs[1] & Date <= input$DateTs[2]
    )
    
    plot2 <- plot2[order(plot2$Date),]
    
    plot2 <- melt(plot2, id.vars = c(1:6,11))
    
    plot2 <- subset(plot2, variable == "gas" | variable == "dieselLPG")
    
    selPoints <- brushedPoints(
      plot2, input$plot_brush, xvar = "Date", yvar = "value"
    )
    
    selPoints$Date <- as.Date(selPoints$Date, origin = "1970-01-01")
    selPoints$Date <- format(selPoints$Date, format = "%b %Y")
    selPoints$value <- prettyNum(selPoints$value, big.mark=",")
    selPoints$variable <- revalue(
      selPoints$variable, c("gas"="Gasoline and Gasohol", 
                            "dieselLPG" = "Diesel and Special Fuel")
    )
    
    selPoints <- selPoints[,c("Date","variable","value")]
    colnames(selPoints) <- c("Date","Fuel","Gallons")
  
    
    return(selPoints)
    
  })
  
  #This is to create the output to return the selected points 
  #when we click on a point in the volume by day plot
  
  output$infoV <- renderDataTable({
    selPointsV <- selectedV()
    req(is.na(selPointsV) == FALSE)
    datatable(selPointsV, options = list(dom = 't', autoWidth = TRUE), rownames = FALSE)
  })
  
  #This is to create the output to return the selected points 
  #when we click on a point in the volume plot
  
  output$infoD <- renderDataTable({
    selPointsD <- selectedV()
    req(is.na(selPointsD) == FALSE)
    datatable(selPointsD, options = list(dom = 't', autoWidth = TRUE), rownames = FALSE)
  })

  ## Data Explorer ###########################################
  
  # Output for Monthly Motor Fuel Tables, these are the stock tables that 
  # were included with previous monthly reports
  output$mytable1 = renderDataTable({
    datatable(
      GrossVolGas, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>%
      formatCurrency(1:12, '', digits = 0
    )
  })
  
  output$mytable2 = renderDataTable({
    datatable(
      MF33CO16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% 
      formatCurrency(1:12, '', digits = 0
    )
  })
  
  output$mytable3 = renderDataTable({
    datatable(
      MF33SF17, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% 
      formatCurrency(1:12, '', digits = 0
    )
  })
  
  output$mytable4 = renderDataTable({
    datatable(
      MF33SF16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% 
      formatCurrency(1:12, '', digits = 0
    )
  })
  
  output$mytable5 = renderDataTable({
    datatable(
      MF121TP1, options = list(lengthMenu = c(10, 25, 51), pageLength = 51)
    )
  })
  
  output$mytable6 = renderDataTable({
    datatable(
      MF33GA16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% 
      formatCurrency(1:12, ''
    )
  })
}
