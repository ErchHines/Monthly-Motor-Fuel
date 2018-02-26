library(leaflet)
library(RColorBrewer)
library(scales)
library(geojsonio)
library(sp)
library(ggplot2)
library(reshape2)
library(DT)


function(input, output, session) {

## Interactive Map ###########################################

# loads a map with state boundaries
states <- geojsonio::geojson_read("data/us-states.geojson", what = "sp")

# creates the colors for the map and places them in bins
bins <- c(-20,-10, -5, -1, 0, 1, 5, 10, 20)
pal <- colorBin("RdYlBu", domain = PercentChange[,ncol(PercentChange)], bins = bins)

# creates the hover over text to display percent change since the previous year, change for month
labels <- sprintf(
  "<strong>%s</strong><br/>%g&#x00025 <br> Sep 16 through Sep 17",
  states$name, PercentChange[,ncol(PercentChange)]
) %>% lapply(htmltools::HTML)

#stockmap
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
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
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

#Takes input from user (see ui.R) and returns a line graph of fuel consumption by month for the year

stateReact <- reactive({
  subset(MGrossVolGas, StateName == input$State) 
})

stateReactSF <- reactive({
  subset(meltMF33SF17, State == input$State) 
})


boldTxt11 <- element_text(face = "bold", size = 11)


output$FuelbyMonth <- renderPlot({
  stateReact1 <- stateReact()
  
  ggplot(stateReact1, aes(x=variable, y=value, group=StateName)) + geom_line() + geom_point() +
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
              
              

  "))
})



  ## Data Explorer ###########################################

# Output for Monthly Motor Fuel Tables
output$mytable1 = DT::renderDataTable({
  DT::datatable(GrossVolGas, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% formatCurrency(1:12, '', digits = 0)
})

output$mytable2 = DT::renderDataTable({
  DT::datatable(MF33CO16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% formatCurrency(1:12, '', digits = 0)
})

output$mytable3 = DT::renderDataTable({
  DT::datatable(MF33SF17, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% formatCurrency(1:12, '', digits = 0)
})

output$mytable4 = DT::renderDataTable({
  DT::datatable(MF33SF16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% formatCurrency(1:12, '', digits = 0)
})

output$mytable5 = DT::renderDataTable({
  DT::datatable(MF121TP1, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% formatCurrency(1:12, '')
})

output$mytable6 = DT::renderDataTable({
  DT::datatable(MF33GA16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))%>% formatCurrency(1:12, '', digits = 0)
})
}
