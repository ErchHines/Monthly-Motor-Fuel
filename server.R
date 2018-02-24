library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(rgdal)
library(geojson)
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
bins <- c(-16,-8, -4, -1, 0, 1, 8, 16)
pal <- colorBin("RdYlBu", domain = PercentChange$September, bins = bins)

# creates the hover over text to display percent change since the previous year
labels <- sprintf(
  "<strong>%s</strong><br/>%g&#x00025 <br> Sep 16 through Sep 17",
  states$name, PercentChange$September
) %>% lapply(htmltools::HTML)

#stockmap
output$map <- renderLeaflet({
  leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    addPolygons(
      fillColor = ~pal(PercentChange$September),
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
    addLegend(pal = pal, values = ~PercentChange$September, opacity = 0.7, 
              title = "Percent Change Gasoline Consumption" , 
              labFormat = labelFormat(between = "% - ", suffix = "%"),
              position = "bottomright")
})

#Takes input from user (see ui.R) and returns a line graph of fuel consumption by month for the year

stateReact <- reactive({
  MGrossVolGas %>% filter(StateName == input$State) 
})

boldTxt11 <- element_text(face = "bold", size = 11)

output$scatterCollegeIncome <- renderPlot({
  # If no zipcodes are in view, don't plot
  stateReact1 <- stateReact()
  
  ggplot(stateReact1, aes(x=variable, y=value, group=StateName)) + geom_line() + geom_point() +
    scale_y_continuous(name="Gallons", labels = comma) +
    scale_x_discrete(name= "Month") +
    theme(axis.text = boldTxt11)

})


  ## Data Explorer ###########################################

# Output for Monthly Motor Fuel Tables
output$mytable1 = DT::renderDataTable({
  DT::datatable(GrossVolGas, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))
})

output$mytable2 = DT::renderDataTable({
  DT::datatable(MF33CO16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))
})

output$mytable3 = DT::renderDataTable({
  DT::datatable(MF33SF17, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))
})

output$mytable4 = DT::renderDataTable({
  DT::datatable(MF33SF16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))
})

output$mytable5 = DT::renderDataTable({
  DT::datatable(MF121TP1, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))
})

output$mytable6 = DT::renderDataTable({
  DT::datatable(MF33GA16, options = list(lengthMenu = c(10, 25, 51), pageLength = 51))
})
}
