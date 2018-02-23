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
library(plotly)

function(input, output, session) {

  ## Interactive Map ###########################################

states <- geojsonio::geojson_read("data/us-states.geojson", what = "sp")

bins <- c(-16,-8, -4, -1, 0, 1, 8, 16)
pal <- colorBin("RdYlBu", domain = PercentChange$September, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g&#x00025 <br> Sep 16 through Sep 17",
  states$name, PercentChange$September
) %>% lapply(htmltools::HTML)

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

# data.r = reactive({
#   a = subset(mydata.m, variable %in% input$daterange)
#   return(a)
# })

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

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
