library(leaflet)
library(DT)

# Choices for drop-downs
vars <- MGrossVolGas$StateName
varsTs <- raw551$STATE_NAME
dates <- raw551$Date

navbarPage("FHWA Monthly Motor Fuel Report", id="nav", 
  
  tabPanel("Fuels Map",
    div(class="outer",

  tags$head(
    # Include the custom CSS
    includeCSS("styles.css")
  ),

  leafletOutput("map", width="100%", height="100%"),

  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 100, left = "auto", right = 0, bottom = "auto",
    width = 450, height = "auto",

    h3(reportYear,"Monthly Gasoline Consumption"),

    selectInput(
      "State", "Click on a state or select below:", vars, 
      multiple = FALSE, selected = 1
    ),

    plotOutput("FuelbyMonth"),
    
    uiOutput("FuelFigures"),
  
    actionButton(inputId='ab1', label="Dowload .xls", 
      icon = icon("file"), style="color: #fff; background-color: #268715; border-color: #2e6da4",
      onclick ="window.open('https://www.fhwa.dot.gov/policyinformation/motorfuel/sep17/sep17.xls', '_blank')"),
    
    actionButton(inputId='ab1', label="Dowload .pdf", 
      icon = icon("file"), style="color: #fff; background-color: red; border-color: #2e6da4",
      onclick ="window.open('https://www.fhwa.dot.gov/policyinformation/motorfuel/sep17/sep17.pdf', '_blank')") 
  ),
    
    
  
  tags$div(id="cite",
    'Data based on monthly gasoline consumption reported by states'
  )
    )
  ),
  
  # creates the tabs and displays the plots
  tabPanel("Plots",
           
    sidebarLayout(
      
      sidebarPanel(
        h4("Motor Fuel Gallonage January 2006 - September 2017"),

      
        selectInput(
          "StateTs", "Select a State to View:", varsTs, 
          multiple = FALSE, selected = 1
        ),
        
        radioButtons(
          "col","Select Plot",
          choices = c("Volume", "Volume per Day", 
                      "Time Series (Gasoline Gallons)",
                      "Time Series (Diesel Gallons)"),
          selected = "Volume"
        ),
        
        sliderInput(
          "DateTs", "Select Date Range:", 
          min = as.Date("2006-01-01"), max = as.Date("2017-09-01"), 
          value = c(as.Date("2006-01-01"), as.Date("2017-09-01"))
        ),
        conditionalPanel(
          condition = "input.col == 'Volume'",
          h5("This graph displays historical data for 
             monthly motor fuel gallonage by state."),
          h5("Click and drag the mouse
             over an area of the graph to display the values in a table below 
             (limited to 10 selections)"),
          dataTableOutput("infoV")
        ),
        conditionalPanel(
          condition = "input.col == 'Volume per Day'",
          h5("This graph displays historical data for 
             monthly motor fuel gallonage per day by state."),
          h5("Click and drag the mouse
             over an area of the graph to display the values in a table below 
             (limited to 10 selections)"),
          dataTableOutput("infoD")
        ),
        conditionalPanel(
          condition = "input.col == 'Time Series (Gasoline Gallons)'",
          h5("This graph  extracts the seasonality from our monthly gasoline and
             gasahol data to display an overal trend as well as an unexplained
             remainder."),
          h5("In cases where there is a large remainder,
             data should be checked with the state. Please select at least
             two years worth of data")
        ),
        conditionalPanel(
          condition = "input.col == 'Time Series (Diesel Gallons)'",
          h5("This graph  extracts the seasonality from our monthly gasoline and
             gasahol data to display an overal trend as well as an unexplained
             remainder."),
          h5("In cases where there is a large remainder,
             data should be checked with the state. Please select at least
             two years worth of data")
        )
      ),
    
      mainPanel(
        conditionalPanel(condition = "input.col == 'Volume'",
          fluidRow(
            column(
              12, plotOutput(
                    outputId = "plot1", height = 620, 
                      brush = brushOpts(
                        id = "plot_brush"
                      )
                  )
            )
          )
        ),
        conditionalPanel(condition = "input.col == 'Volume per Day'",
         fluidRow(
           column(
             12, plotOutput(
                   outputId = "plot2", height = 620,
                     brush = brushOpts(
                       id = "plot_brush"
                     )
                 )
           )
         )
        ),
        conditionalPanel(condition = "input.col == 'Time Series (Gasoline Gallons)'",
         fluidRow(
           column(
             12, plotOutput(outputId = "plotTs1", height = 620)
           )
         )
        ),
        conditionalPanel(condition = "input.col == 'Time Series (Diesel Gallons)'",
         fluidRow(
           column(
             12, plotOutput(outputId = "plotTs2", height = 620)
           )
         )
        )
      )
    )
  ),


  navbarMenu("Tables",
  
    tabPanel("MF33G",
      h3("Gross Volume of Gasoline/Gasahol Reported by States",reportYear),
      dataTableOutput("mytable1")
    ),
  
    tabPanel("MF33GA16",
      h3("Gross Volume of Gasoline/Gasahol Reported by States",reportYear - 1),
      dataTableOutput("mytable6")
    ),
    
    
    tabPanel("MF33CO16",
      h3("Monthly Special Fuels & Gasoline/Gasohol Reported by States",reportYear - 1),
      dataTableOutput("mytable2")
    ),
    
    tabPanel("MF33SF17",
      h3("Monthly Special Fuels Reported by States",reportYear),
      dataTableOutput("mytable3")
    ),
    
    tabPanel("MF33SF16",
      h3("Monthly Special Fules Reported by States",reportYear - 1),
      dataTableOutput("mytable4")
    ),
    
    tabPanel("Tax Rates",
      h3("Tax Rates on Motor Fuel"),
      dataTableOutput("mytable5")
    )
  
  ),
  
  #This suppresses an error due to a dataframe not being created when the time
  #series buttons aren't selected
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
  
)
