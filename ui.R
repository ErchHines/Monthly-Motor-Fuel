library(leaflet)

# Choices for drop-downs
vars <- MGrossVolGas$StateName

navbarPage("FHWA Monthly Motor Fuel Report", id="nav", 

  tabPanel("Fuels Map",
    div(class="outer",

  tags$head(
    # Include the custom CSS
    includeCSS("styles.css")
  ),

  # If not using custom CSS, set height of leafletOutput to a number instead of percent
  leafletOutput("map", width="100%", height="100%"),

  # Shiny versions prior to 0.11 should use class = "modal" instead.
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 100, left = "auto", right = 0, bottom = "auto",
    width = 450, height = "auto",

    h3(reportYear,"Monthly Gasoline Consumption"),

    selectInput("State", "Click on a state or select below:", vars, multiple = FALSE, selected = 1),

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
  
  # creates the tabs and displays the tables

  
  tabPanel("Time Series",
    h3("California Gasoliine/Gasahol Time Series 2012 - 2016"),
    plotOutput(outputId = "decompose_ca", height = 500, width = 700)
  ),
  
  tabPanel("MF33G",
    h3("Gross Volume of Gasoline/Gasahol Reported by States",reportYear),
    DT::dataTableOutput("mytable1")
       ),

  tabPanel("MF33GA16",
    h3("Gross Volume of Gasoline/Gasahol Reported by States",reportYear - 1),
    DT::dataTableOutput("mytable6")
  ),
  
  
  tabPanel("MF33CO16",
    h3("Monthly Special Fuels & Gasoline/Gasohol Reported by States",reportYear - 1),
    DT::dataTableOutput("mytable2")
  ),
  
  tabPanel("MF33SF17",
    h3("Monthly Special Fuels Reported by States",reportYear),
    DT::dataTableOutput("mytable3")
  ),
  
  tabPanel("MF33SF16",
    h3("Monthly Special Fules Reported by States",reportYear - 1),
    DT::dataTableOutput("mytable4")
  ),
  
  tabPanel("Tax Rates",
    h3("Tax Rates on Motor Fuel"),
    DT::dataTableOutput("mytable5")
  )

)
   