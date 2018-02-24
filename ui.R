library(leaflet)

# Choices for drop-downs
vars <- MGrossVolGas$StateName


navbarPage("FHWA Monthly Motor Fuel Report", id="nav",

  tabPanel("Fuels Map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
        width = 500, height = "auto",

        h3("2017 Monthly Gasoline Consumption"),

        selectInput("State", "State", vars, selected = ),

        
        plotOutput("FuelbyMonth")
      ),

      tags$div(id="cite",
        'Data based on monthly gasoline consumption reported by states'
      )
    )
  ),
  
  # creates the tabs and displays the tables
  
  tabPanel("MF33G",
           h3("Gross Volume of Gasoline/Gasahol Reported by States 2017"),
           DT::dataTableOutput("mytable1")
           ),

  tabPanel("MF33GA16",
           h3("Gross Volume of Gasoline/Gasahol Reported by States 2016"),
           DT::dataTableOutput("mytable6")
  ),
  
  
  tabPanel("MF33CO16",
           h3("Monthly Special Fuels & Gasoline/Gasohol Reported by States 2016"),
           DT::dataTableOutput("mytable2")
  ),
  
  tabPanel("MF33SF17",
           h3("Monthly Special Fuels Reported by States 2017"),
           DT::dataTableOutput("mytable3")
  ),
  
  tabPanel("MF33SF16",
           h3("Monthly Special Fules Reported by States 2016"),
           DT::dataTableOutput("mytable4")
  ),
  
  tabPanel("Tax Rates",
           h3("Tax Rates on Motor Fuel"),
           DT::dataTableOutput("mytable5")
  )

)
   