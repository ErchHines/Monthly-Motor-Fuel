# FHWA Monthly Fuels Report Concept
This project was created as a concept for displaying FHWA's monthly motor fuel data collected from the States. It is an R Shiny app that includes an interactive map that displays the change in fuel consumption from a year ago for each state along with a graph of that State's consumption over the year to date as well as time series data and the legacy motor fuel tables that are part of the current monthly motor fuel reports available here: https://www.fhwa.dot.gov/policyinformation/motorfuelhwy_trustfund.cfm

This was based on the September 17 report since that was publicly available when I created the prototype. I used csvs created from the excel file for the prototype so that I could share the code and allow others to run it without querying the server directly with RODBC. 

The program is designed to provide a more visual display of the fuel data for use by analysts. The map and graph would allow analysts to quickly determine if there were any unusual changes to the data. The time series analysis that extracts seasonality and an overall trend from the data was created so that analysts could see easily if there was an unexplained remainder that might require further investigation. 

