library(shiny)
#library(sf)
#library(sfdep)
#library(spdep)
library(dplyr)
library(tidyr)
#library(ggplot2)


server <- function(input, output) {
  
  
   

output$hist <- renderPlot({
      filestring <- tolower(input$select)
      tes_data <-  read.csv(paste0(filestring,"_tes.csv"))$tesctyscor
      hist(tes_data,color="darkgreen")
    })
      

  
}
