library(shiny)
#library(sf)
#library(sfdep)
#library(spdep)
library(dplyr)
library(tidyr)
#library(ggplot2)


server <- function(input, output) {
filestring <- tolower(input$select)
#define histogram break points
hist_breaks <- hist(read.csv(paste0(filestring,"_tes.csv"))$tesctyscor)$breaks
  
color_list <- rep('darkgreen', length(hist_breaks))
color_list[hist_breaks < 80] <- 'green'
color_list[hist_breaks < 60] <- 'brown'  
   

output$hist <- renderPlot({
      #filestring <- tolower(input$select)
      tes_data <-  read.csv(paste0(filestring,"_tes.csv"))$tesctyscor
      hist(tes_data,breaks="Sturges", main=paste0(
      "Distribution of tree equity scores for ",input$select),
      xlab="Tree Equity Score [range 0-100]",col=color_list)
    })
      

  
}
