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
      #define histogram break points
      hist_breaks <- hist(tes_data)$breaks
      #define colors to use in histogram based on break points
      color_list <- rep('darkgreen', length(hist_breaks))
      color_list[hist_breaks < 85] <- 'green'
      color_list[hist_breaks < 70] <- 'brown'
      hist(tes_data, breaks=30, main=paste0("Distribution of tree equity scores for ",input$select),
      xlab="Tree Equity Score [range 0-100]",col=color_list,
       cex.axis=0.5, font.main=1, cex.main=0.8)})
    }


      

  

