ui <- fluidPage(
  

  sidebarLayout(
    #comment out lines 11, 12, 13, 18, 19, 21, 22 for histograms only
    sidebarPanel(selectInput( inputId = "select", 
                              label = "Select a state to see a Tree Map", 
                              choices = sort(c("AK","AL","CT","FL","MA","VT",
                                               "OR","CA","NY",
                                               "MS","ME","TX","CO" ))),width=4
   #              , 
   #         checkboxInput("clustermap", "cluster map", TRUE),
  #          verbatimTextOutput("value")
                ),
    
    mainPanel(
      fluidPage(
        column(width = 8,
    #    plotOutput(outputId = "geo",width = "900px", height="1000px"
    #               
    #               )
    #    ),
        
        column(width = 8,
        plotOutput(outputId="hist",width = "500px", height="500px"
                   ))))
  
)))
