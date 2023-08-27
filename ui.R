ui <- fluidPage(
  

  sidebarLayout(
    
    sidebarPanel(selectInput( inputId = "select", 
                              label = "Select a state to see a Tree Map", 
                              choices = sort(c("AK","AL","CT","FL","MA","VT",
                                               "OR","CA","NY",
                                               "MS","ME","TX" ))),width=1
                 #, 
            #checkboxInput("clustermap", "cluster map", TRUE),
            #verbatimTextOutput("value")
                ),
    
    mainPanel(
      fluidPage(
        #column(width = 8,
        #plotOutput(outputId = "geo",width = "900px", height="1000px"
                   
                  # )
        #),
        
        column(width = 3,
        plotOutput(outputId="hist",width = "280px", height="250px"
                   ))))
  
))
