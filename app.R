library(shiny)
library(ggplot2)
library(sf)
library(sfdep)
library(spdep)
library(dplyr)
library(tidyr)
library(rgdal)
library(terra)
library(shinydashboard)
library(shinythemes)
library(shiny.fluent)
library(shinyjs)

options(scipen=999)
abbrev<-c("AK", "AL", "AR", "AZ", "CA", 
          "CO", "CT", "DC", "DE", "FL", 
          "GA", "HI", "IA", "ID", "IL", 
          "IN", "KS", "KY", "LA", "MA", 
          "MD", "ME", "MI", "MN", "MO", 
          "MS", "MT", "NC", "ND", "NE", 
          "NH", "NJ", "NM", "NV", "NY", 
          "OH", "OK", "OR", "PA", "RI", 
          "SC", "SD", "TN", "TX", "TU",
										"UT", "VA", "VT", "WA", "WI", 
										"WV", "WY")
states<-c("Alaska", "Alabama", "Arkansas", "Arizona",  
          "California", "Colorado", "Connecticut", "District of Columbia",
          "Delaware", "Florida", "Georgia", 
          "Hawaii", "Iowa", "Idaho", "Illinois", "Indiana", "Kansas", 
          "Kentucky", "Louisiana", "Massachusetts", "Maryland", "Maine", 
          "Michigan",  "Minnesota","Missouri", 
          "Mississippi", "Montana", "North Carolina",  "North Dakota", 
          "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", 
          "New York", "Ohio", "Oklahoma", "Oregon",   
          "Pennsylvania", "Rhode Island", 
          "South Carolina", "South Dakota", "Tennessee", 
										"Texas", "Tucson, Arizona", 
          "Utah", "Virginia",
          "Vermont",  "Washington", "Wisconsin","West Virginia", "Wyoming")
state_lookup <- cbind(abbrev,states)

longlist <- list('Alabama' = 'AL','Alaska'='AK','Arizona'='AZ',
																	'Arkansas'='AR','California'='CA','Colorado'='CO',
																	'Connecticut'='CT','Delaware'='DE',
																	'District of Columbia'='DC','Florida'='FL','Georgia'='GA',
																	'Hawaii'='HI','Idaho'='ID','Illinois'='IL','Indiana'='IN',
																	'Iowa'='IA','Kansas'='KS','Kentucky'='KY','Louisiana'='LA',
																	'Maine'='ME','Maryland'='MD','Massachusetts'='MA',
																	'Michigan'='MI','Minnesota'='MN','Mississippi'='MS',
																	'Missouri'='MS','Montana'='MT','Nebraska'='NE','Nevada'='NV',
																	'New Hampshire'='NH','New Jersey'='NJ', 'New Mexico'='NM',
																	'New York'='NY','North Carolina'='NC','North Dakota'='ND',
																	'Ohio'='OH','Oklahoma'='OK','Oregon'='OR',
																	'Pennsylvania'='PA','Rhode Island'='RI','South Carolina'='SC',
																	'South Dakota'='SD','Tennessee'='TN', 'Texas'='TX', 
																	'Tucson, Arizona'='TU','Utah'='UT','Vermont'='VT',
																	'Virginia'='VA','Washington'='WA','West Virginia'='WV',
																	'Wisconsin'='WI','Wyoming'='WY')
shortlist <- list('Connecticut'='CT','Delaware'='DE','Florida'='FL',
																		'Massachusetts'='MA','New Jersey'='NJ', 'Rhode Island'='RI',
																		'Tucson, Arizona'='TU')

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Tree Equity in the United States"),
  tags$style("
      .checkbox { /* checkbox is a div class*/
        line-height: 8px;
        margin-bottom: 20px; /*set the margin, so boxes don't overlap*/
      }
      input[type='checkbox']{ /* style for checkboxes */
        width: 12px; /*Desired width*/
        height: 12px; /*Desired height*/
        line-height: 18px; 
      }
      span { 
          margin-left: 0px;  /*set the margin, so boxes don't overlap labels*/
          line-height: 15px; 
      }
  "),

  sidebarLayout(
    
    sidebarPanel(checkboxInput("specialstates", "Limit states to those with 
    																											most complete data.", FALSE),
    													verbatimTextOutput("value3"),
    													uiOutput("specialStates"),
    												selectInput( inputId = "select", 
                              label = "Select a state or city:", 
                              choices = longlist),
            width=2,
            checkboxInput("clustermap", "Cluster Map", FALSE),
            verbatimTextOutput("value"),
            #tags$br(),
            p(id = "element", "Adjust green threshold:"),
    
												SpinButton.shinyInput("spin", value = 0, min = -3, max = 3, 
																																		step = 1, label="",
																																		textOutput(outputId = "spin")),
												tags$br(),
            checkboxInput("colorblind", 
            "Color Enhancement (for better contrast or to assist 
            the color-blind)", FALSE),
            verbatimTextOutput("value2")),

    mainPanel(
      fluidPage(
        column(width = 7,
        plotOutput(outputId = "geo",width = "700px", height="700px"
                   
                   )
        ),
        
        column(width = 3,
        plotOutput(outputId="hist",width = "280px", height="250px"
                   )),
  

tags$footer(
  "Data Source: ",
  tags$a(
    "treeequityscore.org/methodology.",
    target = "_blank",
    href = "https://www.treeequityscore.org/methodology"),
   "  Spatial autocorellation template: ",
  tags$a(
    "https://rpubs.com/heatherleeleary/hotspot_getisOrd_tut.",
    target = "_blank",
    href = "https://rpubs.com/heatherleeleary/hotspot_getisOrd_tut"),
    style = "position:absolute;bottom:0;width:100%;color:black;
     text-align:center;"
)))))



server <- function(input, output, session) {
 observe({
 	if (input$specialstates==TRUE){updateSelectInput(session = session,
 																																																		inputId = "select",
 																										choices = shortlist)
 	}
 	else
 	{updateSelectInput(session = session,
 																				inputId = "select",
 																				choices = longlist)}
 	
 }) 
 
	observe({
		if(input$clustermap==FALSE)
		 {shinyjs::show("spin")
			html("element", "Adjust green threshold:")
			}
		else
		{
			html("element", "")
			shinyjs::hide("spin")
		}
	})
	 
  output$geo <- renderPlot({
  	 # Create a Progress object
   	progress <- shiny::Progress$new()
  	 # Make sure it closes when we exit this reactive, even if there's an error
  	 on.exit(progress$close())
  	 progress$set(message = "Building tree equity map...", value = 0)
    #comment out the set working directory function to deploy to shinyapps.io
    #setwd(
    #"/Users/josephcerniglia/Documents/eCornell Data Analytics in R/Hotspots/App-4")
    #equitable red palette is #8
    #Palette #7 will turn the equitable to blue for a greater
    #accessibility to the color-blind.
  	 #90EE90=light equitable ;  #013220=dark equitable. #0000FF=dark blue
  	 #800000=maroon #B66B3E=tan #8E4B32=chestnut #ADD8E6=light blue (cyan)
  	 #print("#3D0C02") #=dark bean
  	 #F5F5F5=light grey ;  ##57504d=dark grey
    if (input$colorblind==TRUE) {Pale<-7} else {Pale<-8}
    if (input$colorblind==TRUE) {Gradi1<-"#800000" 
    Gradi2<-"#ADD8E6"}
  	 else 
  	 {Gradi1<-"#5C2C26" 
    	Gradi2<-"#90EE90"}
  	 #print(input$spin)
  	 #print(input$colorblind)
  	 req(input$spin)
  
			  if (input$spin==-3) {
			  	 	if (input$colorblind==TRUE) {Gradi<-c("#9f1D34",
			  	 														"#E3EEEF","#8FDDF8","#6ED3F6","#2abff3","#0DB3EC","#097FA9",
			  	 																																							"#076687","#1750AC")}
			  	 	else {Gradi<-c("#9F1D34",
			  	 																"#93F593","#72F272","#51EF51","#14E514","#11C411","#0EA30B",
			  	 																"#0B820B","#086208")}
			  	 }	
			  else if (input$spin==-2) {
			  	 	if (input$colorblind==TRUE) {Gradi<-c("#9f1D34","#E7626A","#F1988F",
			  	 																"#E3EEEF",	"#8FDDF8","#2abff3","#0B99CA","#0DB3EC","#076687",
			  	 																																							"#1750AC")}
			  	 	else {Gradi<-c("#9F1D34","#E7626A","#F1988F",
			  	 																"#93F593", "#51EF51","#14E514","#11C411","#0EA30B",
			  	 																"#0B820B","#086208")}
			  	 }	 
			  else if (input$spin==-1) {
			  	 	if (input$colorblind==TRUE) {Gradi<-c("#9f1D34","#E7626A","#F1988F",
			  	 																																							"#FCCCA5",
			  	 																																							"#E3EEEF",	"#8FDDF8","#0B99CA", 
			  	 																																							"#0DB3EC",
			  	 																																						 "#076687","#1750AC")}
			  	 	else {Gradi<-c("#9F1D34","#E7626A","#F1988F","#FCCCA5",
			  	 																"#93F593", "#51EF51","#11C411","#0EA30B","#0B820B",
			  	 																"#086208")}
			  	 }
			 else if (input$spin==0) {
			  	 					if (input$colorblind==TRUE) {Gradi<-c("#9f1D34","#E7626A","#F1988F",
			  	 																																											"#FCCCA5","#FCE2B6",
			  	 																							"#E3EEEF","#8FDDF8","#0DB3EC", "#097FA9","#1750AC")}
			  	 	    
			  	 	else {Gradi<-c("#9F1D34","#E7626A","#F1988F","#FCCCA5","#FCE2B6",
			  	 																"#93F593", "#51EF51","#11C411","#0B820B","#086208")}
			  	 }
			 else if (input$spin==1) {
			  	 
							  	 if (input$colorblind==TRUE) {Gradi<-c("#9f1D34","#E7626A","#F1988F",
							  	 																																						"#FCCCA5","#FCE2B6",
							  	 																																					 "#8FDDF8","#097FA9","#1750AC")}
							  	 	else {Gradi<-c("#9F1D34","#E7626A","#F1988F","#FCCCA5","#FCE2B6",
							  	 																"#11C411","#0B820B","#086208")}
			  	 }
			 else if (input$spin==2) {
			  	 	   if (input$colorblind==TRUE) {Gradi<-c("#9f1D34","#E7626A","#F1988F",
			  	 	   																																						"#FCCCA5","#FCE2B6",
			  	 																																																															"#8FDDF8",
			  	 	   																																						"#1750AC")}
			  	 	  else {Gradi<-c("#9F1D34","#E7626A","#F1988F","#FCCCA5","#FCE2B6",
			  	 																  "#0B820B","#086208")}
			  	 }
			 else if (input$spin==3) {
			  	 	if (input$colorblind==TRUE) {Gradi<-c("#9f1D34","#E7626A","#F1988F",
			  	 																																							"#FCCCA5","#FCE2B6",
			  	 																																							"#1750AC")}
			  	 	else {Gradi<-c("#9F1D34","#E7626A","#F1988F","#FCCCA5","#FCE2B6",
			  	 																"#086208")}
			  	 }

  	 n<-7
  	 progress$inc(1/n, detail = paste("Reading in data: step", 1))
    filestring <- tolower(input$select)
    tes_data_csv <-  read.csv(paste0(filestring,"_tes.csv"))
    tes_data_g <- st_read(paste0(filestring,"_tes.shp"))
    tes_data <- cbind(tes_data_csv, geometry = tes_data_g$geometry)
    row_count <- nrow(tes_data)
    boundary_factor <- (1/row_count)*100
    print(row_count)
    if (filestring=='tu') {
    colnames(tes_data)[colnames(tes_data) == "TreeEquityScore"] ="tesctyscor"}
    tes_data <- tes_data[!is.na(tes_data$tesctyscor),]
    
    if (input$clustermap==TRUE) {
    			
				    #if (input$select=='CA') {tes_data <- sample_n(tes_data,10000)}
				    # Create a neighbor list based on queen contiguity
				    progress$inc(1/n, detail = paste("Removing polygons with empty neighbor 
				    																																	sets from the data: step", 2))
				    list_nb <- poly2nb(tes_data$geometry, queen = TRUE)
				    # Check for empty neighbor sets
				    # card() calculates number of neighbors for each polygon in the list
				    # which() finds polygons with 0 neighbors
				    empty_nb <- which(card(list_nb) == 0)
				    #empty_nb  
				    # Remove polygons with empty neighbor sets from the data
				    if (length(empty_nb)>0) {
				      print('Empty neighbor sets found')
				      tes_subset <- tes_data[-empty_nb, ]
				    } else {
				      print('No empty neighbor sets found')
				      tes_subset <- tes_data
				    }
    
    
    
				    # Now that we removed empty neighbor sets, (tes_subset).
				    # once again identify neighbors with queen contiguity (edge/vertex 
				    #touching)
				    #View(tes_subset)
				    tes_nb <- poly2nb(tes_subset$geometry, queen=TRUE)
				    #View(tes_nb)
				    # Binary weighting assigns a weight of 1 to all neighboring features 
				    # and a weight of 0 to all other features
				    tes_w_binary <- nb2listw(tes_nb, style="B")
				    ##tes_w_binary_g <- nb2listw(tes_nb_g, style="B")
				    
				    # Calculate spatial lag of tree equity score
				    tes_lag <- lag.listw(tes_w_binary, tes_subset$tesctyscor)
				    progress$inc(1/n, detail = paste("Calculating global p value: step", 3))
				    #globalG.test (or global_g_test()) computes a global test for spatial 
				    #autocorrelation using a Monte Carlo simulation approach (simulated 
				    #spatial datasets that have the same spatial structure as the original 
				    #data but are randomly permuted). It tests the null hypothesis of no 
				    #spatial autocorrelation against the alternative hypothesis of positive 
				    #spatial autocorrelation.
				    #The output includes a p-value.  Is it significant?
				    p_value <- globalG.test(tes_subset$tesctyscor, tes_w_binary)$p[1,]
				    p2 <- if(p_value<=.001 & input$select != "DC")
				      {"[global p < 0.001]"}
				    else if(input$select != "DC") {paste0(
				       "[global p=",round(p_value,3),"]")}
				    else {"Insufficient Data"}
				    #print(p2)
    

    				progress$inc(1/n, detail = paste(
    				"Calculating spatial lag, a weighted average of the neighboring tree 
    				#equity scores: step", 4))
    				
    				
        #Identify neighbors, create weights, calculate spatial lag.
        #Definition of spatial lag:
        #a weighted sum or a weighted average of the neighboring values for the 
        #variable of interest
        tes_nbs <- tes_subset |> 
          mutate(
            nb = st_contiguity(geometry),       # neighbors share border/vertex
            wt = st_weights(nb),                 # row-standardized weights
            tes_lag = st_lag(tesctyscor, nb, wt) # calculate spatial lag of 
                                                # tesctyscor (tree equity score)
          ) 
        
     
        # Increment the progress bar, and update the detail text.
        progress$inc(1/n, detail = paste(
        	"Calculating Gi, indicating cluster strength, and local p's with a 
        	Monte Carlo simulation: step", 5))
        
        # Calculate the Gi using local_g_perm, as tes_hot_spots.
        #The Gi is the ratio of the spatial lag of a feature to the sum of the 
        #feature’s values for its neighbors. A positive Gi value indicates that 
        #a feature and its neighbors have high values, while a negative Gi value 
        #indicates that they have low values. The magnitude of the Gi value 
        #indicates the strength of the clustering.
        
        tes_hot_spots <- tes_nbs |> 
          mutate(
            Gi = local_g_perm(tesctyscor, nb, wt, nsim = 999)
            # nsim = number of Monte Carlo simulations (999 is default)
          ) |> 
          # The new 'Gi' column itself contains a dataframe 
          # We can't work with that, so we need to 'unnest' it
          unnest(Gi) 
        
            progress$inc(1/n, detail = paste("Plotting final map:", 6)) 
            # final graph with piped input of tes_hot_spots
            tes_hot_spots |> 
              # with the columns 'gi' and 'p_folded_sim"
              # 'p_folded_sim' is the p-value of a folded permutation test
              select(gi, p_folded_sim, geometry) |> 
              mutate(
                # Add a new column called "classification"
                classification = case_when(
                  # Classify based on the following criteria:
                  gi > 0 & p_folded_sim <= 0.01 ~ "Very equitable",
                  gi > 0 & p_folded_sim <= 0.05 ~ "Moderately equitable",
                  gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat equitable",
                  gi < 0 & p_folded_sim <= 0.01 ~ "Very inequitable",
                  gi < 0 & p_folded_sim <= 0.05 ~ "Moderately inequitable",
                  gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat inequitable",
                  TRUE ~ "No cluster detected"
                ),
                # Convert 'classification' into a factor for easier plotting
                classification = factor(
                  classification,
                  levels = c("Very equitable", "Moderately equitable", 
                  											"Somewhat equitable",
                             "No cluster detected",
                             "Somewhat inequitable", "Moderately inequitable", 
                  											"Very inequitable")
                )
              ) |> 
              # Visualize the results with ggplot2
              ggplot(aes(fill = classification)) +
              geom_sf(aes(geometry=geometry),color = "black", 
                      lwd = boundary_factor) +
              coord_sf(expand = TRUE) +
              scale_fill_brewer(type = "div", palette = Pale, direction=-1) +
      
              theme_void(
                base_size = 18,
                base_family = "",
                base_line_size = base_size/22,
                base_rect_size = base_size/22
              ) +
              theme(
                    plot.title = element_text(hjust = 0.5),
                    legend.position = "right") +
              theme_void(
                base_size = 18,
                base_family = "",
                base_line_size = base_size/20,
                base_rect_size = base_size/20
              ) +
              labs(
                fill = "Hot Spot \n Classification",
                title = paste("Tree Equity Hot Spots in", 
                              states[which(state_lookup == input$select)],
                              "\n",p2))
            }
    
    
    
    else {
    
      
    	 ggplot(tes_data) +
      geom_sf(aes(geometry=geometry,fill = tesctyscor), color = "black", 
                lwd = boundary_factor) +
    	 
    		scale_fill_gradientn(colors = Gradi,
    																							name = "Tree Equity Score") + 
    		
      #scale_fill_gradient(
      #    name = "Tree Equity Score",
      #                      low =  Gradi1,
      #                      high = Gradi2) +
       
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "right") +
        theme_void(base_size = 18,
                   base_family = "",
                   base_line_size = base_size/22,
                   base_rect_size = base_size/22) +
        labs(title = paste(
        "Tree Equity Scores of",
        states[which(state_lookup == input$select)],"\nNeighborhoods"))
        }
  })
    output$hist <- renderPlot({
      filestring <- tolower(input$select)
      tes_data <-  read.csv(paste0(filestring,"_tes.csv"))
      if (filestring=='tu') {
         colnames(tes_data)[colnames(tes_data) =="TreeEquityScore"]="tesctyscor"
         tes_data <- tes_data$tesctyscor
         loc<-'Tucson'}
      else
         {tes_data <-  tes_data$tesctyscor
         loc=input$select}
      hist(tes_data, main=paste0(
      "Tree equity scores for ", loc),
      xlab=paste0("Tree Equity Score [range 0-100]"))
    })
      

    
}
shinyApp(ui, server)

