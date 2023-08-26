library(shiny)
library(ggplot2)
library(sf)
library(sfdep)
library(spdep)
library(dplyr)
library(tidyr)
library(rgdal)
library(terra)
server <- function(input, output) {
  
  
  output$geo <- renderPlot({
    print(input$clustermap)
    #setwd(
    #  "/Users/josephcerniglia/Documents/eCornell Data Analytics in R/Hotspots")
    
    filestring <- tolower(input$select)
    tes_data <-  read.csv(paste0(filestring,"_tes.csv"))
    tes_data_g <- st_read(paste0(filestring,"_tes.shp"))
    tes_data <- cbind(tes_data, geometry = tes_data_g$geometry)
    row_count <- nrow(tes_data)
    boundary_factor <- (1/row_count)*100
    print(row_count)
    #tes_data<-get(paste0(tolower(input$select), "_data"))
    tes_data <- tes_data[!is.na(tes_data$tesctyscor),]
    # Create a neighbor list based on queen contiguity
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
    if (input$clustermap==TRUE) {
        #Identify neighbors, create weights, calculate spatial lag
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
        
        
            # final graph with piped input of tes_hot_spots
            tes_hot_spots |> 
              # with the columns 'gi' and 'p_folded_sim"
              # 'p_folded_sim' is the p-value of a folded permutation test
              select(gi, p_folded_sim, geometry) |> 
              mutate(
                # Add a new column called "classification"
                classification = case_when(
                  # Classify based on the following criteria:
                  gi > 0 & p_folded_sim <= 0.01 ~ "Very green",
                  gi > 0 & p_folded_sim <= 0.05 ~ "Moderately green",
                  gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat green",
                  gi < 0 & p_folded_sim <= 0.01 ~ "Very bare",
                  gi < 0 & p_folded_sim <= 0.05 ~ "Moderately bare",
                  gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat bare",
                  TRUE ~ "No cluster detected"
                ),
                # Convert 'classification' into a factor for easier plotting
                classification = factor(
                  classification,
                  levels = c("Very green", "Moderately green", "Somewhat green",
                             "No cluster detected",
                             "Somewhat bare", "Moderately bare", "Very bare")
                )
              ) |> 
              # Visualize the results with ggplot2
              #green red palette is #8
              #Palette #7 will turn the green to blue for a greater
              #accessibility to the color-blind.
              ggplot(aes(fill = classification)) +
              geom_sf(aes(geometry=geometry),color = "black", 
                      lwd = boundary_factor) +
              coord_sf(expand = TRUE) +
              scale_fill_brewer(type = "div", palette = 8, direction=-1) +
              labs(
                caption = 
                  "Data source: https://www.treeequityscore.org/methodology") +
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
                base_size = 20,
                base_family = "",
                base_line_size = base_size/20,
                base_rect_size = base_size/20
              ) +
              labs(
                fill = "Green Spot \n Classification",
                title = paste("Tree Equity Green Spots in", input$select))
            }
    else {ggplot(tes_subset) +
        geom_sf(aes(geometry=geometry,fill = tesctyscor), color = "black", 
                lwd = boundary_factor) +
        scale_fill_gradient(name = "Tree Equity Score",
                            low = "red",
                            high = "darkgreen") +
        ggtitle(paste("Tree Equity Scores of",input$select,"Neighborhoods")) +
        labs(caption = 
               "Data source: https://www.treeequityscore.org/methodology") +
        theme_void(base_size = 18,
                   base_family = "",
                   base_line_size = base_size/22,
                   base_rect_size = base_size/22) +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "right")}
  })
    output$hist <- renderPlot({
      filestring <- tolower(input$select)
      tes_data <-  read.csv(paste0(filestring,"_tes.csv"))$tesctyscor
      hist(tes_data)
    })
      

  
}
