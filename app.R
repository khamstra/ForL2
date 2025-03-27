library(shiny)
library(bslib)
library(terra)
library(tmap)

ui <- page_sidebar(
      
      title = "Extract Slope and Aspect",
      
      sidebar = sidebar(
         
            selectInput( 
                  "select", 
                  "Chose an option:", 
                  list("Slope" = "S", "Aspect" = "A", "Reclassified Aspect" = "RA") 
            )   
            
      ),
      
      card(
            
            plotOutput("plot", brush = "plot_brush")
      ), 
      
      theme = bs_theme(bootswatch = "morph")
      
)

server <- function(input, output){
      
      # Read inventory data
      dem <- rast("./files/unit2.img")
      
      # a. Extract Slope
      slope <- terrain(dem, v = "slope", unit = "degrees", neighbors = 8)
      
      # b. Extract Aspect:
      aspect <- terrain(dem, v = "aspect", unit = "degrees")
      
      # a. Create Aspect Classification Matrix:
      asp_class <- matrix(c(
            0, 45, 1,
            45, 90, 2,
            90, 175, 2,
            175, 180, 3,
            180, 225, 3,
            225, 270, 4,
            270, 315, 4,
            315, 360, 1
      ), ncol = 3, byrow = TRUE)
      
      
      # b. Reclassify Aspect:
      asp <- classify(aspect, asp_class)
      
      
      output$plot <- renderPlot({
            
            if(input$select == "S" ){
                  
                  tm_shape(slope, alpha = 0.5) +
                        tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)")
                  
            }
            
            else if(input$select == "A"){
                  
                  tm_shape(aspect) +
                        tm_raster(style = "cont")
                  
            }
            
            else if(input$select == "RA"){
                  
                  # c. Visualize Reclassified Aspect:
                  
                  tm_shape(asp) +
                        tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
                                  labels = c(NA, "North", "East", "South", "West"), alpha = 0.2)
                  
            }
            
            
      })
      
      
}

shinyApp(ui = ui, server = server)
