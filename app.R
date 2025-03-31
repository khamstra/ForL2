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
                  list("Slope" = "S", "Aspect" = "A") 
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
      
      output$plot <- renderPlot({
            
            if(input$select == "S" ){
                  
                  tm_shape(slope, alpha = 0.5) +
                        tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)")
                  
            }
            
            else if(input$select == "A"){
                  
                  tm_shape(aspect) +
                        tm_raster(style = "cont")
                  
            }
                       
            
      })
      
      
}

shinyApp(ui = ui, server = server)
