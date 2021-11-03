# Load packages -----------------------------------------------------
library(shiny)
library(ggplot2)
library(ggmap)
library(ggrepel)
library(RColorBrewer)

#position of site labels
nudge <- c(rep(0.03,4),0,0,0,0.03,0.03,0.01, 0.03)
#Set your API Key to call map from google
ggmap::register_google(key = "AIzaSyA-US0xjpp6-mru0kgOHDSF2eUOHgGsJGI")

# Load data ---------------------------------------------------------
load("data/data.Rdata")


# Define UI ---------------------------------------------------------
ui <- fluidPage(
    
    # Sidebar layout with a input and output definitions
    sidebarLayout(
        
        # Inputs: Select variables to plot
        sidebarPanel(
            
            # Select variable for y-axis
            selectInput(inputId = "param", 
                        label = "Parameter:",
                        choices = parameters, 
                        selected = "Chl"),
            
            # Show data table
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = FALSE),
            
            # YSI Button
            downloadButton("downloadYSI", "Download Sensor"),
            
            # YSI Button
            downloadButton("downloadNUTS", "Download Nutrient")
        ),
        
        # Output
        mainPanel(
            
            # Show mapplot
            plotOutput(outputId = "mapplot"),
            
            # Show data table
            DT::dataTableOutput(outputId = "sstable")
        )
    )
)

# Define server function --------------------------------------------
server <- function(input, output) {
    
    
    # Create mapplot object the plotOutput function is expecting
    output$mapplot <- renderPlot({
        

        myPalette <- colorRampPalette(brewer.pal(9, colors[1]), space="Lab") #set unique color palatte for color gradient
        
        p <- qmap(c(lon = -116.048047, lat =  33.47), zoom = 13, source = "google", maptype = "terrain", crop = FALSE) +
            geom_label_repel(data = d.all, aes(x = Longitude, y = Latitude, label = Station), colour = "black", 
                             force = 1, size = 4.5, box.padding = 2, nudge_x = nudge,
                             min.segment.length = 1, segment.size = 1.1, segment.color = "darkgrey") +
            geom_point(data = d.all, aes_string(x = "Longitude", y = "Latitude", color = input$param), alpha=1, stroke = 4, size = 4, na.rm = T) +
            scale_colour_gradientn(colours = myPalette(100),
                                   guide = "colourbar",
                                   name = units[names(units) ==  input$param]) + 
            ggtitle(titles[names(titles) == input$param])+
            theme(plot.title = element_text(size = 20, face = "bold"),
                  legend.position="bottom", legend.box = "horizontal")
        
        suppressWarnings(print(p))
    })
    
    # Print data table if checked
    output$sstable <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = d.all, 
                          options = list(pageLength = 11), 
                          rownames = FALSE)
        }
    )
    
    # Downloadable csv of YSI dataset ----
    output$downloadYSI <- downloadHandler(
        filename = function() {
            "SSET_YSI.csv"
        },
        content = function(file) {
            write.csv(d.ysi, file, row.names = FALSE)
        }
    )
    
    # Downloadable csv of nutrient dataset ----
    output$downloadNUTS <- downloadHandler(
        filename = function() {
            "SSET_nutrients.csv"
        },
        content = function(file) {
            write.csv(d.photo, file, row.names = FALSE)
        }
    )
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui, server)