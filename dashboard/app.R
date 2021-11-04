# Load packages -----------------------------------------------------
library(shiny)
library(ggplot2)
library(ggmap)
library(ggrepel)
library(RColorBrewer)

#Run data_cleaner
suppressWarnings(source("data_cleaner.R"))

#position of site labels
nudge <- c(rep(0.03,4),0,0,0,0.03,0.03,0.01, 0.03)
#Set your API Key to call map from google
ggmap::register_google(key = "AIzaSyA-US0xjpp6-mru0kgOHDSF2eUOHgGsJGI")

# Load data ---------------------------------------------------------
load("data/data.Rdata")


parameters <- c("sal", "temp", "pH", "turbidity", "ODO_mgl", "Chl", "pe", "Nitrate", "Ammonia", "Phosphate", "Sulphate", "Sulphide")
names(parameters) <- c("Salinity", "Water Temperature", "pH", "Turbidity", "Dissolved Oxygen", "Chlorophyll", "Phycoerythrin", "Nitrate", "Ammonia", "Phosphate", "Sulphate", "Sulphide")

titles <- c("Salinity", "Water Temperature", "pH", "Turbidity", "Dissolved Oxygen", "Chlorophyll", "Phycoerythrin", "Nitrate", "Ammonia", "Phosphate", "Sulphate", "Sulphide")
names(titles) <- parameters

units <- c("PSU", "°C", "", "FNU", "mg/L", rep("µg/L",2), rep("mg/L",5))
names(units) <- parameters


# Define UI ---------------------------------------------------------
ui <- fluidPage(
    
    # Sidebar layout with a input and output definitions
    sidebarLayout(
        
        # Inputs: Select variables to plot
        sidebarPanel(
            
            # Select variable 
            selectInput(inputId = "param1", 
                        label = "Parameter 1:",
                        choices = parameters, 
                        selected = "Chl"),
            
            # Select variable 
            selectInput(inputId = "param2", 
                        label = "Parameter 2:",
                        choices = parameters, 
                        selected = "Nitrate"),
            
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
            
            # Show mapplot1
            plotOutput(outputId = "mapplot1"),
            
            # Show mapplot2
            plotOutput(outputId = "mapplot2"),
            
            # Show data table
            DT::dataTableOutput(outputId = "sstable")
        )
    )
)

# Define server function --------------------------------------------
server <- function(input, output) {
    
    
    # Create mapplot1 object the plotOutput function is expecting
    output$mapplot1 <- renderPlot({
        

        myPalette <- colorRampPalette(brewer.pal(9, "YlGnBu"), space="Lab") #set unique color palatte for color gradient
        
        p <- qmap(c(lon = -116.048047, lat =  33.47), zoom = 13, source = "google", maptype = "terrain", crop = FALSE) +
            geom_label_repel(data = d.all, aes(x = Longitude, y = Latitude, label = Station), colour = "black", 
                             force = 1, size = 4.5, box.padding = 2, nudge_x = nudge,
                             min.segment.length = 1, segment.size = 1.1, segment.color = "darkgrey") +
            geom_point(data = d.all, aes_string(x = "Longitude", y = "Latitude", color = input$param1), alpha=1, stroke = 4, size = 4, na.rm = T) +
            scale_colour_gradientn(colours = myPalette(100),
                                   guide = "colourbar",
                                   name = units[names(units) ==  input$param1]) + 
            ggtitle(titles[names(titles) == input$param1])+
            theme(plot.title = element_text(size = 20, face = "bold"),
                  legend.position="bottom", legend.box = "horizontal")
        
        suppressWarnings(print(p))
    })
    
    # Create mapplot2 object the plotOutput function is expecting
    output$mapplot2 <- renderPlot({
        
        
        myPalette <- colorRampPalette(brewer.pal(9, "YlOrRd"), space="Lab") #set unique color palatte for color gradient
        
        p <- qmap(c(lon = -116.048047, lat =  33.47), zoom = 13, source = "google", maptype = "terrain", crop = FALSE) +
            geom_label_repel(data = d.all, aes(x = Longitude, y = Latitude, label = Station), colour = "black", 
                             force = 1, size = 4.5, box.padding = 2, nudge_x = nudge,
                             min.segment.length = 1, segment.size = 1.1, segment.color = "darkgrey") +
            geom_point(data = d.all, aes_string(x = "Longitude", y = "Latitude", color = input$param2), alpha=1, stroke = 4, size = 4, na.rm = T) +
            scale_colour_gradientn(colours = myPalette(100),
                                   guide = "colourbar",
                                   name = units[names(units) ==  input$param2]) + 
            ggtitle(titles[names(titles) == input$param2])+
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
