library(shiny)
library(ggplot2)
library(dplyr)

# Define UI ----
ui <- fluidPage(
  titlePanel("EpiPlot"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Patients data"),
      fileInput("Data", "", accept=c("text/csv", "text/txt")),
      dateRangeInput("DateRange", "Dates range"),
      hr(),
      
      h3("Strains clusters"),
      checkboxInput("checkCluster", label = "Display", value = FALSE),
      numericInput("DotSize", label = "Dot", value = 1),
      numericInput("SegSize", label = "Segment", value = 1),
      textInput("ClustColor", label = "Color", value = "black")
      ),
    
    mainPanel(
      plotOutput("timeline"),
      tableOutput("table")
      )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # Load table
  get_data <- reactive({
    
    # Load the table
    table <- read.csv(input$Data$datapath)
    
    # Convert into time
    table <- table %>% mutate(in_date = as.POSIXct(paste(in_date, "00:00:00")), 
                              out_date = as.POSIXct(paste(out_date, "23:59:59")), 
                              sampling = as.POSIXct(paste(sampling, "12:00:00")))

    # Update date range widget
    updateDateRangeInput(session, "DateRange", start=min(table$in_date), end=max(table$out_date))
    
    return(table)
  })
  
  # Filtering data
  polished_data <- reactive({
    pol_data <- get_data()
    
    # Filter by date
    pol_data <- pol_data[(which(pol_data$in_date>=as.POSIXct(input$DateRange[1]))),]
    pol_data <- pol_data[(which(pol_data$out_date<=as.POSIXct(input$DateRange[2]))),]
    
    return(pol_data)
  })
  
  # Display plot when data uploaded
  output$timeline <- renderPlot({
    
    # Return nothing if no data loaded
    if (is.null(input$Data))
      return(NULL)
    
    # Import filtered data
    data <- polished_data()
    
    # Define best segment and dot size depending on patient number (with minimums)
    line_size <- max(3, 25-length(levels(as.factor(data$patient))))
    cluster_line_size <- 3

    # Main plot
    plot <- ggplot(data, aes(x=in_date, xend=out_date, y=patient, yend=patient, color=unit)) +
      geom_segment(size=line_size) +
      theme_bw()+ #use ggplot theme with black gridlines and white background
      theme(axis.title = element_blank())
    
    if (input$checkCluster == TRUE){
      # Add cluster segments
      for (level in levels(factor(data$cluster))){
        df <- data[which(data$cluster==level),]
        df <- df[!duplicated(df$patient),]
        df <- df[order(df$sampling),]
        if (nrow(df) > 1){
          for (line in 1:(nrow(df)-1)){
            plot <- plot + geom_segment(x=df[line,"sampling"], xend=df[line+1,"sampling"], 
                                        y=df[line,"patient"], yend=df[line+1,"patient"],
                                        color=input$ClustColor, size=input$SegSize)
          }
        }
      }
      
      # Add sampling date
      plot <- plot + geom_point(aes(x=sampling, y=patient), colour=input$ClustColor, size=input$DotSize)
    }
    
    plot
    })
  
  output$table <- renderTable({
    
    if (is.null(input$Data))
      return(NULL)
    
    table <- polished_data()
    table <- table %>% mutate(in_date = as.character(as.POSIXct(in_date)), 
                              out_date = as.character(as.POSIXct(out_date)), 
                              sampling= as.character(as.POSIXct(sampling)))
    return(table)
      })
  
}

shinyApp(ui = ui, server = server)