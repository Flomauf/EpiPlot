library(shiny)
library(ggplot2)

# Define UI ----
ui <- fluidPage(
  titlePanel("EpiPlot"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Patients data"),
      fileInput("Data", "", accept=c("text/csv", "text/txt")),
      dateRangeInput("DateRange", "Dates range"),
      
      h3("Strains clusters"),
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
    table <- table %>% mutate(in_date = as.POSIXct(paste(in_date, "00:00:01 AM")), 
                              out_date = as.POSIXct(paste(out_date, "11:59:59 PM")), 
                              sampling=as.POSIXct(paste(sampling, "12:00:00 PM")))

    # Update date range widget
    updateDateRangeInput(session, "DateRange", start=min(table$in_date), end=max(table$out_date))
    
    return(table)
  })
  
  # Filtering data
  polished_data <- reactive({
    pol_data <- get_data()
    
    # Filter by date
    pol_data <- pol_data[(which(pol_data$in_date>=input$DateRange[1])),]
    
    return(pol_data)
  })
  
  # Display plot when data uploaded
  output$timeline <- renderPlot({
    
    # Return nothing if no data loaded
    if (is.null(input$Data))
      return(NULL)
    
    # Import filtered data
    data <- polished_data()

    # Main plot
    plot <- ggplot(data, aes(x=in_date, xend=out_date, y=patient, yend=patient, color=unit)) +
      geom_segment() +
      theme_bw()+ #use ggplot theme with black gridlines and white background
      geom_segment(size=8)+ #increase line width of segments in the chart
      theme(axis.title = element_blank())
    
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
    
    plot
    })
  
  output$table <- renderTable({
    
    if (is.null(input$Data))
      return(NULL)
    
    table <- polished_data()
    table$in_date <- as.character(as.POSIXct(table$in_date, format("%Y-%m-%d")))
    
    return(table)
      })
  
}

shinyApp(ui = ui, server = server)