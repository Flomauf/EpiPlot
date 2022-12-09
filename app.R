library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)

hosp_or_comm <- function(table, timelapse){
  # Define if infection is community or hospital acquired.
  # Add column in table and return the new table
  table <- cbind.data.frame(table, infection=rep("", nrow(table)))
  
  # Analyze for each patient
  for (p in levels(factor(table$patient))){
    
    pat_table <- table[table$patient==p,]
    sampling_date <- pat_table$sampling[1]
    visits <- list()
    
    for (line in 1:nrow(pat_table)){
      if (length(visits) >= 1){
        if (pat_table$in_date[line] == visits[[length(visits)]][[2]]){
          # If entrance date is the same as exit date of the last element, replace this element
          visits[[length(visits)]][[2]] <- pat_table$out_date[line]
        } else {
          # If not equal, create a new list for the next visit
          visits[[length(visits)+1]] <- list(pat_table$in_date[line], pat_table$out_date[line])
        }
      } else {
        # First visit
        visits[[1]] <- list(pat_table$in_date[1], pat_table$out_date[1])
      }
    }
    
    for (visit in visits){
      # Consider only the period with the sampling
      in_date_epoch <- as.numeric(as.POSIXct(visit[[1]]))
      out_date_epoch <- as.numeric(as.POSIXct(visit[[2]]))
      sample_date_epoch <- as.numeric(as.POSIXct(sampling_date))
      if (in_date_epoch<=sample_date_epoch & sample_date_epoch<=out_date_epoch){
        if (sample_date_epoch-in_date_epoch >= timelapse*86400){
          # Considered as hospital acquired
          table[table$patient==p, "infection"] <- "Hospital"
        } else {
          # Considered as community acquired
          table[table$patient==p, "infection"] <- "Community"
        }
      }
    }
  }
  
  return(table)
}


# Define UI ----
ui <- fluidPage(
  titlePanel("EpiPlot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("Data", "Data file", accept=c("text/csv", "text/txt")),
      sliderInput("incubation", label = "Incubation period (days)", min = 0, 
                  max = 5, value = 4, step = 1),
      
      h3("Filters"),
      dateRangeInput("DateRange", "Dates range"),
      selectInput("plotOrder", "Samples order", list("Patients" = "patients",
                                                     "Admission" = "in_date",
                                                     "First MRSA" = "sampling",
                                                     "NGS cluster" = "cluster")),
      selectInput("plotColor", "Color", list("Unit" = "unit",
                                             "Infection" = "infection")),
      hr(),
      
      h3("Strains clusters"),
      checkboxInput("checkCluster", label = "Display", value = FALSE),
      numericInput("DotSize", label = "Dot", value = 1),
      numericInput("SegSize", label = "Segment", value = 1),
      textInput("ClustColor", label = "Color", value = "black"),
      hr(),
      
      h3("Output"),
      textInput("saveName", "Filename"),
      textInput("saveHeight", "Height", value = 1024),
      textInput("saveWidth", "Width", value = 1024),
      actionButton("saveButton", "Save")
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
    table <- table %>% mutate(in_date = as.POSIXct(in_date), 
                              out_date = as.POSIXct(out_date), 
                              sampling = as.POSIXct(sampling,))

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
    
    # Add community/hospital column
    pol_data <- hosp_or_comm(pol_data, input$incubation)
    
    # Changing samples order for plot
    order_var <- input$plotOrder
    if (order_var == "patients"){
      pol_data$patient <- factor(pol_data$patient, levels = unique(sort(pol_data$patient)))
    } else if (order_var == "in_date") {
      pol_data$patient <- factor(pol_data$patient, levels = unique(pol_data$patient[order(pol_data$in_date)]))
    } else if (order_var == "sampling"){
      pol_data$patient <- factor(pol_data$patient, levels = unique(pol_data$patient[order(pol_data$sampling)]))
    } else if (order_var == "cluster"){
      pol_data$patient <- factor(pol_data$patient, levels = unique(data$patient[order(data$cluster)]))
    }
    
    return(pol_data)
  })
  
  # Display plot when data uploaded
  output$timeline <- renderPlot({
    
    # Return nothing if no data loaded
    if (is.null(input$Data))
      return(NULL)
    
    # Import filtered data
    plot_data <- polished_data()
    
    # Add time on date to have period of time for one-day visits
    # Add in seconds (86399 = 23h59m59s, 43200=12h00m00s)
    plot_data <- plot_data %>% mutate(out_date = out_date + 86399, 
                              sampling = sampling + 43200)
    
    
    # Define best segment and dot size depending on patient number (with minimums)
    line_size <- max(3, 25-length(levels(as.factor(plot_data$patient))))
    text_size <- max(10, 25-length(levels(as.factor(plot_data$patient))))
    
    # Define colors group
    color <- plot_data[,input$plotColor]
    
    # Main plot
    plot <- ggplot(plot_data, aes(x=in_date, xend=out_date, y=patient, yend=patient, color=color)) +
      geom_segment(linewidth=line_size) +
      theme_bw()+ #use ggplot theme with black gridlines and white background
      theme(axis.title = element_blank(), legend.position = "bottom",
            legend.key.size = unit(1.5, 'cm'),
            legend.text = element_text(size=15),
            legend.title = element_blank(),
            axis.text = element_text(size=text_size))
    
    if (input$checkCluster == TRUE){
      # Add cluster segments
      for (level in levels(factor(plot_data$cluster))){
        df <- plot_data[which(plot_data$cluster==level),]
        df <- df[!duplicated(df$patient),]
        df <- df[order(df$sampling),]
        if (nrow(df) > 1){
          for (line in 1:(nrow(df)-1)){
            plot <- plot + geom_segment(x=df[line,"sampling"], xend=df[line+1,"sampling"], 
                                        y=df[line,"patient"], yend=df[line+1,"patient"],
                                        color=input$ClustColor, linewidth=input$SegSize)
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
  
  observeEvent(input$saveButton, {
    ggsave("test.png",
           width = as.numeric(input$saveWidth),
           height = as.numeric(input$saveHeight),
           units="px")
  })
  
}

shinyApp(ui = ui, server = server)