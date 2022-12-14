# Load or install packages
list.of.packages <- c("ggplot2", "shiny", "dplyr", "forcats", "shinyWidgets",
                      "svglite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(forcats)
library(svglite)

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

add_time <- function(table){
  # Add time depending on previous line (if some date overlay)
  new_table <- data.frame()
  for (patient in levels(factor(table$patient))){
    pat_table <- table[which(table$patient==patient),]
    
    # If only one date
    if (nrow(pat_table) == 1){
      pat_table$out_date <- pat_table$out_date + 86399
      pat_table$sampling <- pat_table$sampling + 43200
    
    # For other patients tables  
    } else {
      for (line in 2:nrow(pat_table)){
        if (pat_table[line, "in_date"] == pat_table[line-1, "out_date"]){
          pat_table[line, "in_date"] <- pat_table[line, "in_date"] + 43200
          pat_table[line-1, "out_date"] <- pat_table[line-1, "out_date"] + 43200
        } else {
          pat_table[line-1, "out_date"] <- pat_table[line-1, "out_date"] + 86399
        }
        pat_table[line-1, "sampling"] <- pat_table[line-1, "sampling"] + 43200
      }
      # Changing last row out_date
      pat_table[nrow(pat_table), "out_date"] <- pat_table[nrow(pat_table), "out_date"] + 86399
      pat_table[line, "sampling"] <- pat_table[line, "sampling"] + 43200
    }
    
    new_table <- rbind.data.frame(new_table, pat_table)
  }
  return(new_table)
}    

# Define UI ----
ui <- fluidPage(
  titlePanel("EpiPlot"),
  tabsetPanel(
    tabPanel("Table", fluid = TRUE,
       sidebarLayout(
         sidebarPanel(
           style = "height: 90vh; overflow-y: auto;", # Scroll bar
           h3("Data"),
           fileInput("Data", NULL, accept=c("text/csv", "text/txt")),
           sliderInput("incubation", label = "Incubation period (days)", min = 0, 
                       max = 5, value = 4, step = 1),
           dateRangeInput("DateRange", "Dates range")),
        mainPanel(
          tableOutput("table"))
      
      )
    ),
    tabPanel("Plot", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 style = "height: 90vh; overflow-y: auto;", # Scroll bar
                 h3("Filters"),
                 selectInput("plotOrder", "Samples order", list("Patients" = "patients",
                                                                "Admission" = "in_date",
                                                                "First MRSA" = "sampling",
                                                                "NGS cluster" = "cluster")),
                 selectInput("plotColor", "Color", list("Unit" = "unit",
                                                        "Infection" = "infection")),
                 pickerInput("patientPicker", "Select patients", choices = "",
                             multiple = TRUE, options = list(`actions-box` = TRUE)),
                 hr(),
                 
                 h3("Strains clusters"),
                 checkboxInput("checkCluster", label = "Display", value = FALSE),
                 numericInput("DotSize", label = "Dot", value = 4),
                 numericInput("SegSize", label = "Segment", value = 1),
                 checkboxInput("checkClusterLabel", label = "Cluster label", value = FALSE),
                 numericInput("clustLabelText", label = "Text size", value = 3, step = 0.5),
                 numericInput("clustLabelNudge", label = "Nudge", value = 0.5, step = 0.1),
                 hr(),
                 
                 h3("Frequency table"),
                 numericInput("freqBins", label = "Bins", value = 30, step = 1),
                 hr(),
                 
                 h3("Output"),
                 sliderInput("saveHeight", "Height", value = 2048, min = 1024, 
                             max = 4096, step = 1024),
                 sliderInput("saveWidth", "Width", value = 2048, min = 1024, 
                             max = 4096, step = 1024),
                 radioButtons("typeOut", NULL, choiceNames = c("PNG", "PDF", "SVG"),
                              choiceValues = c("png", "pdf", "svg"), inline = TRUE),
                 downloadButton("dlButton", "Download")
               ),
            mainPanel(
              plotOutput("timeline", brush = brushOpts(id = "plot_brush", resetOnNew = TRUE), dblclick = "db_click"),
              plotOutput("frequency")
        )
      )
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
    updateDateRangeInput(session, "DateRange", start=min(table$in_date), 
                         end=max(table$out_date))

    return(table)
  })


  # Filter dates by brushing the plot
  filtered_data <- reactive({
    fil_data <- get_data()
    brush_data <- input$plot_brush
    min_date <- brush_data$xmin
    max_date <- brush_data$xmax
    db_click <- input$db_click
    
    if (!is.null(min_date)){
      fil_data <- fil_data[(which(fil_data$in_date>=min_date)),]
      fil_data <- fil_data[(which(fil_data$out_date<=max_date)),]
      updateDateRangeInput(session, "DateRange", start=as.POSIXct(min_date, origin="1970-01-01"), 
                           end=as.POSIXct(max_date, origin="1970-01-01"))
    }
    
    if (!is.null(db_click)){
      updateDateRangeInput(session, "DateRange", start=min(fil_data$in_date), 
                           end=max(fil_data$out_date))
    }
    
    # Filter by date
    fil_data <- fil_data[(which(fil_data$in_date>=as.POSIXct(input$DateRange[1]))),]
    fil_data <- fil_data[(which(fil_data$out_date<=as.POSIXct(input$DateRange[2]))),]
    
    return(fil_data)
    })
  
  # Filtering data
  polished_data <- reactive({
    pol_data <- filtered_data()
    
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
      pol_data$patient <- factor(pol_data$patient, levels = unique(pol_data$patient[order(pol_data$cluster)]))
    }
    
    updatePickerInput(session, "patientPicker", choices = levels(factor(pol_data$patient)),
                      selected = levels(factor(pol_data$patient)))
    
    return(pol_data)
  })
  
  # Gantt plot
  draw_gantt <- reactive({
    
    # Return nothing if no data loaded
    if (is.null(input$Data))
      return(NULL)
    
    # Import filtered data
    plot_data <- polished_data()
    
    # Add time on date to have period of time for one-day visits
    # Add in seconds (86399 = 23h59m59s, 43200=12h00m00s)
    plot_data <- add_time(plot_data)
    
    # Keep only selected patients
    plot_data <- plot_data[plot_data$patient %in% input$patientPicker,]
    
    # Define best segment and dot size depending on patient number (with minimums)
    line_size <- max(3, 20-length(levels(as.factor(plot_data$patient))))
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
                                        color="black", linewidth=input$SegSize)
          }
        }
      }
      # Add sampling date
      plot <- plot + geom_point(aes(x=sampling, y=patient), colour="black", size=input$DotSize)

      # Add cluster label
      if (input$checkClusterLabel == TRUE){
        plot <- plot + geom_text(aes(x=sampling, y=patient), 
                                  label=plot_data$cluster,
                                  color="black",
                                  nudge_y = input$clustLabelNudge, 
                                  size=input$clustLabelText)
      }
    }
    
    return(plot)
  })
  
  # Frequency plot
  draw_frequency <- reactive({
    
    # Return nothing if no data loaded
    if (is.null(input$Data))
      return(NULL)
    
    # Import filtered data
    plot_data <- polished_data()

    # Keep only selected patients
    plot_data <- plot_data[plot_data$patient %in% input$patientPicker,]
    
    # Transform data for plotting
    plot_data2 <- data.frame()
    
    for (line in 1:nrow(plot_data)){
      days <- seq(plot_data[line,"in_date"], plot_data[line,"out_date"], by="days")
      subtable <- data.frame(day=days, 
                             unit=rep(plot_data[line,"unit"], length(days)),
                             cluster=rep(plot_data[line,"cluster"], length(days)),
                             infection=rep(plot_data[line,"infection"], length(days))
      )
      plot_data2 <- rbind.data.frame(plot_data2, subtable)
    }

    # Define text size and color
    text_size <- max(10, 25-length(levels(as.factor(plot_data$patient))))
    color <- plot_data2[,input$plotColor]
    
    # Main plot
    plot <- ggplot(plot_data2, aes(x=day, fill=color)) + geom_histogram(bins=input$freqBins, color="black")
    plot <- plot + theme_bw() + theme(axis.title = element_blank(), 
                                      legend.position = "none",
                                      axis.text = element_text(size=text_size))
    
    return(plot)
  })
  
  # Display Gantt plot and frequency plot
  output$timeline <- renderPlot({draw_gantt()})
  output$frequency <- renderPlot({draw_frequency()})
  
  # Display table
  output$table <- renderTable({
    
    if (is.null(input$Data))
      return(NULL)
    
    table <- polished_data()
    table <- table %>% mutate(in_date = as.character(as.POSIXct(in_date)), 
                              out_date = as.character(as.POSIXct(out_date)), 
                              sampling= as.character(as.POSIXct(sampling)))
    return(table)
      })
  
  # Saving button Gantt plot
  output$dlButton <- downloadHandler(filename = function(){paste("timeline", input$typeOut, sep = ".")},
                                       content = function(file){
                                         ggsave(file, draw_gantt(), device = input$typeOut,
                                                width = as.numeric(input$saveWidth),
                                                height = as.numeric(input$saveHeight),
                                                units = "px")
                                         })
  
}

shinyApp(ui = ui, server = server)