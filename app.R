# Install packages if not present
list.of.packages <- c("ggplot2", "shiny", "dplyr", "forcats", "shinyWidgets",
                      "svglite", "shinydashboard", "plotly", "shinycssloaders")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required packages
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(forcats)
library(svglite)
library(shinydashboard)
library(plotly)
library(shinycssloaders)

##### Custom functions for EpiPlot #######
hosp_or_comm <- function(table, timelapse){
  # Define if infection is community or hospital acquired.
  # Add column in table and return the new table
  
  # Basically, we consider everything community acquired
  table <- cbind.data.frame(table, infection=rep("Community", nrow(table)))
  
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
          pat_table[line-1, "out_date"] <- pat_table[line-1, "out_date"] + 86400
        }
        pat_table[line-1, "sampling"] <- pat_table[line-1, "sampling"] + 43200
      }
      # Changing last row out_date
      pat_table[nrow(pat_table), "out_date"] <- pat_table[nrow(pat_table), "out_date"] + 86400
      pat_table[line, "sampling"] <- pat_table[line, "sampling"] + 43200
    }
    
    new_table <- rbind.data.frame(new_table, pat_table)
  }
  return(new_table)
}    

#### Settings parameters #######
# Loading animation
options(spinner.type = 7)

##### Shiny code for EpiPlot #######
# UI
ui <- dashboardPage(
  # Header
  dashboardHeader(title = "EpiPlot"),
  
  # Side bar menu
  dashboardSidebar(
    sidebarMenu(
      id = "SideBar",
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Gantt", tabName = "gantt", icon = icon("chart-gantt")),
      menuItem("Frequency", tabName = "frequency", icon = icon("chart-column"))

    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      ########## Table loading tab
      tabItem(tabName = "table",
              fluidRow(box(width = 12,
                           fluidRow(column(2, valueBoxOutput("BoxPatients", width=10)),
                                    column(2, valueBoxOutput("BoxCluster", width=10)),
                                    column(2, valueBoxOutput("BoxUnits", width=10))))),
              box(width = 2,
                  p("Load table", style="font-size:25px"),
                  fileInput("Data", NULL, accept=c("text/csv", "text/txt")),
                  hr(),
                  p("Parameters", style="font-size:25px"),
                  sliderInput("incubation", label = "Incubation period (days)", min = 0, 
                              max = 5, value = 4, step = 1),
                  dateRangeInput("DateRange", "Dates range"),
                  pickerInput("patientPicker", "Select patients", choices = "",
                              multiple = TRUE, options = list(`actions-box` = TRUE, size = 10)),
                  pickerInput("unitPicker", "Select units", choices = "",
                              multiple = TRUE, options = list(`actions-box` = TRUE, size = 10)),
                  pickerInput("clusterPicker", "Select clusters", choices = "",
                              multiple = TRUE, options = list(`actions-box` = TRUE, size = 10)),
                  pickerInput("infectionPicker", "Select infection", choices = list("Hospital", "Community"),
                              multiple = TRUE, options = list(`actions-box` = TRUE), selected = list("Hospital", "Community"))
                  ),
              box(width = 10, dataTableOutput("table"))
              ),
      
      ######## Gantt tab
      tabItem(tabName = "gantt",
              
              # Box with plot
              box(width = 12,
              "Click and drag on area to zoom in. Double click to zoom out",
              withSpinner(plotlyOutput("timeline"))),
              
              # Box 1 with filtering parameters
              box(width = 3,  height = 400,
                  p("Parameters", style="font-size:25px"),
                  selectInput("plotOrder", "Samples order", list("Patients" = "patients",
                                                                 "Admission" = "in_date",
                                                                 "First MRSA" = "sampling",
                                                                 "NGS cluster" = "cluster")),
                  selectInput("plotColor", "Information", choices = list("Unit" = "unit",
                                                                         "Infection" = "infection",
                                                                         "Cluster" = "cluster")),
                  checkboxInput("checkCluster", label = "Display clusters", value = FALSE),
                  checkboxInput("checkClusterLabel", label = "Cluster label", value = FALSE)
                  ),
              
              # Box 2 with controls of graphical parameters
              box(width = 3,  height = 400,
                  p("Cluster options", style="font-size:25px"),
                  fluidRow(column(6, numericInput("DotSize", label = "Sampling size", value = 4),
                                  numericInput("segmentSize", label = "Segment size", value = 4)),
                           column(6, numericInput("clustLabelText", label = "Text size", value = 3, step = 0.5),
                                  numericInput("clustLabelNudge", label = "Nudge", value = 0.5, step = 0.1)))
                  ),
              
              # Box for download
              box(width = 3,  height = 400,
                  p("Export plot", style="font-size:25px"),
                  fluidRow(column(6,
                                  numericInput("ganttHeight", "Height", value = 2048, min = 1024, step = 1024),
                                  radioButtons("ganttTypeOut", NULL, choiceNames = c("PNG", "PDF", "SVG"),
                                               choiceValues = c("png", "pdf", "svg"), inline = TRUE)),
                           column(6, numericInput("ganttWidth", "Width", value = 2048, min = 1024, step = 1024),
                                  downloadButton("ganttDlButton", "Download"))
                  )
              )
      ),
      
      ########### Frequency tab
      tabItem(tabName = "frequency",
              
              # Box with frequency plot
              box(width = 12,
                "Click and drag on area to zoom in. Double click to zoom out",
                withSpinner(plotlyOutput("frequency"))),
              
              # Box with controls
              box(width = 3, height = 300,
                  p("Bins control", style="font-size:25px"),
                  numericInput("freqBins", label = "Number", value = 30, step = 1)),
              
              # Box for download
              box(width = 3, height = 300,
                  p("Export plot", style="font-size:25px"),
                  fluidRow(column(6,
                                  numericInput("freqHeight", "Height", value = 2048, min = 1024, step = 1024),
                                  radioButtons("freqTypeOut", NULL, choiceNames = c("PNG", "PDF", "SVG"),
                                               choiceValues = c("png", "pdf", "svg"), inline = TRUE)),
                           column(6, numericInput("freqWidth", "Width", value = 2048, min = 1024, step = 1024),
                                  downloadButton("freqDlButton", "Download"))
                  )
              )
              )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load table
  raw_data <- reactive({
    
    # Load the table
    table <- read.csv(input$Data$datapath)
    
    # Convert into time
    table <- table %>% mutate(in_date = as.POSIXct(in_date), 
                              out_date = as.POSIXct(out_date), 
                              sampling = as.POSIXct(sampling,))

    # Update date range widget
    updateDateRangeInput(session, "DateRange", start=min(table$in_date), 
                         end=max(table$out_date))
    
    # Update information selectInput with additional categories
    categories <- list("Unit" = "unit",
         "Infection" = "infection",
         "Cluster" = "cluster")
    if (ncol(table) > 6){
      add_cat <- colnames(table)[7:ncol(table)]
      for (add in add_cat){
        categories[[add]] = add
      }
      updateSelectInput(session, "plotColor", choices = categories)
    }
   
    
    # Update patient picker widget
    updatePickerInput(session, "patientPicker", choices = levels(factor(table$patient)),
                      selected = levels(factor(table$patient)))
    
    # Update unit picker widget
    updatePickerInput(session, "unitPicker", choices = levels(factor(table$unit)),
                      selected = levels(factor(table$unit)))
    
    # Update cluster picker widget
    updatePickerInput(session, "clusterPicker", choices = levels(factor(table$cluster)),
                      selected = levels(factor(table$cluster)))
    
    return(table)
  })


  # Filtering data
  filtered_data <- reactive({
    filt_data <- raw_data()
    
    # Filter by date
    filt_data <- filt_data[(which(filt_data$in_date>=as.POSIXct(input$DateRange[1]))),]
    filt_data <- filt_data[(which(filt_data$out_date<=as.POSIXct(input$DateRange[2]))),]
    
    # Filter by patient, unit and cluster
    filt_data <- filt_data[which(filt_data$patient %in% input$patientPicker),]
    filt_data <- filt_data[which(filt_data$unit %in% input$unitPicker),]
    filt_data <- filt_data[which(filt_data$cluster %in% input$clusterPicker),]

    # Add community/hospital column and filter this data
    filt_data <- hosp_or_comm(filt_data, input$incubation)
    filt_data <- filt_data[which(filt_data$infection %in% input$infectionPicker),]
    
    # Changing samples order for plot
    order_var <- input$plotOrder
    if (order_var == "patients"){
      filt_data$patient <- factor(filt_data$patient, levels = unique(sort(filt_data$patient)))
    } else if (order_var == "in_date") {
      filt_data$patient <- factor(filt_data$patient, levels = unique(filt_data$patient[order(filt_data$in_date)]))
    } else if (order_var == "sampling"){
      filt_data$patient <- factor(filt_data$patient, levels = unique(filt_data$patient[order(filt_data$sampling)]))
    } else if (order_var == "cluster"){
      filt_data$patient <- factor(filt_data$patient, levels = unique(filt_data$patient[order(filt_data$cluster)]))
    }

    # Update information boxes
    output$BoxPatients <- renderValueBox({
      valueBox(length(levels(factor(filt_data$patient))), "Patients",
        icon = icon("head-side-mask", class="sharp", lib = "font-awesome"),
        color = "yellow")})
    
    output$BoxCluster <- renderValueBox({
      valueBox(length(levels(factor(filt_data$cluster))),
        "Cluster", icon = icon("circle-nodes", class="sharp", lib = "font-awesome"),
        color = "blue")})
    
    output$BoxUnits <- renderValueBox({
      valueBox(length(levels(factor(filt_data$unit))),
        "Units", icon = icon("hospital", class="sharp", lib = "font-awesome"),
        color = "red")})
    
    return(filt_data)
  })
  
  # Gantt plot
  draw_gantt <- reactive({
    
    # Return nothing if no data loaded
    if (is.null(input$Data))
      return(NULL)
    
    # Import filtered data
    plot_data <- filtered_data()
    
    # Add time on date to have period of time for one-day visits
    # Add in seconds (86399 = 23h59m59s, 43200=12h00m00s)
    plot_data <- add_time(plot_data)
    
    # Define best segment and dot size depending on patient number (with minimums)
    text_size <- max(10, 25-length(levels(as.factor(plot_data$patient))))
    
    # Define colors group
    legend <- plot_data[,input$plotColor]
    
    # Main plot
    plot <- ggplot(plot_data, aes(x=in_date, xend=out_date, y=patient, yend=patient, color=legend)) +
      geom_segment(linewidth=input$segmentSize) +
      theme_bw()+ #use ggplot theme with black gridlines and white background
      theme(axis.title = element_blank(), 
            legend.position = "bottom",
            legend.key.size = unit(1.5, 'cm'),
            legend.text = element_text(size=15),
            legend.title = element_blank(),
            axis.text = element_text(size=text_size))
    
    if (input$checkCluster == TRUE){
      sub_plot_data <- plot_data[!duplicated(plot_data$patient),]
      
      # Add sampling date
      plot <- plot + geom_point(data=sub_plot_data,
                                aes(x=sampling, y=patient), color="black", size=input$DotSize)

      plot <- plot + geom_line(data=sub_plot_data,
                               aes(x=sampling, y=patient, group=cluster), color="black")

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
    plot_data <- filtered_data()

    # Keep only selected patients
    plot_data <- plot_data[plot_data$patient %in% input$patientPicker,]
    
    # Transform data for plotting
    plot_data2 <- data.frame()
    
    for (line in 1:nrow(plot_data)){
      days <- seq(plot_data[line,"in_date"], plot_data[line,"out_date"], by="days")
      subtable <- data.frame(day=days, 
                             var=rep(plot_data[line, input$plotColor], length(days)) # Add selected variable in the plot
      )
      plot_data2 <- rbind.data.frame(plot_data2, subtable)
    }

    # Define text size and color
    text_size <- max(10, 25-length(levels(as.factor(plot_data$patient))))
    color <- plot_data2[,"var"]
    
    # Main plot
    plot <- ggplot(plot_data2, aes(x=day, fill=color)) + geom_histogram(bins=input$freqBins)
    plot <- plot + theme_bw() + theme(axis.title = element_blank(), 
                                      axis.text = element_text(size=text_size),
                                      legend.position = "bottom",
                                      legend.key.size = unit(1.5, 'cm'),
                                      legend.text = element_text(size=15),
                                      legend.title = element_blank())
    
    return(plot)
  })
  
  # Display Gantt plot and frequency plot
  output$timeline <- renderPlotly({draw_gantt()})
  output$frequency <- renderPlotly({draw_frequency()})
  
  # Display table
  output$table <- renderDataTable({
    if (is.null(input$Data))
      return(NULL)
    
    table <- filtered_data()
    
    table <- table %>% mutate(in_date = as.character(as.POSIXct(in_date)), 
                              out_date = as.character(as.POSIXct(out_date)), 
                              sampling= as.character(as.POSIXct(sampling)))
    },
    options = list(
      pageLength = 15,
      lengthChange = FALSE,
      searching = FALSE
    ))
  
  # Saving button Gantt plot
  output$ganttDlButton <- downloadHandler(filename = function(){paste("timeline", input$ganttTypeOut, sep = ".")},
                                       content = function(file){
                                         ggsave(file, draw_gantt(), device = input$ganttTypeOut,
                                                width = as.numeric(input$ganttWidth),
                                                height = as.numeric(input$ganttHeight),
                                                units = "px")
                                         })
  
  # Saving button frequency plot
  output$freqDlButton <- downloadHandler(filename = function(){paste("timeline", input$freqTypeOut, sep = ".")},
                                          content = function(file){
                                            ggsave(file, draw_frequency(), device = input$freqTypeOut,
                                                   width = as.numeric(input$freqWidth),
                                                   height = as.numeric(input$freqHeight),
                                                   units = "px")
                                          })
  
}

shinyApp(ui = ui, server = server)