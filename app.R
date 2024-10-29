# Install and load required packages
# install.packages(c("shiny", "dplyr", "ggplot2"))
library(shiny)
library(dplyr)
library(ggplot2)
library(rsconnect)


# Sample Data
# Replace this with your actual data
present_energy_data <- read.csv("Present_data.csv")
future_energy_data = read.csv("Future_data.csv")
present_energy_data$date = as.Date(present_energy_data$date)
future_energy_data$date = as.Date(future_energy_data$date)


# Define UI
ui <- fluidPage(
  titlePanel("Energy Usage Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_county", "Select County ID", 
                  choices = c("All Counties", unique(present_energy_data$in.county))),
      actionButton("plot_energy", "Plot Energy Usage Comparison")
    ),
    
    mainPanel(
      plotOutput("energy_plot"), # Single plot output for the combined plot
      h4("Highest Peak Energy"),
      textOutput("highest_peak_present"),
      textOutput("highest_peak_future")
    )
  ),
  
  # Add custom styles
  tags$head(
    tags$style(
      HTML(".main-panel { background-color: #F5F5F5; }"),
      HTML(".sidebar { background-color: #ADD8E6; color: #FFFFFF; }"),
      HTML(".action-button { background-color: #FF6600; color: #FFFFFF; border-color: #FF6600; }")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  present_energy_data <- read.csv("Present_data.csv")
  future_energy_data = read.csv("Future_data.csv")
  present_energy_data$date = as.Date(present_energy_data$date)
  future_energy_data$date = as.Date(future_energy_data$date)
  
  observeEvent(input$plot_energy, {
    selected_county <- input$selected_county
    
    # Filter data for the selected county from present and future datasets
    if (selected_county == "All Counties") {
      present_data_filtered <- present_energy_data %>%
        group_by(date) %>%
        summarise(total_energy = sum(total_energy)) %>%
        mutate(Year = "Present")
      
      future_data_filtered <- future_energy_data %>%
        group_by(date) %>%
        summarise(total_energy = sum(total_energy)) %>%
        mutate(Year = "Future")
      
    } else {
      present_data_filtered <- present_energy_data %>%
        filter(in.county == selected_county) %>%
        group_by(date) %>%
        summarise(total_energy = sum(total_energy)) %>%
        mutate(Year = "Present")
      
      future_data_filtered <- future_energy_data %>%
        filter(in.county == selected_county) %>%
        group_by(date) %>%
        summarise(total_energy = sum(total_energy)) %>%
        mutate(Year = "Future")
    }
    
    combined_data <- rbind(present_data_filtered, future_data_filtered)
    
    # Find the date and value of the peak energy usage for the present and future year
    peak_present <- present_data_filtered[which.max(present_data_filtered$total_energy), ]
    peak_future <- future_data_filtered[which.max(future_data_filtered$total_energy), ]
    
    # Plot combined energy usage
    output$energy_plot <- renderPlot({
      ggplot(combined_data, aes(x = date, y = total_energy, color = Year, group = Year)) +
        geom_line() +
        geom_point(size = 2) +
        scale_color_manual(values = c("Present" = "blue", "Future" = "red")) +
        theme_minimal() +
        theme(legend.position = "top",
              text = element_text(size = 12),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 12),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 14)) +
        labs(title = "Energy Consumption Comparison",
             subtitle = "Present Year vs Future Year",
             x = "Date",
             y = "Total Energy Consumption(kWh)",
             color = "Year")
    })
    
    # Display the date and value of the highest peak energy for the present year
    output$highest_peak_present <- renderText({
      paste("Present: ", round(peak_present$total_energy, 2), " kWh on", format(peak_present$date, "%d %b"))
    })
    
    # Display the date and value of the highest peak energy for the future year
    output$highest_peak_future <- renderText({
      paste("Future: ", round(peak_future$total_energy, 2), " kWh on", format(peak_future$date, "%d %b"))
    })
    
    # Remove the separate plot outputs for future energy usage
    output$future_energy_plot <- NULL
    output$highest_energy_future <- NULL
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
