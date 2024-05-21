library(shiny)
library(plotly)
library(dplyr)

# UI -----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("COVID-19 Counts by Age Group"),
  sidebarLayout(
    sidebarPanel(
      selectInput("district", "Select District:",
                  choices = c("Alexandria", "Arlington", "Fairfax", "Loudoun", "Prince William"))
    ),
    mainPanel(
      plotlyOutput("plot"),
      br(),
      tableOutput("table")
    )
  )
)

# Server -------------------------------------------------------------------
server <- function(input, output, session) {
  output$plot <- renderPlotly({
    plot <- switch(input$district,
                   "Alexandria" = alexandria_plot,
                   "Arlington" = arlington_plot,
                   "Fairfax" = fairfax_plot,
                   "Loudoun" = loudoun_plot,
                   "Prince William" = pw_plot)
    plot
  })
  
  output$table <- renderTable({
    combined_data %>%
      filter(district == input$district) %>%
      group_by(variable) %>%
      summarize(total = sum(count))
    
    
  })
}

# Shiny App ----------------------------------------------------------------
shinyApp(ui, server)
