
library(shiny)
library(googlesheets4)
library(lubridate)
library(tidyverse)


#read in data
df <- read_sheet("https://docs.google.com/spreadsheets/d/18ItR7lTILKxZ6VA4TRZvwByrr8xwBtjhzu692F733cc/edit?usp=sharing")
df <- df %>%
  mutate(date = ymd(date))
str(df)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sarah's Training Progress (BETA)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(fluid=TRUE,
        sidebarPanel(
            selectInput("lift_var",label = 'Select lift', 
                        c("squat","bench","deadlift"),
                        selected = "Squat",
                        multiple = FALSE),
            
            selectInput("sets_var", label="Select sets",
                        unique(df$sets), 
                        selected = "1",
                        multiple= FALSE),
            
            selectInput("reps_var", label="Select reps",
                        unique(df$reps), 
                        selected = "1",
                        multiple= FALSE),
        ),
        
                      
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("plot")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Title main area
  # ----------------------------------
  output$toptitle <- renderText({
    paste("Lifting progress for ", input$lift_var)
  })
  
  # ----------------------------------
  # Reactive elements
  display_dataset <- reactive({
    df %>% filter(lift %in% input$lift_var & sets %in% input$sets_var & reps %in% input$reps_var)
  })
  
  output$table <- renderTable({
      display_dataset()
    })
  
  # Plot outputs
  output$plot <- renderPlot({  
    display_dataset() %>%  ggplot(aes(x=date, y = lbs)) +
      geom_line(color = "palevioletred3") + 
      theme_minimal() + 
      scale_x_date(date_labels = "%b-%d-%y", date_breaks = "2 week") + 
      theme(axis.text.x=element_text(angle=60, hjust=1)) 
  })
}





# Run the application 
shinyApp(ui = ui, server = server)
