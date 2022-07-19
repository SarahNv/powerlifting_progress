library(googlesheets4)

# Set authentication token to be stored in a folder called `.secrets`
#options(gargle_oauth_cache = ".secrets")

# Authenticate manually
#gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:
#list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
#gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
#gs4_auth(cache = ".secrets", email = "snarvaiz@vols.utk.edu")

library(shiny)
library(googlesheets4)
library(lubridate)
library(tidyverse)



#read in data
df <- read_sheet("https://docs.google.com/spreadsheets/d/18ItR7lTILKxZ6VA4TRZvwByrr8xwBtjhzu692F733cc/edit?usp=sharing")
df <- df %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

str(df)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sarah's Training Progress (BETA)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("lift_var",label = 'Select lift',
                     choices = c("",unique(df$lift)),
                     selected = NULL, multiple=F),
      selectizeInput("sets_var", "Select sets",choices = NULL),
      selectizeInput("reps_var", "Select reps", choices = NULL),
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("toptitle"), align = "left"),
      plotlyOutput("plot"),
      uiOutput("stats"))
    )
  )
)



# Define server logic to plot progress
server <- function(input, output, session) {
  
  # Title main area
  # ----------------------------------
  output$toptitle <- renderText({
    paste("Lifting progress for ", input$lift_var)
  })
  
  # Reactive elements
  lift_sel <- reactive({
    df %>% filter(lift == input$lift_var)
  })
  
  observeEvent(lift_sel(),{
    updateSelectizeInput(session, "sets_var", choices = sort(lift_sel()$sets))
    
    sets_sel <- reactive({lift_sel() %>% filter(sets == input$sets_var)
    })
    
    observeEvent(sets_sel(),{
      updateSelectizeInput(session, "reps_var", choices = sort(sets_sel()$reps))
      
      output$plot <- renderPlotly({
        df %>% 
          filter(lift == input$lift_var,
                 sets == input$sets_var,
                 reps == input$reps_var) %>%
          plot_ly(x= ~ date, y = ~ lbs,
          mode = "lines+markers")
      })
      
    })
    
    output$stats <- renderUI({
      weight_max <- df %>%
        filter(lift == input$lift_var,
               sets == input$sets_var,
               reps == input$reps_var)
      
      weight_max <- max(weight_max$lbs)
      
      weight_ave <- df %>%
        filter(lift == input$lift_var,
               sets == input$sets_var,
               reps == input$reps_var)
      weight_ave <- round(mean(weight_ave$lbs), 2)
      
      tags$div(paste("The max", input$lift_var, "weight lifted for", input$sets_var, "x", 
                     input$reps_var, "is", weight_max, "lbs. The average weight lifted is", weight_ave, "lbs."))
      
    })
  })
  
}





# Run the application 
shinyApp(ui = ui, server = server)

