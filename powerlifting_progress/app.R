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
library(plotly)

#gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
#gs4_auth(cache = ".secrets", email = "snarvaiz@vols.utk.edu")
gs4_deauth()



#read in data
df <- read_sheet("https://docs.google.com/spreadsheets/d/18ItR7lTILKxZ6VA4TRZvwByrr8xwBtjhzu692F733cc/edit?usp=sharing")
df <- df %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

#str(df)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  h1(id = "app-heading", "Sarah's Powerlifting Progress"),
  tags$style(HTML("#app-heading{color: 	#9999ff}")),
  
  # Sidebar with user input
  fluidRow(
    column(width = 3,
           selectizeInput("lift_var",label = 'Select lift',
                     choices = c("",unique(df$lift)),
                     selected = NULL, multiple=F),
      selectizeInput("sets_var", "Select sets",choices = NULL),
      selectizeInput("reps_var", "Select reps", choices = NULL),
      h4("See my lifts on Instagram!",
         style = "color: 	#eba9a9; text-align: left; padding: 25px"),
      tags$iframe(
        src = "//www.instagram.com/p/CdEHJ2XLay3/embed",
        height =350, width = 250, style="text-align:center")
    ),
    column(width = 8,
           h3(textOutput("toptitle"), style = "color: 	#eba9a9; text-align: center", align = "center"),
           plotlyOutput("plot"),
           uiOutput("stats", style = "padding: 75px"), style = "display: block; margin-left: auto; margin-right: auto; padding: 25px")
    ),
  
  
  tags$footer(
    "Borrow Shiny app code from ",
    tags$a(
      "Github",
      target = "_blank",
      href = "https://github.com/SarahNv/powerlifting_progress"
    ),
    style = "position: absolyte; bottom:0; width:95%; height: 50px; padding: 50px; color: black; text-align: center;"
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
          mode = "lines+markers", text = ~paste("RPE:", rpe), line = list(color = "rgb(235, 169, 169)"),
          marker = list(color='rgb(153, 153, 255)', size = 10)) %>%
          layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
          config(displayModeBar = F)
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
      
      tags$div(HTML(paste("The <b>max</b>", input$lift_var, "weight lifted for", input$sets_var, "x", 
                     input$reps_var, "is", "<b>",weight_max,"lbs</b>. The <b>average</b> weight lifted is", "<b>",weight_ave, "lbs</b>.")))
      
    })
  })
  
}





# Run the application 
shinyApp(ui = ui, server = server)

