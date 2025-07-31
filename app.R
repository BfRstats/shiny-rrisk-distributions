library(shiny)
#library(DT)
#library(jsonlite)

source("rrisk_distributions/rriskDistributionClass.R", chdir = TRUE)

source("myShinyAlert/myShinyAlert.R", chdir = TRUE)

source("utility_functions/get_shiny_R6_class.R")
source("utility_functions/csv_to_vec.R")
source("utility_functions/build_action_buttons.R")
source("utility_functions/parse_action_buttons.R")
source("utility_functions/read_and_interpretate_table.R")

source("shiny-rrisk-distributions_modules/handling_menu_bar/menu_bar_ui.R")
source("shiny-rrisk-distributions_modules/handling_menu_bar/menu_bar_server.R")
source("shiny-rrisk-distributions_modules/handling_menu_bar/menu_bar_utils.R")

source("shiny-rrisk-distributions_modules/handling_input_data/input_data_ui.R")
source("shiny-rrisk-distributions_modules/handling_input_data/input_data_server.R")

source("shiny-rrisk-distributions_modules/handling_model_fit/model_fit_ui.R")
source("shiny-rrisk-distributions_modules/handling_model_fit/model_fit_server.R")

source("shiny-rrisk-distributions_modules/handling_documentation/documentation_ui.R")
source("shiny-rrisk-distributions_modules/handling_documentation/documentation_server.R")
source("shiny-rrisk-distributions_modules/handling_documentation/documentation_utils.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  #titlePanel("shiny rrisk distributions"),
  
  menu_bar_ui(version = "alpha v0.7.3"),
  
  htmltools::br(), 
  htmltools::br(), 
  htmltools::br(),
  tabsetPanel(
    id = "main_tab",
    tabPanel( # MODEL CREATION
      title = htmltools::HTML("Modelling"),
      value = "p1",
      htmltools::br(),
      
      input_data_ui(),
      
      htmltools::hr(),
      
      model_fit_ui()
    ),
    tabPanel( # Description
      title = htmltools::HTML("Description"),
      value = "p2",
      
      documentation_ui()
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # instantiated main object
  rrisk_dist_obj <- get_shiny_R6_class(
    class_name   = "shinyRriskDistributionClass", 
    parent_class = rriskDistributionsClass)$new()$reactive()
  
  # menu bar functionality
  menu_bar_server(input, output, session, rrisk_dist_obj)
  
  # input data
  input_data_server(input, output, rrisk_dist_obj)
  
  # presenting fit results
  model_fit_server(input, output, rrisk_dist_obj)

  # handling author table
  documentation_server(input, output, rrisk_dist_obj)

}

# Run the application 
shinyApp(ui = ui, server = server)
