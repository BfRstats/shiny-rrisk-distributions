model_fit_ui <- function()
{
  tagList(
    fluidRow(
      column(
        width = 2,
        selectInput(inputId = "select_dist",
                    label   = "Select distribution:",
                    choices = c(""))
      ),
      column(
        width = 2,
        selectInput(inputId = "select_statistic",
                    label   = "Sort with statistic:",
                    choices = list("avg. neg. log-like" = "loglike", 
                                   "aic risk"           = "aic", 
                                   "bic risk"           = "bic"))
      ),
    ),
    
    htmlOutput(outputId = "info_text_for_fit_result"),
    
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), 
                  plotOutput("pdf_plot"),
                  plotOutput("ecdf_plot"))
    )
  )
}