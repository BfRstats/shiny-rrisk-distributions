input_data_ui <- function()
{
  tagList(
    radioButtons(
      inputId  = "input_data_type",
      label    = "type of data",
      choices  = c("pdf", "cdf"),
      selected = "pdf",
      inline   = TRUE
    ),
    
    span(textOutput(outputId = "info_text_for_input_data"), 
         style = "color:red"),
  )
}