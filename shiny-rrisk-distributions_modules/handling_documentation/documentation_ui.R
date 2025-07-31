documentation_ui <- function()
{
  tagList(
    htmltools::br(),
    htmltools::h4("Author List"),
    htmltools::br(),
    DT::DTOutput(outputId = "author_table"),
    htmltools::br(),
    actionButton(inputId = "btn_add_author",
                 label   = "Add author"),
    htmltools::hr(),
    textInput(inputId = "unit_description",
              label   = "Unit",
              width   = "100%"),
    textAreaInput(inputId = "source_description",
                  label   = "Source",
                  width   = "100%"),
    textAreaInput(inputId = "general_description",
                  label   = "Description",
                  width   = "100%")
  )
}