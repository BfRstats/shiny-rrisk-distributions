input_data_server <- function(input, output, rrisk_dist_obj)
{
  #---BEGIN: handle manual input data-------------------------------------------
  observeEvent(
    eventExpr   = input$input_data_type,
    handlerExpr = {
      removeUI(
        selector = "div:has(> #input_data)"
      )
      removeUI(
        selector = "div:has(> #input_data_cdf)"
      )
      insertUI(
        selector = "#input_data_type",
        where    = "afterEnd",
        ui       = switch(
          EXPR = input$input_data_type,
          "pdf" = textAreaInput(
            inputId = "input_data",
            label   = "user provided data",
            value   = paste(rrisk_dist_obj()$get_data()$provided_data,
                            collapse = ", "),
            width   = "100%"),
          "cdf" = htmltools::tagList(
            textAreaInput(
              inputId = "input_data",
              label   = "user provided data",
              value   = paste(rrisk_dist_obj()$get_data()$provided_data,
                              collapse = ", "),
              width   = "100%"),
            textAreaInput(
              inputId = "input_data_cdf",
              label   = "user provided cdf data",
              value   = paste(rrisk_dist_obj()$get_data()$provided_data_cdf,
                              collapse = ", "),
              width   = "100%")
          )
        )
      )
    }
  )
  
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = list(input$input_data, input$input_data_cdf, 
                     input$input_data_type),
    handlerExpr = {
      user_data <- csv_to_vec(input$input_data, type = "numeric")
      user_data_cdf <- csv_to_vec(input$input_data_cdf, type = "numeric")
      result <- rrisk_dist_obj()$set_data(user_data,
                                          user_data_cdf,
                                          input_type = input$input_data_type)
      if (result$is_ok) {
        info_text <- ""
        rrisk_dist_obj()$fit_all_distributions()
        available_dists <- rrisk_dist_obj()$get_fitted_dist_name_list(sorted_by = input$select_statistic)
      } else {
        info_text <- paste("Check input data:", result$error_message)
        available_dists <- ""
      }
      updateSelectInput(inputId  = "select_dist",
                        choices  = available_dists,
                        selected = available_dists[1])
      output$info_text_for_input_data  <- renderText({info_text})
    }
  )
  #---END: handle manual input data---------------------------------------------
}