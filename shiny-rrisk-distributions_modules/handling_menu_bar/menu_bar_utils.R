open_rrisk_dist_file <- function(rrisk_dist_obj, file_path, session)
{
  result <- rrisk_dist_obj()$open_file(file_path, silent = TRUE)
  
  if (result$is_ok) {
    
    updateTextInput(
      session = session,
      inputId = "text_model_name",
      value   = rrisk_dist_obj()$get_model_name()
    )
    
    input_data <- rrisk_dist_obj()$get_data()
    # set radio button for type of input data (pdf or cdf)
    updateRadioButtons(
      session  = session,
      inputId  = "input_data_type",
      selected = input_data$data_type
    )
    # update textAreaInputs
    updateTextAreaInput(
      session = session,
      inputId = "input_data",
      value   = paste(input_data$provided_data, collapse = ", ")
    )
    if (input_data$data_type == "cdf") {
      updateTextAreaInput(
        session = session,
        inputId = "input_data_cdf",
        value   = paste(input_data$provided_data_cdf, collapse = ", ")
      )
    }
    
    rrisk_dist_obj()$fit_all_distributions(silent = TRUE)
    
    #available_dists <- names(rrisk_dist_obj()$get_fit_result())
    available_dists <- rrisk_dist_obj()$get_fitted_dist_name_list(sorted_by = "loglike")

    # update selected dist
    updateSelectInput(inputId  = "select_dist",
                      choices  = available_dists,
                      selected = available_dists[1])
    # set selected statistics
    updateSelectInput(inputId  = "select_statistic",
                      selected = "loglike")
    
    # #output$info_text_for_input_data  <- renderText({info_text})
    
    # update documentation area
    info_list <- rrisk_dist_obj()$get_info()
    
    # update unit
    updateTextInput(
      session = session,
      inputId = "unit_description",
      value   = info_list$unit
    )
    
    # update source
    updateTextAreaInput(
      session = session,
      inputId = "source_description",
      value   = info_list$source
    )
    
    # update description
    updateTextAreaInput(
      session = session,
      inputId = "general_description",
      value   = info_list$description
    )
    
  } else {
    myShinyAlert(
      title = "Could not open model file",
      text  = result$error_message,
      type  = "error")
  }
}