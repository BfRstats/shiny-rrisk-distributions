menu_bar_server <- function(input, output, session, rrisk_dist_obj)
{
  #---BEGIN: handle user input file---------------------------------------------
  input_data <- reactiveVal()
  df_user_data <- NULL
  
  observeEvent(
    eventExpr = input$open_csv_file,
    handlerExpr = {
      # open connection to file
      file_con <- file(description = input$open_csv_file$datapath, 
                       open        = "r")
      # read all lines in file
      input_data(readLines(file_con))
      # close connection to file
      close(file_con)
      # show dialog for interpretation user data
      showModal(
        modalDialog(
          title = "Interpretation of user data file",
          verbatimTextOutput(outputId = "input_data_content"),
          verbatimTextOutput(outputId = "df_content"),
          radioButtons(inputId  = "header_yes_no",
                       label    = "Header?",
                       choices  = c("yes", "no"),
                       selected = "yes",
                       inline   = TRUE),
          radioButtons(inputId  = "rownames_yes_no",
                       label    = "Row names?",
                       choices  = c("yes", "no"),
                       selected = "yes",
                       inline   = TRUE),
          selectInput(inputId  = "select_separator",
                      label    = "Separator",
                      choices  = c("white space", "comma", "semicolon", "tab"),
                      selected = "white space"),
          size = "l",
          footer = htmltools::tagList(
            modalButton("Cancel"),
            actionButton(inputId = "btn_add_user_data",
                         label   = "Add User Data")
          )
        )
      )
    }
  )
  
  # for modal dialog interpretation of user file
  output$input_data_content <- renderText({paste(input_data(),
                                                 collapse = "\n")})

  # for modal dialog interpretation of user file
  output$df_content <- renderPrint({
    split_rule <- switch(
      input$select_separator,
      "white space" = " ",
      "comma"       = ",",
      "semicolon"   = ";",
      "tab"         = "\t"
    )
    df <- convert_to_df(
      input_data   = input_data(),
      header       = ifelse(input$header_yes_no == "yes", TRUE, FALSE),
      split_rule   = split_rule,
      has_rownames = ifelse(input$rownames_yes_no == "yes", TRUE, FALSE))
    df_user_data <<- df
    df
  })
  
  # click ok of modal dialog interpretation of user file
  observeEvent(
    eventExpr = input$btn_add_user_data,
    handlerExpr = {
      removeModal()
      # set data in rrisk_dist object
      result <- rrisk_dist_obj()$set_data(
        x = df_user_data[[1]],
        y = if (ncol(df_user_data) > 1) df_user_data[[2]] else NULL,
        input_type = input$input_data_type
      )
      if (result$is_ok) {
        info_text <- ""
        rrisk_dist_obj()$fit_all_distributions()
        #available_dists <- names(rrisk_dist_obj()$get_fit_result(stat_name = input$select_statistic))
        available_dists <- rrisk_dist_obj()$get_fitted_dist_name_list(sorted_by = input$select_statistic)
      } else {
        info_text <- paste("Check input data:", result$error_message)
        available_dists <- ""
      }
      # update
      updateSelectInput(inputId  = "select_dist",
                        choices  = available_dists,
                        selected = available_dists[1])
      output$info_text_for_input_data  <- renderText({info_text})
      # update radio button pdf/cdf
      updateRadioButtons(
        session  = session,
        inputId  = "input_data_type",
        selected = ifelse(test = ncol(df_user_data) == 1,
                          yes  = "pdf",
                          no   = "cdf")
      )
      # update textAreaInputs
      updateTextAreaInput(
        session = session,
        inputId = "input_data",
        value   = paste(df_user_data[[1]], collapse = ", ")
      )
      if (ncol(df_user_data) > 1) {
        updateTextAreaInput(
          session = session,
          inputId = "input_data_cdf",
          value   = paste(df_user_data[[2]], collapse = ", ")
        )
      }
    }
  )
  #---END: handle user input file-----------------------------------------------
  
  #---BEGIN server handling menu bar--------------------------------------------
  
  # new project
  observeEvent(
    eventExpr = input$menu_bar,
    handlerExpr = {
      if (input$menu_bar == "NEW") {
        myShinyAlert(
          title             = "Create new project?",
          text              = "Do you want to create a new project?",
          type              = "info",
          showCancelButton  = TRUE,
          confirmButtonText = "New project",
          callbackR         = function(value) {
            if (isTRUE(value)) {
              # clear data fields
              # for x
              updateTextAreaInput(
                session = session,
                inputId = "input_data",
                value   = ""
              )
              # for y
              updateTextAreaInput(
                session = session,
                inputId = "input_data_cdf",
                value   = ""
              )
              # reset radio button pdf/cdf
              updateRadioButtons(
                session  = session,
                inputId  = "input_data_type",
                selected = "pdf"
              )
              # clear model
              rrisk_dist_obj()$clear_project()
              # reset selection for fitted distribution
              updateSelectInput(
                session  = session,
                inputId  = "select_dist",
                choices  = c(""),
                selected = ""
              )
              # reset selected statistic
              updateSelectInput(
                session  = session,
                inputId  = "select_statistic",
                selected = "loglike"
              )
              # reset info text field for fit results
              output$info_text_for_input_data <- renderUI({""})
              # reset unit text input
              updateTextInput(
                session = session,
                inputId = "unit_description",
                value   = ""
              )
              # reset source description text input
              updateTextAreaInput(
                session = session,
                inputId = "source_description",
                value   = ""
              )
              # reset description text input
              updateTextAreaInput(
                session = session,
                inputId = "general_description",
                value   = ""
              )   
            }
          }
        )
        updateNavbarPage(
          session  = session,
          inputId  = "menu_bar",
          selected = "1"
        )
      }
    }
  )
  
  # open project file
  observeEvent(
    eventExpr   = input$open_rrisk_dist_file,
    handlerExpr = {
      open_rrisk_dist_file(rrisk_dist_obj, 
                           input$open_rrisk_dist_file$datapath,
                           session)
    }
  )
  
  # save model by download
  output$download_project <- downloadHandler(
    filename = function()
    {
      #get_proper_file_name(model_name(), "rrisk")
      "test.rriskdist"
    }, 
    content  = function(file_path) 
    {
      rrisk_dist_obj()$set_info(list(unit        = input$unit_description,
                                     source      = input$source_description,
                                     description = input$general_description))
      rrisk_dist_obj()$save_file(file_path, silent = TRUE)
    },
    contentType = "application/json"
  )

  # download export of current fit to shiny rrisk
  output$download_export_to_shiny_rrisk <- downloadHandler(
    filename = function()
    {
      "test_export.rriskdistex"
    },
    content = function(file_path)
    {
      myShinyAlert(
        title = "Export to shiny rrisk with current fit",
        text  = paste("Current fit:", input$select_dist),
        type  = "info"
      )
      rrisk_dist_obj()$set_info(
        list(unit        = input$unit_description,
             source      = input$source_description,
             description = input$general_description)
      )
      rrisk_dist_obj()$save_file(
        file_path    = file_path,
        exported_fit = input$select_dist,
        silent       = TRUE
      )
    }
  )
    
  # download report

  #---END server handling menu bar----------------------------------------------
}