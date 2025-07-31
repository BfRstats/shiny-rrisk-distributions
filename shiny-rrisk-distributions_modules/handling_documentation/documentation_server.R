documentation_server <- function(input, output, rrisk_dist_obj)
{
  replace_author <- FALSE
  remind_author <- NULL 
  
  # ADD author
  observeEvent(
    eventExpr   = input$btn_add_author,
    handlerExpr = {
      showModal(add_change_author_dialog())
      replace_author <<- FALSE
    }
  )
  
  # CHANGE author
  observeEvent(
    eventExpr   = input$author_table_dblclick,
    handlerExpr = {
      # get row number
      author_name <- input$author_table_dblclick$author_name
      # get author info
      result <- rrisk_dist_obj()$get_author(author_name = author_name, 
                                            silent      = TRUE)
      remind_author <<- result$author_list$name
      showModal(add_change_author_dialog(result$author_list, 
                                         is_add_dialog = FALSE))
      replace_author <<- TRUE
    }
  )
  
  # DELETE author
  observeEvent(
    eventExpr   = input$btn_delete_author,
    handlerExpr = {
      item_num <- parseActionButton(input$btn_delete_author)
      myShinyAlert(
        title             = "Delete author?",
        text              = "Do you want to delete author?",
        type              = "info",
        showCancelButton  = TRUE,
        confirmButtonText = "Delete",
        callbackR         = function(value) {
          if (isTRUE(value))
            rrisk_dist_obj()$remove_author(list_num = item_num)
        }
      )
    }
  )
  
  # ADD or CHANGE author in rrisk model
  observeEvent(
    eventExpr = input$btn_add_change_author,
    handlerExpr = {
      if (replace_author) {
        result <- rrisk_dist_obj()$change_author(
          author_name     = remind_author,#(), 
          new_author_name = input$author_name_text,
          additional_info = list(institution = input$institution_name_text,
                                 email       = input$email_text),
          silent          = TRUE
        )
      } else {
        result <- rrisk_dist_obj()$add_author(
          author_name     = input$author_name_text,
          additional_info = list(institution = input$institution_name_text,
                                 email       = input$email_text),
          silent          = TRUE
        )
      }
      if (result$is_ok)
        removeModal()
      else {
        myShinyAlert(
          title = "Error in input for author",
          text  = result$error_message,
          type  = "error"
        )
      }
    }
  )
  
  # author table
  output$author_table <- DT::renderDT({
    create_author_table(rrisk_dist_obj()$get_author_list(), 
                        c("Name", "Institution", "E-Mail"))
  })
  
}