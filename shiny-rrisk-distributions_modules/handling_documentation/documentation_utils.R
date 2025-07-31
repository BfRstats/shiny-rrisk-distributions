add_change_author_dialog <- function(author = list(name        = "", 
                                                   institution = "", 
                                                   email       = ""),
                                     is_add_dialog = TRUE) 
{
  if (is_add_dialog) {
    add_change_btn_text <- "Add new author"
    modal_dialog_title  <- "Add Author"
  } else {
    add_change_btn_text <- "Change author"
    modal_dialog_title  <- "Change Author"
  }
  
  modalDialog(
    title = modal_dialog_title,
    textInput(inputId = "author_name_text",
              label   = "Name",
              value   = author$name),
    textInput(inputId = "institution_name_text",
              label   = "Institution",
              value   = author$institution),
    textInput(inputId = "email_text",
              label   = "E-Mail",
              value   = author$email),
    footer = tagList(modalButton("Cancel"),
                     actionButton(inputId = "btn_add_change_author",
                                  label   = add_change_btn_text))
  )
}

create_author_table <- function(author_list, colnames)
{
  if (length(author_list) > 0) {
    author_mat <- sapply(
      X   = author_list,
      FUN = function(x)
      {
        name        <- ifelse(is.null(x$name), "", x$name)
        email       <- ifelse(is.null(x$email), "", x$email)
        institution <- ifelse(is.null(x$institution), "", x$institution)
        c(name, institution, email)
      }, 
      simplify = TRUE
    )
    df <- as.data.frame(t(author_mat), row.names = NULL)
    colnames(df) <- colnames
    rownames(df) <- NULL
  } else {
    df <- list2DF(setNames(object = vector("list", length(colnames)), 
                           nm     = colnames))
  }
  
  actionButtons <- sapply(
    X   = seq_len(nrow(df)),
    FUN = build_action_buttons,
    FALSE, "author")

  df <- cbind.data.frame(df, Actions = actionButtons)

  DT::datatable(
    data      = df,
    # Need to disable escaping for html as string to work
    escape    = FALSE,
    colnames  = colnames(df),
    selection = 'single',
    options   = list(paging = FALSE),
    callback = DT::JS(
      "table.on('dblclick', 'td',", 
      "  function() {",
      "    let data = table.row(this).data();",
      "    Shiny.setInputValue('author_table_dblclick', {list_num: data[0]});",
      "    Shiny.setInputValue('author_table_dblclick', {author_name: data[1]});",
      "  }",
      ");"
    )
  )
}