#---BEGIN: public methods "add_author", "remove_author", "get_author", and
#          "get_author_list"----------------------------------------------------
# MAIN METHOD add_author
rriskDistributionsClass$set("public", "add_author",
  function(author_name, additional_info, silent = FALSE) 
  {
    # check input
    result <- private$check_add_author_input(author_name, additional_info)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
  
    author_name <- trimws(author_name)
    private$author_list[[author_name]] <- c(name = author_name, 
                                            additional_info)
    private$tickle_shiny()
    
    invisible(result)
  }
)

# MAIN METHOD change_author
rriskDistributionsClass$set("public", "change_author",
  function(author_name, new_author_name = author_name,
           additional_info = NULL, silent = FALSE) 
  {
    # check input
    result <- private$check_change_author_input(author_name, new_author_name,
                                                additional_info)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
    
    # get old author entry
    old_entry <- self$get_author(author_name = author_name)
    if (is.null(additional_info)) 
      additional_info <- old_entry[names(old_entry) != "name"]
    
    # remove old author entry
    self$remove_author(author_name = author_name)
    
    # set new author
    self$add_author(author_name     = new_author_name,
                    additional_info = additional_info,
                    silent          = silent)
    
    invisible(result)
  }
)

# MAIN METHOD remove_author
rriskDistributionsClass$set("public", "remove_author",
  function(author_name = NULL, list_num = NULL, silent = FALSE) 
  {
    result <- private$check_author_input(author_name, list_num)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
    
    if (is.null(author_name))
      author_name <- names(private$author_list[list_num])
    
    # remove author from list
    private$author_list[[author_name]] <- NULL
    
    private$tickle_shiny()
    
    invisible(result)
  }
)

# MAIN METHOD get_author
rriskDistributionsClass$set("public", "get_author",
  function(author_name = NULL, list_num = NULL, silent = FALSE) 
  {
    result <- private$check_author_input(author_name, list_num)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
    
    if (is.null(author_name))
      author_name <- names(private$author_list[list_num])
    
    if (silent) {
      result$author_list <- private$author_list[[author_name]]
      invisible(result)
    } else
      private$author_list[[author_name]]
  }
)

# MAIN METHOD get_author_list
rriskDistributionsClass$set("public", "get_author_list",
  function() private$author_list)

#---END: public methods "add_author", "remove_author", "get_author", and
#        "get_author_list"------------------------------------------------------

#---BEGIN: private utility methods for author methods---------------------------
rriskDistributionsClass$set("private", "check_add_author_input",
  function(author_name, additional_info) 
  {
    private$check_all(
      private$check_this(object = setNames(author_name, "author_name"),
                         private$is_character,
                         private$is_unique_author_name),
      private$is_error(!is.null(additional_info) && !is.list(additional_info),
                       "Input variable 'additional_info' must be a list.")
    )
  }
)

rriskDistributionsClass$set("private", "check_change_author_input",
  function(author_name, new_author_name, additional_info) 
  {
    private$check_all(
      private$check_this(object = setNames(author_name, "author_name"),
                         private$is_character,
                         preset_args(
                           private$is_known_name,
                           known_names = names(private$author_list)
                         )),
      if (!identical(trimws(author_name), trimws(new_author_name))) {
        private$check_this(object = setNames(new_author_name, 
                                             "new_author_name"),
                           private$is_character,
                           private$is_unique_author_name)
      },
      private$is_error(!is.null(additional_info) && !is.list(additional_info),
                       "Input variable 'additional_info' must be a list.")
    )
  }
)

rriskDistributionsClass$set("private", "check_author_input",
  function(author_name = NULL, list_num = NULL) 
  {
    private$check_all(
      private$check_this(object = setNames(list(author_name, list_num),
                                           nm = c("author_name", "list_num")),
                         private$is_all_not_null),
      if (!is.null(author_name)) {
          private$check_this(object = setNames(author_name, "author_name"),
                             private$is_character,
                             private$is_author_name_known)
      },
      if (!is.null(list_num)) {
        private$check_this(object = setNames(list_num, "list_num"),
                           private$is_number,
                           preset_args(
                             private$is_in_range,
                             max_range = length(private$author_list)
                           ))
      }
    )
  }
)
#---END: private utility methods for public author methods----------------------
