#---BEGIN: public main methods "save_model", and "open_model"-------------------
# MAIN METHOD save_model
rriskDistributionsClass$set("public", 
  "save_file", function(file_path, exported_fit = NULL, silent = FALSE)
  {
    result <- private$is_correct_file_extension(file_path, 
                                                ext = c("", "rriskdist", 
                                                        "rriskdistex"))
    #result <- private$check_input_for_saving_file(file_path, 
    #                                              ext = c("", "rriskdist"),
    #                                              exported_fit)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    if (tools::file_ext(file_path) == "") 
      if (is.null(exported_fit))
        file_path <- paste0(file_path, ".rriskdist")
      else
        file_path <- paste0(file_path, ".rriskdistex")
    
    obj_representation <- list(
      file_type         = if (is.null(exported_fit)) 
                            "rrisk-distributions"
                          else 
                            "rrisk-distributions-export",
      file_version      = 1L,
      data_type         = private$data_type,
      provided_data     = private$provided_data,
      provided_data_cdf = if (private$data_type == "cdf") 
                            private$provided_data_cdf 
                          else 
                            NULL,
      info_list         = private$info_list,
      author_list       = private$author_list,
      model_name        = private$model_name
    )
    
    if (!is.null(exported_fit)) {
      obj_representation$selected_fit <- private$fit_results[[exported_fit]]
      obj_representation$selected_fit$fitted_dist_name <- exported_fit
    }
    
    # readable json file
    write(x    = jsonlite::serializeJSON(x      = obj_representation,
                                         pretty = TRUE), 
          file = file_path)
    
    invisible(result)
  }
)

# MAIN METHOD open_model
rriskDistributionsClass$set("public", 
  "open_file", function(file_path, silent = FALSE) 
  {
    
    result <- private$check_this(object = file_path,
                                 preset_args(
                                   private$is_correct_file_extension,
                                   ext = "rriskdist"
                                 ),
                                 private$is_file_readable)
    
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    #---BEGIN: read model file--------------------------------------------------
    con <- file(file_path, "r")
    obj_data <- tryCatch(jsonlite::unserializeJSON(readLines(con)),
                         error = function(e) "no-json")
    close(con)
    
    if (is.character(obj_data) &&
        obj_data == "no-json") {
      if (silent)
        return(list(is_ok = FALSE,
                    error_message = "Could not read file."))
      else {
        stop("Could not read file")
      }
    }
    #---END: read model file----------------------------------------------------
    
    #---BEGIN: set slots in rrisk dist obj with data----------------------------
    file_type <- obj_data$file_type
    file_version <- as.integer(obj_data$file_version)
    
    if (file_type == "rrisk-distributions") {
      
      # clear existing project
      self$clear_project()
      
      # set new data
      private$data_type         <- obj_data$data_type
      private$provided_data     <- obj_data$provided_data
      private$provided_data_cdf <- if (obj_data$data_type == "cdf") 
                                     obj_data$provided_data_cdf 
                                   else 
                                     NULL
      private$info_list         <- obj_data$info_list
      private$author_list       <- obj_data$author_list
      private$model_name        <- obj_data$model_name
      
    } else {
      
      result$is_ok <- FALSE
      result$error_message <- paste("File type '", file_type, "' is unknown.")
      
    }
    #---END: set slots in rrisk dist obj with data------------------------------
    
    private$tickle_shiny()
    
    invisible(result)
  }
)

rriskDistributionsClass$set("public", 
  "clear_project", function()
  {
    private$provided_data     <- c()
    private$provided_data_cdf <- c()
    private$data_type         <- "pdf"
    private$fit_results       <- list()
    private$info_list         <- list()
    private$author_list       <- list()
    private$model_name        <- ""
    
    private$tickle_shiny()
  }
)
#---END: public main methods "save_model", and "open_model"---------------------

# rriskDistributionsClass$set("private",
#   "check_project_file", function(file_path)
#   {
#     private$check_this(object = file_path,
#                        preset_args(
#                          private$is_correct_file_extension, 
#                          ext = "rriskdist"
#                        ),
#                        private$is_file_readable)
#   }
# )

# rriskModelClass$set("private",
#   "check_node_expr_of_input_file", function()
#   {
#     # init in case of no nodes
#     result <- private$is_error(FALSE, "")
#     #
#     allowed_func_names <- c(private$function_white_list_1,
#                             private$get_global_func_names(),
#                             private$dist_func_names,
#                             "do_bootstrapping", "list", "cbind")
#     #
#     for (this_node_name in names(private$node_list)) {
#       
#       result <- private$check_this(
#         object = setNames(private$node_list[[this_node_name]]$code, 
#                           "user_def_expr"),
#         private$is_character,
#         preset_args(
#           private$is_correct_syntax,
#           check_for_reserved_words = FALSE
#         ),
#         preset_args(
#           private$uses_allowed_functions,
#           allowed_functions = allowed_func_names
#         )
#       )
#       
#       if (!result$is_ok) break
#     }
#     
#     result
#   }
# )
