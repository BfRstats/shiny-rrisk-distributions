# utility functions for testing inputs in rrisk & shiny rrisk

rriskDistributionsClass$set("private", 
  "is_error", function(test, error_message)
  {
    result <- list(is_ok         = TRUE,
                   error_message = "")
    if (test) {
      result$is_ok <- FALSE
      result$error_message <- error_message
    }
    result
  }
)

rriskDistributionsClass$set("private", 
  "check_this", function(object, ...)
  {
    test_functions <- c(...)
    for (test_func in test_functions) {
      result <- test_func(object)
      if (!result$is_ok) break
    }
    result
  }
)

rriskDistributionsClass$set("private", 
  "check_all", function(...)
  {
    test_results <- list(...)
    end_result <- private$is_error(FALSE, "")
    for (this_test_result in test_results)
    {
      if (isFALSE(this_test_result$is_ok)) {
        end_result <- this_test_result
        break
      }
    }
    end_result
  }
)

#---BEGIN: atomic check methods-------------------------------------------------

rriskDistributionsClass$set("private", 
  "is_not_missing", function(object, object_name)
  {
    private$is_error(
      missing(object),
      paste("Input for object", dQuote(object_name), "is missing")
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_all_not_null", function(input_list) 
  {
    private$is_error(
      all(sapply(X = input_list, FUN = is.null)),
      paste("At least on input item must be set:", names(input_list))
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_character", function(string)
  {
    private$is_error(
      !is.character(string) || !nzchar(trimws(string)),
      paste("Input for", dQuote(names(string)), 
            "needs to be of type character, and non-empty.")
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_number", function(number) 
  {
    private$is_error(
      !is.numeric(number),
      paste("Input for", dQuote(names(number)), "must be of type numeric.")
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_integer", function(number)
  {
    private$is_error(
      !is.integer(number),
      paste("Input for", dQuote(names(number)), 
            "must be of type integer, e.g. 1L.")
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_in_range", function(number, min_range = 1, max_range = Inf) 
  {
    private$is_error(
      !is.na(number) && (number < min_range || number > max_range),
      paste("The list number for", dQuote(names(number)), 
            "is out of range:", number, "\n",
            "Allowed range is 1 to", max_range))
  }
)

rriskDistributionsClass$set("private", 
  "is_valid_name", function(name)
  {
    # remove trailing blanks from string
    name <- trimws(name)
    # set initial flag
    name_not_valid <- FALSE
    error_message <- ""
    # check now test_name
    if (!nzchar(name)) {
      # empty string as name is not allowed
      name_not_valid <- TRUE
      error_message <- "The name cannot be empty"
    } else if (name %in% c(private$reserved_words,
                           private$function_white_list_1,
                           private$function_white_list_2)) {
      # name is a reserved word
      name_not_valid <- TRUE
      error_message <- paste("Reserved words cannot be used as a name.\n",
                             "Choose a different name.")
    } else {
      # check non-empty string, check if it is valid
      # first char must be a letter
      # all other chars must alphanumerical plus "_", or just ""
      first_char <- substr(name, 1, 1)
      # get rest of the string
      # if test_name is only one char, this returns ""
      rest_of_string <- substr(name, 2, nchar(name))
      # check if test_name is valid
      if (grepl("[^A-Za-z]", first_char) ||
          grepl("\\W", rest_of_string)) {
        name_not_valid <- TRUE
        error_message <- paste("Only letters, numbers and _ are allowed.\n",
                               "The name has to start with a letter.")
      }
    }
    
    private$is_error(
      name_not_valid,
      paste("Name", dQuote(name), "for", dQuote(names(name)), "is not valid.\n",
            error_message)
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_known_name", function(name, known_names = NULL) 
  {
    name <- trimws(name)
    private$is_error(
      !is.null(known_names) && !(name %in% known_names),
      paste("Name", dQuote(name), "for", dQuote(names(name)), "is unknown.")
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_unique_node_name", function(node_name, exception_implicit_node)
  {
    node_name <- trimws(node_name)
    
    error_occured <- FALSE
    error_message <- ""
    
    if (node_name %in% private$get_empty_nodes()) {
      # node_name is already in use, but node is implicitly defined
      if (exception_implicit_node) # for add_node in rrisk, and change_node in rrisk and shiny rrisk
        error_occured <- FALSE # no error
      else # for add_node in shiny rrisk
        error_occured <- TRUE # error
    } else if (node_name %in% names(private$node_list)) {
      # node_name is not implicit, and is already in use
      error_occured <- TRUE
    } else if (node_name %in% names(private$global_expr)) {
      # node_name is used as param_name in global_expression list
      error_occured <- TRUE
      error_message <- "\nGlobal function with the same name does exist."
    }
    
    private$is_error(error_occured,
                     paste0("Name ", dQuote(node_name), " for ", 
                           dQuote(names(node_name)), " is already in use.",
                           error_message))
  }
)

rriskDistributionsClass$set("private", 
  "is_unique_param_name", function(param_name, def) 
  {
    param_name <- trimws(param_name)
    error_message <- ""
    error_occured <- FALSE
    
    is_function <- function(x)
    {
      if (!is.null(x))
        is.function(eval(str2lang(x)))
      else
        FALSE
    }
    
    if (param_name %in% names(private$global_expr)) {
      # param_name already in use for gloabl object
      error_occured <- TRUE
      error_message <- "The name for this global object is already in use"
    } else if (param_name %in% names(private$node_list)) {
      # param_name is also a node_name
      if (param_name %in% private$get_empty_nodes()) {
        # node_name is a implicit node
        if (is_function(def)) {
          # definition for global object is a function
          error_occured <- TRUE
          error_message <- paste("\nAn implicit node exists with the same",
                                 "name. Only non-function global objects can",
                                 "be used in this case.")
        }
      } else {
        # node_name is an explicit node
        error_occured <- TRUE
        error_message <- "\nName already used for node name."
      }
    }
    
    private$is_error(
      error_occured,
      paste0("Error for global object ", dQuote(param_name),
             error_message))
  }
)

rriskDistributionsClass$set("private", 
  "is_unique_author_name", function(author_name)
  {
    print(names(private$author_list))
    this_author_name <- trimws(author_name[[1]])
    private$is_error(author_name %in% names(private$author_list),
                     paste("Author name", dQuote(author_name), 
                           "is already in use."))
  }
)

rriskDistributionsClass$set("private", 
  "is_proper_param_type", function(definition) 
  {
    this_def <- definition
    private$is_error(
      !is.character(this_def) ||
      !is.function(this_def) ||
      !is.numeric(this_def),
      "Defintion must be of type 'character','numeric', or 'function'."
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_correct_syntax", function(code) 
  {
    parse_result <- tryCatch(utils::getParseData(parse(text = code,
                                                       keep.source = TRUE)),
                             error = function(e) e)
    # check if syntax is ok
    if ("error" %in% attr(parse_result, "class")) {
      private$is_error(
        TRUE,
        paste("Syntax error for", dQuote(names(code)), ":\n",
              as.character(parse_result$message))
      )
    } else {
      # init
      error_occured <- FALSE
      error_message <- ""
      # find symbol names of all symbols in code
      symbol_calls <- with(parse_result, text[token == "SYMBOL"])
      # check if reserved words were used as var name
      not_allowed_symbols <- symbol_calls %in% private$reserved_words
      if (any(not_allowed_symbols)) {
        error_occured <- TRUE
        error_message <- paste("\nReserved names were used:",
                               paste(symbol_calls[not_allowed_symbols],
                                     collapse = ", "))
      }
      # check if reserved functions were used as var name
      not_allowed_function_names <- symbol_calls %in% private$function_white_list_1
      if (any(not_allowed_function_names)) {
        error_occured <- TRUE
        error_message <- paste0(error_message,
                                "\nReserved function names were used: ",
                                paste(symbol_calls[not_allowed_function_names],
                                      collapse = ", "))
      }
      # check if user defined global expression names were used as var names
      global_object_names <- symbol_calls %in% names(private$global_expr)
      for (this_symbol in symbol_calls[global_object_names]) {
        # this var_name appears in the global object list
        if (this_symbol %in% names(private$node_list) &&
            !(this_symbol %in% private$get_empty_nodes())) {
          # If the var_name is used for a name of an global object and
          # an explicit node_name, an error occured
          error_occured <- TRUE
          error_message <- paste(error_message,
                                 paste("\nThis Symbol is used for a global",
                                       "object and an explicit node:",
                                       this_symbol))
        }
      }

      private$is_error(
        error_occured,
        paste("These symbols are reserved and not allowed in a 
              user defined expression:", error_message)
      )
    }
  }
)

rriskDistributionsClass$set("private",
  "are_all_functions_specified_or_allowed", function()
  {
    # get the names of all global objects that are functions
    global_function_names <- NULL
    for (global_obj_name in names(private$global_expr)) {
      if (is.function(eval(private$global_expr[[global_obj_name]])))
        global_function_names <- c(global_function_names, global_obj_name)
    }
    
    # init result output in case everything is fine
    result <- list(is_ok = TRUE,
                   error_message = "")
    # check for every node types "param_dist", "user_defined",
    # and "bootstrap", if they are using only allowed functions
    for (node_name in names(private$node_list)) {
      node <- private$node_list[[node_name]]
      if (node$type == "param_dist") {
        # check for node type "param_dist"
        for (code in node$dist_def$def) {
          result <- private$uses_allowed_functions(
            setNames(code, node_name),
            c(private$function_white_list_1, global_function_names)
          )
          if (!result$is_ok) return(result)
        }
      } else if (node$type == "user_defined") {
        # check for node type "user_defined"
        result <- private$uses_allowed_functions(
          setNames(node$display_code, node_name),
          c(private$function_white_list_1, global_function_names)
        )
        if (!result$is_ok) break
      } else if (node$type == "bootstrap") {
        # check for node type "bootstrap"
        bootstrap_func_names <- c(private$function_white_list_bs,
                                  global_function_names)
        for (sum_stat in node$bootstrap_data$sum_stats) {
          if (!(sum_stat$func_name) %in% bootstrap_func_names) {
            result$is_ok <- FALSE
            result$error_message <- paste0("Function '", sum_stat$func_name,
                                           "' for bootstrap node '", node_name, 
                                           "' is either not allowed, or not ",
                                           "defined")
            return(result)
          }
        }
      }
    }
    result
  }
)

rriskDistributionsClass$set("private", 
  "uses_allowed_functions", function(code, allowed_functions) 
  {
    # parse expression, get parsed data
    parse_result <- utils::getParseData(parse(text = code,
                                              keep.source = TRUE))
    # find function names of all function calls in code
    function_calls <- with(parse_result,
                           text[token == "SYMBOL_FUNCTION_CALL"])
    # are found function names in function white list, or
    # are names of functions, defined by the user 
    this <- function_calls %in% allowed_functions
    # return all function names that are not in function white list,
    # are not defined by user
    unknown_functions <- function_calls[!this]
      
    private$is_error(
      length(unknown_functions) > 0,
      paste("The input for", dQuote(names(code)), 
            "contains following unknown (or forbidden) functions:", 
             paste(unknown_functions, collapse = ", "))
    )
  }
)

rriskDistributionsClass$set("private", 
  "is_pure_expression", function(code, allowed_functions) 
  {
    # check if definition code is ok
    result <- utils::getParseData(parse(text        = code,
                                        keep.source = TRUE))
    
    if ("FUNCTION" %in% result$token) {
      # if definition is a function, than only SYMBOL_FORMALS, 
      # and local defined variables are allowed
      result           <- result[result$terminal == TRUE,]
      symbol_formals   <- result$text[which(result$token == "SYMBOL_FORMALS")]
      symbol_subs      <- result$text[which(result$token == "SYMBOL_SUB")]
      symbols          <- unique(result$text[which(result$token == "SYMBOL")])
      # remove the symbols in the function declaration (SYMBOL_FORMALS)
      # from the symbol vector
      internal_symbols <- setdiff(symbols, symbol_formals)
      
      # find non local variables, i.e. variables that are used before being
      # declared within the function. For these variables, R will search outside
      # of the function. We do not want to have this here.
      non_local_symbols <- c()
      for (this_internal_symbol in internal_symbols) {
        first <- which(result$text == this_internal_symbol)[1]
        if (result$token[first+1] != "LEFT_ASSIGN" &&
            result$token[first-2] != "FOR") {
          non_local_symbols <- c(non_local_symbols, 
                                 this_internal_symbol)
        }
      }
      
      # did any of the found non local symbols be defined prior as
      # SYMBOL_SUB?
      for (this_symbol in non_local_symbols) {
        if (this_symbol %in% symbol_subs) {
          # if so, check in parsed data for line1 and col1
          # if the non local symbols were defined prior as SYMBOL_SUB
          if (result$token[which(result$text == this_symbol)][1] == "SYMBOL_SUB") {
            # this non local symbol was first defined in a SYMBOL_SUB
            # therefore remove it from non-local symbols
            non_local_symbols <- non_local_symbols[!(non_local_symbols %in% this_symbol)]
          }
        }
      }
      
      # non local defined symbols might also be functions, 
      # e.g. in apply(data, 1, mean);
      # the function "mean" would appear as a symbols in the parse tree in results.
      # check here for possible function with methods::existsFunction, 
      # and if the symbol is a function, check if it is allowed to use, 
      # i.e. part of the white list
      if (length(non_local_symbols) > 0) {
        is_function <- sapply(non_local_symbols, methods::existsFunction)
        is_allowed_function <- non_local_symbols[is_function] %in% allowed_functions
        # check for error
        check <- private$is_error(
          any(!is_allowed_function),
          paste("The following functions are not allowed:",
                non_local_symbols[is_function][!is_allowed_function])
        )
        if (!check$is_ok) return(check)
        # remove function names from the internal_symbol vector
        non_local_symbols <- non_local_symbols[!is_function]
      }

      private$is_error(length(non_local_symbols) > 0,
                       paste("Only local defined variables are allowed in", 
                             "functions.\n",
                             "Check these variables:", non_local_symbols, 
                             "\n"))
    } else {
      private$is_error("SYMBOL" %in% result$token,
                       paste("No variables are allowed in constant expressions.", 
                              "Use a function instead."))
    }
    
  }
)

rriskDistributionsClass$set("private", 
  "is_valid_param_dist_list", function(param_dist) 
  {
    valid_param_dist_entries <- c("name", "def")
    param_dist_names <- param_dist
    is_not_valid <- !(param_dist_names %in% valid_param_dist_entries)
    private$is_error(any(is_not_valid),
                     paste("Entry in list", dQuote(names(param_dist)), 
                           "is not correct:",
                            paste(param_dist_names[is_not_valid],
                                  collapse = ", "), "\n",
                            "Allowed values are:",
                            paste(valid_param_dist_entries, 
                                  collapse = ", ")))
  }
)

rriskDistributionsClass$set("private", 
  "has_no_duplication", function(provided_dist_var_names) 
  {
    is_duplicated <- duplicated(provided_dist_var_names)
    private$is_error(any(is_duplicated),
                     paste("Duplicated parameter/s:",
                            paste(unique(provided_dist_var_names[is_duplicated]),
                                  collapse = ", ")))
  }
)

rriskDistributionsClass$set("private", 
  "has_no_unknown_params", function(provided_dist_var_names, known_params) 
  {
    # check if provided names are NOT used by the distribution
    is_not_var_name <- !(provided_dist_var_names %in% known_params)
    private$is_error(any(is_not_var_name),
                     paste("Unkown parameter/s in dist definition: ", 
                           paste(provided_dist_var_names[is_not_var_name], 
                                 collapse = ", "), "\n",
                           "Allowed params are:",
                           paste(known_params, collapse = ", "), "\n"))
  }
)

rriskDistributionsClass$set("private", 
  "has_no_non_optional_params_missing", function(provided_dist_var_names, 
                                                 this_params)
  {
    # check if non-optional parameter are missing
    non_optional_vars <- vapply(X         = this_params, 
                                FUN       = function(x) !x$optional,
                                FUN.VALUE = logical(1))
    non_opt_dist_var_names <- names(this_params)[non_optional_vars]
    is_not_present <- !(non_opt_dist_var_names %in% provided_dist_var_names)
    private$is_error(any(is_not_present),
                     paste("The parameter/s",
                           paste(non_opt_dist_var_names[is_not_present],
                                 collapse = ", "),
                           "are non-optional"))
  }
)

rriskDistributionsClass$set("private", 
  "are_params_in_range", function(user_input, dist_info)
  {
    #---BEGIN: extend user input for not set optional params--------------------
    # some optional parameter for parametric distibutions contain information
    # needed to validate other parameter of the distribtuion.
    #
    f <- function(user_input, dist_info) 
    {
      # obtain logical vector indicating if param is optional (TRUE), or not (FALSE)
      is_opt_param <- sapply(dist_info$def, function(x) x$optional)
      # get optional parameter names
      opt_param_names <- names(is_opt_param)[is_opt_param]
      # get optional parameters that are not set by user
      not_set_opt_param_names <- opt_param_names[!(opt_param_names %in% names(user_input))]
      # extend user input with not set optional params
      c(user_input,
        sapply(
          X = not_set_opt_param_names,
          FUN = function(param_name) 
          {
            param_init <- dist_info$def[[param_name]]$init
            if (is.character(param_init))
              user_input[[param_init]]
            else
              param_init
           }, 
           simplify = FALSE
          )
        )
    }
    extended_user_input <- f(user_input, dist_info)
    #---END: extend user input for not set optional params----------------------
    
    # set initial empty result in case nothing is to check
    result <- list(is_ok         = TRUE,
                   error_message = "")
    # apply check rules to user input
    for (param_name in names(user_input)) {
      # if param is of type character, omit the evaluation for now
      # move this to f
      if (is.character(user_input[[param_name]])) next
      # get the check rules for this parameter
      check_rules <- dist_info$def[[param_name]]$check_rules
      # apply all defined check rules for this parameter
      for (check_rule in check_rules) {
        # evaluate this check rule with user input
        result <- eval(expr  = str2lang(check_rule), 
                       envir = extended_user_input)
        # throw error if result of evaluation is *explicitly* FALSE
        # otherwise ignore result
        result <- private$is_error(isFALSE(result),
                                   paste("Parameter", dQuote(param_name), 
                                         "is out of range.\n",
                                         param_name, " = ", 
                                         user_input[[param_name]], "\n", 
                                         "Rule is:", check_rule, "\n"))
        if (!result$is_ok) return(result)
      }
    }
    
    result
  }
)

rriskDistributionsClass$set("private", 
  "is_correct_file_extension", function(file_path, ext)
  {
    file_ext <- tools::file_ext(file_path)
    private$is_error(!(file_ext %in% ext),
                     paste("File extension", dQuote(file_ext), 
                           "is not allowed.",
                           "Allowed file extensions:", 
                           toString(ext[!(ext %in% "")])))
  }
)

rriskDistributionsClass$set("private", 
  "is_file_readable", function(file_path)
  {
    private$is_error(file.access(file_path, mode = 4),
                     paste("Cannote read file:", file_path))
  }
)

rriskDistributionsClass$set("private", 
  "is_correct_info_list", function(info)
  {
    info_list_names <- c("unit", "source", "descr")
    private$is_error(!is.list(info) ||
                     !all(info_list_names %in% names(info)),
                     paste("Info must be a list with the named entries:",
                           paste(info_list_names, collapse = ", ")))
  }
)

rriskDistributionsClass$set("private", 
  "is_graph_a_dag", function() 
  {
    edge_list <- private$edge_list
    # try to remove all nodes from edge list
    remove_stuff <- TRUE
    while (remove_stuff) {
      remove_stuff <- FALSE
      for (i in seq_along(edge_list)) {
        removed <- vapply(X         = edge_list[[i]],
                          FUN       = function(x) length(edge_list[[x]]) == 0,
                          FUN.VALUE = logical(1),
                          USE.NAMES = FALSE)
        if (length(removed) > 0 &&
            all(removed)) {
          edge_list[[i]] <- character(0)
          remove_stuff <- TRUE
        }
      }
    }
    
    # check if all nodes could be removed
    removed <- vapply(X         = edge_list, 
                      FUN       = function(x) length(x) == 0,
                      FUN.VALUE = logical(1),
                      USE.NAMES = FALSE)
    
    private$is_error(!all(removed),
                     paste("Model Graph is not a DAG.\n",
                           "Check nodes:", 
                           paste(names(edge_list[!removed]),
                                 collapse = ", ")))
  }
)

rriskDistributionsClass$set("private", 
  "is_single_graph", function() 
  {
    get_all_source_node_names <- function(node_name) 
    {
      this_source_nodes <- private$edge_list[[node_name]]
      other_source_nodes <- lapply(this_source_nodes,
                                   get_all_source_node_names)
      c(this_source_nodes, unlist(other_source_nodes))
    }
    
    is_single <- TRUE
    end_nodes <- self$get_end_nodes()
    if (length(end_nodes) > 1) {
      node_name_list <- sapply(
        X   = end_nodes,
        FUN = get_all_source_node_names,
        simplify = FALSE
      )
      overlapping_nodes <- Reduce(intersect, node_name_list)
      if (length(overlapping_nodes) == 0) {
        is_single <- FALSE
      }
    }
    
    private$is_error(isFALSE(is_single),
                     "Model contains more than one graph")
  }
)

rriskDistributionsClass$set("private", 
  "is_model_fully_specified", function() 
  {
    empty_nodes <- private$get_empty_nodes()
    private$is_error(!is.null(empty_nodes),
                     paste("The model is not fully specified.\n",
                           "These nodes are empty (implicit): ",
                          paste(empty_nodes, collapse = ", ")))
  }
)

rriskDistributionsClass$set("private",
  "is_model_stochastic", function()
  {
    is_not_stochastic <- TRUE
    for (node in private$node_list) {
      if (!is.null(node$mc_dim) &&
          (node$mc_dim == 1 ||
           node$mc_dim == 2)) {
        is_not_stochastic <- FALSE
        break
      }
    }
    private$is_error(is_not_stochastic,
                     "Model does not contain any variable nodes.")
  }
)

rriskDistributionsClass$set("private", 
  "is_result_available", function(node_name)
  {
    tmp <- private$is_error(
      !(node_name %in% names(private$result_list)),
      paste("No data available for node", node_name)
    )
    tmp
  }
)

rriskDistributionsClass$set("private", 
  "are_bootstrap_var_names_ok", function(var_names, replace_implicit_node, 
                                         is_change = FALSE)
  {
    var_names <- trimws(strsplit(var_names, ",")[[1]])
    for (var_name in var_names) {
      result <- private$check_this(
        object = setNames(var_name, "var_name"),
        private$is_character,
        private$is_valid_name,
        if (is_change) {
          preset_args(
            private$is_unique_node_name,
            exception_implicit_node = replace_implicit_node
          )
        }
      )
      if (!result$is_ok) break
    }
    result
  }
)
#---END: atomic check methods---------------------------------------------------

