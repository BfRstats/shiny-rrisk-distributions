source("utils.R", chdir = TRUE)

myShinyAlert <- function(
  title = "",
  text = "",
  type = "",
  closeOnEsc = TRUE,
  closeOnClickOutside = FALSE,
  html = FALSE,
  showCancelButton = FALSE,
  showConfirmButton = TRUE,
  inputType = "text",
  inputValue = "",
  inputPlaceholder = "",
  confirmButtonText = "OK",
  confirmButtonCol = "#AEDEF4",
  cancelButtonText = "Cancel",
  timer = 0,
  animation = FALSE,
  imageUrl = NULL,
  imageWidth = 100,
  imageHeight = 100,
  className = "",
  callbackR = NULL,
  callbackJS = NULL,
  inputId = "shinyalert",
  size = "s",
  immediate = FALSE,
  session = getSession()
) 
{
  if (timer < 0) {
    stop("timer cannot be negative.")
  }
  if (!type %in% c("", "warning", "error", "success", "info", "input")) {
    stop("type =", type, " is not supported.")
  }
  if (!size %in% c("xs", "s", "m", "l")) {
    stop("size =", size, " is not supported.")
  }
  if (!is.null(imageUrl) && imageUrl == "") {
    imageUrl <- NULL
  }
  
  params <- as.list(environment())
  
  # Remove parameters that should not be passed to JavaScript
  params$session <- NULL
  params$closeOnEsc <- NULL
  params$className <- NULL
  params$closeOnClickOutside <- NULL
  params$confirmButtonCol <- NULL
  params$imageWidth <- NULL
  params$imageHeight <- NULL

  # Rename some parameters that shinyalert tries to use more sensible names for
  params$customClass <- paste0(className, " alert-size-", size)
  params$allowEscapeKey <- closeOnEsc
  params$allowOutsideClick <- closeOnClickOutside
  params$confirmButtonColor <- confirmButtonCol
  params$imageSize <- paste0(imageWidth, "x", imageHeight)
  params$inputId <- session$ns(inputId)
  
  # if the user data of the shiny session does not contain the
  # flag .shinyalert_added, or the flag is added, but is set FALSE
  # then insert this basic shinyalert UI element
  if (is.null(session$userData$.shinyalert_added) || 
      !session$userData$.shinyalert_added) {
    shiny::insertUI(
      selector  = "head", 
      where     = "beforeEnd",
      getDependencies(), 
      immediate = TRUE)
    session$userData$.shinyalert_added <- TRUE
  }

  if (isTRUE(immediate)) {
    closeAlert()
  }

  # generate a unique id for this shinyalert
  cbid <- paste0("__shinyalert-", gsub("-", "", uuid::UUIDgenerate()))
  params$cbid <- session$ns(cbid)
  
  # If an R callback function is provided, create an observer for it
  if (is.null(callbackR)) {
    params$callbackR <- FALSE
  } else if (is.function(callbackR)) {
    shiny::observeEvent(
      eventExpr   = session$input[[cbid]], 
      handlerExpr = {
        if (length(formals(callbackR)) == 0) {
          callbackR()
        } else {
          callbackR(session$input[[cbid]])
        }
      }, 
      once        = TRUE)
    params$callbackR <- TRUE
  } else {
    stop("Error in shinyalert: callbackR is not a function")
  }


  if (isTRUE(html)) {
    if (type == "input") {
      stop("Cannot use 'input' type and HTML together. ", 
           "You must supply your own Shiny inputs when using HTML.")
    }

    shiny::insertUI(
      selector  = "head", 
      where     = "beforeEnd",
      htmltools::tagList(
        htmltools::attachDependencies(
          "",
          htmltools::findDependencies(params$text)
        )
      ),
      immediate = FALSE
    )
    
    params$text <- as.character(params$text)
  }

  session$sendCustomMessage(type = "shinyalert.show", message = params)

  invisible(params$cbid)
}

#' Close a shinyalert popup message
#' @param num Number of popup messages to close. If set to 0 (default) then all
#' messages are closed. This is only useful if you have multiple popups queued up.
#' @param id To close a specific popup, use the ID returned by \code{\link{shinyalert}}.
#' Note that if `id` is specified, then `num` is ignored.
#' @export
closeAlert <- function(num = 0, id = NULL) 
{
  session <- getSession()
  session$sendCustomMessage(type = "shinyalert.closeAlert",
                            message = list(count = num, cbid = id))
}
