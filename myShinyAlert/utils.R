getSession <- function() 
{
  session <- shiny::getDefaultReactiveDomain()

  if (is.null(session)) {
    stop("Could not find a Shiny session object")
  }

  session
}

getDependencies <- function() 
{
  shinyalert_assets <- "myShinyAlert/www"
  
  #print(file.path(shinyalert_assets, "srcjs", "shinyalert.js"))
  #print(file.exists(shinyalert_assets, "srcjs", "shinyalert.js"))

  htmltools::singleton(
    htmltools::tags$head(
      htmltools::includeScript(
        path = file.path(shinyalert_assets, "shared", "sweetalert-1.0.1",
                        "js", "sweetalert.min.js")
      ),
      htmltools::includeCSS(
        path = file.path(shinyalert_assets, "shared", "sweetalert-1.0.1",
                         "css", "sweetalert.min.css")
      ),
      htmltools::includeScript(
        path = file.path(shinyalert_assets, "shared", "swalservice",
                        "swalservice.js")
      ),
      htmltools::includeScript(
        path = file.path(shinyalert_assets, "srcjs", "shinyalert.js")
      ),
      htmltools::includeCSS(
        path = file.path(shinyalert_assets, "css", "shinyalert.css")
      )
    )
  )
}
