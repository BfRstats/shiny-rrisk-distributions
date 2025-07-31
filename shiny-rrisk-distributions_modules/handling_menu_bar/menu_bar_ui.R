# UI
menu_bar_ui <- function(version)
{
  navbarPage(
    title       = paste("shiny rrisk distributions", version),
    id          = "menu_bar",
    selected    = "1",
    position    = "fixed-top",
    collapsible = TRUE,
    bslib::nav_panel(title = "", value = "1"),
    bslib::nav_panel(title = "NEW", value = "NEW"),
    bslib::nav_menu(
      title = "OPEN ...",
      bslib::nav_item(
        fileInput(inputId  = "open_csv_file", 
                  label    = "... csv file",
                  multiple = FALSE,
                  accept   = c("text/csv", ".csv"))
      ),
      bslib::nav_item(
        fileInput(inputId  = "open_rrisk_dist_file", 
                  label    = "... rrisk-dist file",
                  multiple = FALSE,
                  accept   = c("application/json", ".rriskdist"))
      )
    ),
    bslib::nav_menu(
      title = "DOWNLOAD ...",
      bslib::nav_panel(
        title = downloadLink(outputId = "download_project", 
                             label    = "... PROJECT",
                             style    = "display:inline"),
        value = "SAVE"
      ),
      bslib::nav_panel(
         title = downloadLink(outputId = "download_report", 
                              label    = "... REPORT",
                              style    = "display:inline"),
         value = "REPORT"
      ),
      bslib::nav_panel(
         title = downloadLink(outputId = "download_export_to_shiny_rrisk", 
                              label    = "... EXPORT current fit for shiny rrisk",
                              style    = "display:inline"),
         value = "REPORT"
      )
    ),
    bslib::nav_panel(
      title = "Disclaimer, Manual, and Contact",
      value = "DISCLAIMER"
    )
  )
}