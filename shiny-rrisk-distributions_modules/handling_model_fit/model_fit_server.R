model_fit_server <- function(input, output, rrisk_dist_obj)
{
    observeEvent(
    eventExpr = list(input$select_dist, input$select_statistic),
    handlerExpr = {
      dist_name <- input$select_dist
      if (nzchar(dist_name)) {
        result <- rrisk_dist_obj()$get_fit_result(dist_name)
        stat_result <- switch(
          input$select_statistic,
          "loglike" = paste("avg. neg. log-liklihood = ", signif(result$mean_neg_loglike)),
          "aic"     = paste("aic risk = ", signif(result$aic)),
          "bic"     = paste("bic risk = ", signif(result$bic))
        )
        fit_result_info <- HTML(paste(
          paste(names(result$par), "=", signif(result$par, 4), collapse = "; "),
          "<br/>",
          stat_result)
        )
      } else {
        fit_result_info <- ""
      }
      output$info_text_for_fit_result <- renderUI({fit_result_info})
    }
  )
  
  observeEvent(
    eventExpr = input$select_statistic,
    handlerExpr = {
      available_dists <- rrisk_dist_obj()$get_fitted_dist_name_list(
        sorted_by = input$select_statistic
      )
      # update
      updateSelectInput(inputId  = "select_dist",
                        choices  = available_dists,
                        selected = available_dists[1])
    }
  )
  #---END: selecting fitted dist and statistic for ordering results-------------
  
  output$ecdf_plot <- renderPlot({
    rrisk_dist_obj()$plot_ecdf(input$select_dist)
  })
  
  output$pdf_plot <- renderPlot({
    rrisk_dist_obj()$plot_hist(input$select_dist)
  })
}