library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  # choose columns to display
  diamonds2 = testdata
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
  })
  
})