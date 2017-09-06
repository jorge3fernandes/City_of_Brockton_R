library(shiny)
library(ggplot2)  # for the testdata dataset

shinyUI(fluidPage(
  title = 'Brockton 911 Call Logs',
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "testdata"',
        checkboxGroupInput('show_vars', 'Columns in testdata to show:',
                           names(testdata), selected = names(testdata))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('testdata', DT::dataTableOutput('mytable1'))
      )
    )
  )
))