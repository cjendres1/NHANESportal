library(shiny)
shinyUI(pageWithSidebar(
  
  headerPanel(h3("National Health and Nutrition Examination Survey Access Portal")),
  
  sidebarPanel(
    fluidRow(
      h4("Instructions"),
      h6("1: Below, Select desired year"),
      h6("2: Click on 'Show columns'"),
      h6("3: Select desired column"),
      h6("4: Click 'Retrieve demographic data'"),
      h6("5: For clarity, select 'Translate code'"),
      h6("6: Finally, click 'Generate Summary'"),
      br(),
      selectInput("dataset", label = h4("Select NHANES data set"), choices = list(' '), selected = ' '),
      ##This action button will initiate downloading of data
      actionButton(inputId = "getdemographicdata", label = "Show columns", value = '' ),
      conditionalPanel(condition = "input.getdemographicdata > 0",
      br(),
      radioButtons("choose_columns", label = "Select Column",
                    choices = list('')),
      br(),
      actionButton(inputId = "previewcolumn", label = "Retrieve demographic data"),
      
#      br(),
      #br(),
     conditionalPanel(condition = "input.choose_columns != 0 & input.choose_columns != 'age'",
                      br(),
                 checkboxInput(inputId="translatecode", label = "Translate code", value=FALSE)
                 )
     #,
#      textOutput("setColumn")#,

#    br(),
#    a(href = "https://gist.github.com/4211337", "Source code")
      ))),#),
  
  
  mainPanel(
    fluidRow(
      column(5,
    uiOutput("echo_request"),
    br(),
    tableOutput("column_data")
      ),
    column(7,
           conditionalPanel(condition = "input.previewcolumn > 0",
           actionButton(inputId = "showtable", label = "Generate Summary"),
           conditionalPanel(condition = "input.showtable > 0",
                            br(),
                            uiOutput("summary_request"),
                            tableOutput("column_summary"),
                            uiOutput("compute_totals")
                            )
           ))
    )
  )
))
