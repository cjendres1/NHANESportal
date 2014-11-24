#data_sets <- c("mtcars", "morley", "rock")
#data_sets <- c("2001-2002", "2003-2004", "2005-2006", "2007-2008", "2009-2010", "2011-2012")
#names(data_sets) <- c("B", "C", "D", "E", "F", "G")
library(shiny)
library(SASxport)
DISPLAY_LIMIT = 25

data_sets <- c("B", "C", "D", "E", "F", "G")
names(data_sets) <- c("2001-2002", "2003-2004", "2005-2006", "2007-2008", "2009-2010", "2011-2012")
nhanesURL <- 'http://wwwn.cdc.gov/nchs/nhanes/'

shinyServer(function(input, output, session) {
  
getDataURL <- function() {
  if(input$dataset == 0) {
    return("Nothing to see here")
  }
  else {
    return(paste(c(nhanesURL,names(data_sets[data_sets == input$dataset]),'/DEMO_',input$dataset,'.XPT'),collapse=''))
  }
}

output$dataURL <- renderText({
  if(input$getdemographicdata > 0) {
    return(getDataURL())
  }
})

output$echo_request <- renderUI({
  if( (input$previewcolumn != 0) & (input$choose_columns != 0) ) {
    str1 <- paste(c("You selected the ", input$choose_columns, " column") ,collapse='') 
    str2 <- paste(c("Displaying the first ", DISPLAY_LIMIT, " values"), collapse='')
    HTML(paste(str1, str2, sep = '<br/>'))
  }
})

output$summary_request <- renderUI({
  if( (input$showtable != 0) & (input$choose_columns != 0) ) {
    isolate(HTML(paste(c("Summary of the ", input$choose_columns, " column") ,collapse=''))) 
  }
})

demographicFields <- list()
demographicFields[['RIAGENDR']] <- 'Gender'
demographicFields[['RIDAGEYR']] <- 'Age'
demographicFields[['RIDRETH1']] <- 'Ethnicity'
demographicFields[['DMDMARTL']] <- 'Marital Status'

## Not used currently
columnDescriptions <- list()
columnDescriptions[['RIAGENDR']] <- "Gender of the sample person"
columnDescriptions[['RIDAGEYR']] <- "Age at time of HH screening"
columnDescriptions[['RIDRETH1']] <- "Race and/or ethnicity info"
columnDescriptions[['DMDMARTL']] <- "Marital Status"

genderFields <- list()
genderFields[['1']] <- 'Male'
genderFields[['2']] <- 'Female'
genderFields[['default']] <- 'Missing'

ethnicFields <- list()
ethnicFields[['1']] <- 'Mexican American'
ethnicFields[['2']] <- 'Other Hispanic'
ethnicFields[['3']] <- 'Non-Hispanic White'
ethnicFields[['4']] <- 'Non-Hispanic Black'
ethnicFields[['5']] <- 'Other Race/Multiracial'
ethnicFields[['default']] <- 'Missing'

maritalFields <- list()
maritalFields[['1']] <- 'Married'
maritalFields[['2']] <- 'Widowed'
maritalFields[['3']] <- 'Divorced'
maritalFields[['4']] <- 'Separated'
maritalFields[['5']] <- 'Never Married'
maritalFields[['6']] <- 'Living With Partner'
maritalFields[['77']] <- 'Refused'
maritalFields[['99']] <- "Don't Know"
maritalFields[['default']] <- 'Missing'

constructDemoList <- function() {
#  print(names(demographicFields))
  return( names(demographicFields) )
}

observe({
if(input$getdemographicdata > 0) {
#  colfields <- constructDemoList()
  updateRadioButtons(session=session, inputId = "choose_columns",
  choices = c('Gender' = 'gender',
              'Age' = 'age',
              'Ethnicity' = 'ethnic',
              'Marital status' = 'marital'))
}
})

observe({
  updateSelectInput(session, "dataset", choices = as.list(data_sets))
})

setColumn <- function() {
  if( input$getdemographicdata != 0 ) {
    switch(isolate(input$choose_columns),
           'gender' =  {column = 'RIAGENDR'},
           'age' =     {column = 'RIDAGEYR'},
           'ethnic' =  {column = 'RIDRETH1'},
           'marital' = {column = 'DMDMARTL'}
    )
    return(column)
  }
  else {
    return('')
  }  
}

demodata <- reactive({
  if(input$getdemographicdata != 0) {
#    if(input$previewcolumn != 0) {
    withProgress(message = 'Retrieving data. \nPlease be patient ....', value = 0, {
      return(read.xport(getDataURL()))
    })    
  }
})

recodeddata <- reactive({  
  recoded <- demodata()[,constructDemoList()]
  recoded[['RIAGENDR']] <- as.factor(recoded[['RIAGENDR']])
  levels(recoded[['RIAGENDR']])[levels(recoded[['RIAGENDR']])=="1"] <- "Male"
  levels(recoded[['RIAGENDR']])[levels(recoded[['RIAGENDR']])=="2"] <- "Female"
  
  recoded[['RIDRETH1']] <- as.factor(recoded[['RIDRETH1']])
  levels(recoded[['RIDRETH1']])[levels(recoded[['RIDRETH1']])=="1"] <- "Mexican American"
  levels(recoded[['RIDRETH1']])[levels(recoded[['RIDRETH1']])=="2"] <- "Other Hispanic"
  levels(recoded[['RIDRETH1']])[levels(recoded[['RIDRETH1']])=="3"] <- "Non-Hispanic White"
  levels(recoded[['RIDRETH1']])[levels(recoded[['RIDRETH1']])=="4"] <- "Non-Hispanic Black"
  levels(recoded[['RIDRETH1']])[levels(recoded[['RIDRETH1']])=="5"] <- "Other Race/Multi-Racial"
  
  recoded[['DMDMARTL']] <- as.factor(recoded[['DMDMARTL']])  
  levels(recoded[['DMDMARTL']])[levels(recoded[['DMDMARTL']])=="1"]  <- "Married"
  levels(recoded[['DMDMARTL']])[levels(recoded[['DMDMARTL']])=="2"]  <- "Widowed"
  levels(recoded[['DMDMARTL']])[levels(recoded[['DMDMARTL']])=="3"]  <- "Divorced"
  levels(recoded[['DMDMARTL']])[levels(recoded[['DMDMARTL']])=="4"]  <- "Separated"
  levels(recoded[['DMDMARTL']])[levels(recoded[['DMDMARTL']])=="5"]  <- "Never Married"
  levels(recoded[['DMDMARTL']])[levels(recoded[['DMDMARTL']])=="6"]  <- "Living With Partner"
  levels(recoded[['DMDMARTL']])[levels(recoded[['DMDMARTL']])=="77"] <- "Refused"
  levels(recoded[['DMDMARTL']])[levels(recoded[['DMDMARTL']])=="99"] <- "Don't Know"
  
  return(as.data.frame(recoded))
})

output$compute_totals <- renderUI({
  input$choose_columns
  if(input$showtable != 0) {
    partialsum <- sum(table(demodata()[[setColumn()]]))
    fullsum <- sum(table(demodata()[[setColumn()]], useNA = 'ifany'))
    missing <- fullsum - partialsum
    str1 <- paste(c("There are ", partialsum, " subjects with data"), collapse='')
    str2 <- paste(c("There are ", missing, " subjects missing data"), collapse='')
    HTML(paste(str1, str2, sep = '<br/>'))    
  }
})

output$column_summary <- renderTable({
  if(input$showtable == 0) {return()}
  else {
    if(input$translatecode == TRUE) {
      column_data <- as.data.frame(table(recodeddata()[[setColumn()]])) #, useNA = 'ifany')
    }
    else {
      column_data <- as.data.frame(table(demodata()[[setColumn()]])) #, useNA = 'ifany')
    }
    names(column_data) <- c(capitalize(input$choose_columns), 'Total') 
    column_data
  }
}, include.rownames = FALSE)

##----------------------
## Update table display
output$column_data <- renderTable({
  input$choose_columns
  if (input$previewcolumn == 0) {return()}
  else {
    column <- setColumn()
#    print(column)
    if(input$translatecode == TRUE) {
      column_data <- as.data.frame(as.character(recodeddata()[[column]][1:DISPLAY_LIMIT]))
    }
    else {
      column_data <- as.data.frame(as.character(demodata()[[column]][1:DISPLAY_LIMIT]))
    }
#    print(names(demodata()))
    names(column_data) <- isolate(capitalize(input$choose_columns))
    column_data
  }
})

}) 