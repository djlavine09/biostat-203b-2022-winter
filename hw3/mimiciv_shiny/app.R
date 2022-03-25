library(shiny)
library(tidyverse)

icu <- read_rds("~/biostat-203b-2022-winter/hw3/mimiciv_shiny/icu_cohort.rds")
icu$thirty_day_mort_lab <- icu$thirty_day_mort
icu$thirty_day_mort_lab[is.na(icu$thirty_day_mort_lab)] <- 0
icu$thirty_day_mort_lab <- factor(icu$thirty_day_mort_lab,
                                  labels = 
                                    c("Did not die", "Died within 30 days"))
ui <- fluidPage(
  titlePanel("ICU Cohort Data Visualization Tool"),
  sidebarLayout(
  #Input: Selector for variable to view distribution (Lab Events)
  sidebarPanel(
    selectInput("varlab", 
                label = "Choose a laboratory measurement to display",
                choices =  c("Creatinine", 
                             "Potasium", 
                             "Chloride", 
                             "Bicarbonate", 
                             "Hematocrit", 
                             "White Blood Cell Count", 
                             "Glucose",
                             "Magnesium", 
                             "Calcium"),
                selected = "Creatinine"),
    checkboxInput("dead", "Seperate those who did and did not die within 
                  30 days of admission", 
                  value = FALSE),
    checkboxInput("outlier", "Exclude Outliers", 
                  value = FALSE)
  ),
  #Main panel for displaying outputs
  mainPanel(
    #Output: Plot of selected variable (histogram)
    plotOutput("labeventsPlot")
    
  )), 
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("varvit", 
                  label = "Choose a vital measurement to display",
                  choices =  c("Heart Rate", 
                               "Mean non-invasive blood pressure", 
                               "Systolic non-invasive blood pressure", 
                               "Body Tempurature in Fahrenheit", 
                               "Respiratory Rate"),
                  selected = "Heart"),
      checkboxInput("dead2", "Seperate those who did and did not die within
                    30 days of admission", 
                    value = FALSE),
      checkboxInput("outlier2", "Exclude Outliers", 
                    value = FALSE)
    ),
    mainPanel(
      plotOutput("vitalsPlot"),
      tableOutput("Tablevitals")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("dem", 
                  label = "Choose a demographic to view 30-day 
                  mortality summary",
                  choices = c("Ethnicity", "Language", "Insurance Type", 
                              "Marital Status", "Gender", 
                              "Age at Hospital Admission",
                              "First ICU Care Unit"),
                  selected = "Ethnicity")), 
    mainPanel(
      plotOutput("demoPlot"), 
      tableOutput("demoTab")
    )
  ),
  
  
  )
  




server <- function(input, output) {
  
  #Create histogram for lab measurements
output$labeventsPlot <- renderPlot({
  data_in <- switch(input$varlab, 
    "Creatinine" = icu$meas_50912, 
    "Potasium" = icu$meas_50971, 
    "Chloride" = icu$meas_50983, 
    "Bicarbonate" = icu$meas_50882, 
    "Hematocrit" = icu$meas_51221, 
    "White Blood Cell Count" = icu$meas_51301, 
    "Glucose" = icu$meas_50931,
    "Magnesium" = icu$meas_50960, 
    "Calcium" = icu$meas_50893)
 
  #hist(data_in, col = 'darkgray', border = 'white')
  if (input$dead == FALSE & input$outlier == FALSE){
  ggplot(data = icu, aes(x = data_in)) + 
      geom_histogram(color = "mediumpurple2", fill = "mediumpurple2") + 
      theme_minimal() +
    ggtitle(paste("Distribution of", input$varlab)) + xlab(input$varlab)}
  else if (input$dead== TRUE & input$outlier == FALSE) {
    ggplot(data = icu, aes(x = data_in)) + 
      geom_histogram(color = "mediumpurple2", fill = "mediumpurple2") + 
      theme_minimal() +
      ggtitle(paste("Distribution of", input$varlab)) + xlab(input$varlab) +
      facet_grid(. ~ thirty_day_mort_lab)}
  else if (input$dead== FALSE & input$outlier == TRUE) {
    ggplot(data = icu, aes(x = data_in)) + 
      geom_histogram(color = "mediumpurple2", fill = "mediumpurple2") + 
      theme_minimal() +
      ggtitle(paste("Distribution of", input$varlab)) + xlab(input$varlab) +
      xlim(c(
        (summary(data_in)[2]-1.5*(summary(data_in)[5] - summary(data_in)[2])),
        (summary(data_in)[5]+1.5*(summary(data_in)[5] - summary(data_in)[2]))))}
  else {ggplot(data = icu, aes(x = data_in)) + 
      geom_histogram(color = "mediumpurple2", fill = "mediumpurple2") + 
      theme_minimal() +
      ggtitle(paste("Distribution of", input$varlab)) + xlab(input$varlab) +
      xlim(c(
        (summary(data_in)[2] - 1.5*(summary(data_in)[5] - 
                                      summary(data_in)[2])), 
        (summary(data_in)[5] + 1.5*(summary(data_in)[5] - 
                                      summary(data_in)[2])))) +
      facet_grid(. ~ thirty_day_mort_lab)}
  })



output$vitalsPlot <- renderPlot({
  data_vit <- switch(input$varvit, 
                    "Heart Rate" = icu$meas_220045, 
                    "Mean non-invasive blood pressure" = icu$meas_220181, 
                    "Systolic non-invasive blood pressure" = icu$meas_220179, 
                    "Body Tempurature in Fahrenheit" = icu$meas_223761, 
                    "Respiratory Rate" = icu$meas_220210) 
  
  

  if (input$dead2 == FALSE & input$outlier2 == FALSE){
    ggplot(data = icu, aes(x = data_vit)) + 
      geom_histogram(color = "cornflowerblue", fill = "cornflowerblue") + 
      theme_minimal() +
      ggtitle(paste("Distribution of", input$varvit)) + xlab(input$varvit)}
  else if (input$dead2 == TRUE & input$outlier2 == FALSE) {
    ggplot(data = icu, aes(x = data_vit)) + 
      geom_histogram(color = "cornflowerblue", fill = "cornflowerblue") + 
      theme_minimal() +
      ggtitle(paste("Distribution of", input$varvit)) + xlab(input$varvit) +
      facet_grid(. ~ thirty_day_mort_lab)}
  else if (input$dead2 == FALSE & input$outlier2 == TRUE) {
    ggplot(data = icu, aes(x = data_vit)) + 
      geom_histogram(color = "cornflowerblue", fill = "cornflowerblue") + 
      theme_minimal() +
      ggtitle(paste("Distribution of", input$varvit)) + xlab(input$varvit) +
      xlim(c(
        (summary(data_vit)[2] - 1.5*(summary(data_vit)[5]
                                     - summary(data_vit)[2])),
        (summary(data_vit)[5] + 1.5*(summary(data_vit)[5] -
                                     summary(data_vit)[2]))))}
  else {
    ggplot(data = icu, aes(x = data_vit)) + 
      geom_histogram(color = "cornflowerblue", fill = "cornflowerblue") + 
      theme_minimal() +
      ggtitle(paste("Distribution of", input$varvit))+xlab(input$varvit) +
      xlim(c(
        (summary(data_vit)[2]-1.5*(summary(data_vit)[5] - 
                                     summary(data_vit)[2])),
        (summary(data_vit)[5]+1.5*(summary(data_vit)[5] -
                                     summary(data_vit)[2])))) +
      facet_grid(. ~ thirty_day_mort_lab)}
})



output$demoPlot <- renderPlot({
  data_dem <- switch(input$dem, 
                     "Ethnicity" = icu$ethnicity,
                     "Language" = icu$language,
                     "Insurance Type" = icu$insurance,
                     "Marital Status" = icu$marital_status,
                     "Gender" = icu$gender,
                     "Age at Hospital Admission" = icu$anchor_age,
                    )
  
  ggplot(data = icu, aes(x= data_dem, y = icu$thirty_day_mort)) + 
    geom_col(color = "lightpink1") +
    theme_minimal() + ggtitle(paste("Thirty Day Mortality by", input$dem)) +
    xlab(input$dem) + 
    ylab("Count of deaths that occured within 30 days of admission")
  })


}



shinyApp(ui, server)
