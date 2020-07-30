library(shiny)
library(r2d3)

# path <- "Zejscie_data_Adam.xlsx"
path <- "~/Dropbox/__smiertelnosc_co_id__/dane_2020_07_01/Zejscie_data.xlsx"
covid <- data.frame(readxl::read_xlsx(path))
new_obs <- data.frame(Age=50, Sex="Male", Comorbidities="Yes") 

ui <- fluidPage(
  
  titlePanel("Covid-19 Death Rate"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("sex", h3("Sex:"),
                   c("Male",
                     "Female")),
      radioButtons("comorbidities", h3("Comorbidities:"),
                   c("Yes",
                     "No")),
      numericInput("age", h3("Age:"), 50, min = 0, max = 125),
      actionButton("calculate", "Calculate")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction", textOutput('prediction')),
        tabPanel("Explanations", htmlOutput('dashboard'))
      )
    )
  )
)


server <- function(input, output, session) {
  #:# id of div where modelStudio will appear
  WIDGET_ID = 'MODELSTUDIO'
  
  library(modelStudio)
  library(DALEX)
  library(dplyr)
  library(rms)
  library(scales)
  Comorbidities <- ifelse(covid$Choroby.współwystępujące=="Tak", "Yes", "No")
  covid_mod <- covid %>% select(Zejście.choroby, Wiek, Płeć, Choroby.współwystępujące) %>%
    transmute(Survived = ifelse(grepl(Zejście.choroby, pattern = "zgon", ignore.case = TRUE) & !is.na(Zejście.choroby),0,1),
              Age=Wiek,
              Sex=ifelse(Płeć=="Mężczyzna","Male","Female"),
              Comorbidities2=ifelse(Choroby.współwystępujące=="Tak", "Yes", "No"),
              Comorbidities = ifelse(is.na(Comorbidities2), "No info", Comorbidities)) %>%
    select(-Comorbidities2)
  
  
  
  Age <- covid_mod$Age
  Sex <- covid_mod$Sex
  Comorbidities <- covid_mod$Comorbidities
  
  ddist <- datadist(Age, Sex, Comorbidities)
  options(ddist)
  
  #model <- lrm(Survived ~ rcs(Age) * Sex * Comorbidities, data = covid_mod)
  
  model <- lrm(Survived ~ rcs(Age) + Sex + Comorbidities, data = covid_mod)
  
  exp <- DALEX::explain(model, select(covid_mod, -Survived), covid_mod$Survived)
  
  ms <- modelStudio(exp,
                    widget_id = WIDGET_ID)
 
  create_ms <- eventReactive(c(input$calculate),{
    new_observation <- new_obs
    new_observation$Age = input$age
    new_observation$Sex = input$sex
    new_observation$Comorbidities = input$comorbidities
    ms <- modelStudio(exp,
                      new_observation = new_observation,
                      widget_id = WIDGET_ID)
    ms

  })
  
  predict_it <- eventReactive(c(input$calculate),{
    new_observation <- new_obs
    new_observation$Age = input$age
    new_observation$Sex = input$sex
    new_observation$Comorbidities = input$comorbidities
    prediction <- paste("\n The chance of surviving the COVID-19 disease for the specified person equals: ", percent(predict(exp, new_observation), accuracy = 0.01))
    
  })
  

  #:# basic render d3 output
  output[[WIDGET_ID]] <- renderD3({
    create_ms()
  })
  
  output$prediction <- renderText({ 
    predict_it()
  })
  
  #:# use render ui to set proper width and height
  output$dashboard <- renderUI({
    d3Output(WIDGET_ID, width=ms$width, height=ms$height)
  })
}

shinyApp(ui = ui, server = server)
