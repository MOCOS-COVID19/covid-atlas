library(shiny)
library(rms)
library(ggplot2)
library(DALEX)
library(shinydashboard)
library(shiny.i18n)
library(dplyr)

theme_ema <- theme(text = element_text(color = "black", size = 12),
                   plot.title = element_text(color = "black", size = 14, hjust = 0), 
                   plot.subtitle = element_text(color = "black", hjust = 0), 
                   axis.text = element_text(color = "black", size = 12), 
                   axis.text.x = element_text(color = "black", size = 12), 
                   axis.text.y = element_text(color = "black", size = 12), 
                   axis.title = element_text(color = "black", size = 12), 
                   legend.text = element_text(color = "black", size = 12), 
                   strip.text = element_text(color = "black", size = 12, hjust = 0))

#i18n <- Translator$new(translation_json_path = "translations/translation.json")

translator <- Translator$new(translation_csvs_path = "translations/")

lrm_exp <- readRDS("lrm_exp_test.rds")
lrm_hosp <- readRDS("lrm_hosp_test.rds")

# Define UI for application that draws a histogram


ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = textOutput("title"),
                  dropdownMenuOutput("messageMenu"),
                  tags$li(class = "dropdown", 
                          radioButtons(inputId = "language",
                                       label = "",
                                       inline = TRUE,
                                       choices = c("Polski" = "pl", "English" = "en"),
                                       selected = "en")
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem(textOutput("menu_name1"), tabName = "dashboard", icon = icon("dashboard")),
      sliderInput("age",
                  textOutput("input_name1"),
                  min = 1,
                  max = 100,
                  value = 30),
      uiOutput("input_sex"),
      uiOutput("input_comorbidities"),
      # selectInput("sex",
      #             textOutput("input_name2"),
      #             c("Mężczyzna", "Kobieta"),
      #             selected = "Mężczyzna"),
      # selectInput("comorbidities",
      #             textOutput("input_name3"),
      #             c("Yes"="Tak", "No"="Nie"),
      #             selected = "Nie"),
#      selectInput("infection",
#                  i18n$t("Miesiąc zakażenia"),
#                  c("March", "April", "May", "June"),
#                  selected = "June"),
      menuItem(textOutput("menu_name2"), tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = textOutput("tab_title1"),
                    background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    uiOutput("textPred")),
                box(title = textOutput("tab_title2"),
                    background = "yellow", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdPlot", height = 300)),
                box(title = textOutput("tab_title3"),
                    background = "yellow", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdCeteriaParibus", height = 300)),
                box(title = textOutput("tab_title4"),
                    background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdROC", height = 300)),
                box(title = textOutput("tab_title5"),
                    background = "green", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdPlotHosp", height = 300)),
                box(title = textOutput("tab_title6"),
                    background = "green", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdCeteriaParibusHosp", height = 300))
              )
      ),

      # Second tab content
      tabItem(tabName = "widgets",
              h2(textOutput("widget_title")),
              textOutput("widget_info")
      )
    )

  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    i18n <- reactive({
      selected <- input$language
      if (length(selected) > 0 && selected %in% translator$languages) {
        translator$set_translation_language(selected)
      }
      translator
    })  
    
    new_obs_reactive <- reactive({
      new_obs <- data.frame('Płeć' = factor(input$sex, c("Kobieta", "Mężczyzna")),
                            'Wiek' = input$age,
                            'Inne.Choroby' = factor(input$comorbidities, c("Nie", "Tak")))
    })
    
    output$textPred <- renderUI({
      new_obs <- new_obs_reactive()
      pred_mort <- ceiling(predict(lrm_exp, new_obs)*1000)/10
      pred_hosp <- ceiling(predict(lrm_hosp, new_obs)*1000)/10

      div(
      i18n()$t("Riscs are calculated for a "),strong(input$age),i18n()$t(" year old "),
      strong(ifelse(input$sex == "Mężczyzna", i18n()$t("male"),i18n()$t("female"))), ", ",
      strong(ifelse(input$comorbidities == "Tak", i18n()$t("with comorbidities"),i18n()$t("without comorbidities"))),
      br(),
      br(),
      i18n()$t("In case of infection"), i18n()$t("the death risk equals"), strong(pred_mort), "%",
      br(),
      i18n()$t("In case of infection"), i18n()$t("the hospitalization risk equals"), strong(pred_hosp), "%")
    })

    # i18n$set_translation_language(input$language)
    # plot
    output$bdPlot <- renderPlot({
      new_obs <- new_obs_reactive()
      
      choiceVec1 <- c("Tak", "Nie") %>% stats::setNames(c(i18n()$t('Yes'), i18n()$t('No')))
      choiceVec2 <- c("Mężczyzna", "Kobieta") %>% stats::setNames(c(i18n()$t('Male'), i18n()$t('Female')))
      
      pp <- predict_parts(lrm_exp, new_obs, order = c("Wiek", "Inne.Choroby", "Płec", "Miesiąc"))

      pp$variable <- c(i18n()$t("Intercept"),
                       paste0(i18n()$t("Age"), " = ", input$age),
                       paste0(i18n()$t("Comorbidities"), " = ", names(choiceVec1)[choiceVec1 == input$comorbidities]),
                       paste0(i18n()$t("Sex"), " = ", names(choiceVec2)[choiceVec2 == input$sex]),
                       i18n()$t("Prediction"))
      
      plot(pp) + facet_null() + ggtitle(i18n()$t("Variable attribution Break-Down plot")) +
        scale_y_continuous(i18n()$t("Mortality"), limits = c(0,max(pp$cumulative)*1.2), labels = scales::percent) +
        theme_ema
    })
    
    # plot
    output$bdCeteriaParibus <- renderPlot({
      new_obs <- new_obs_reactive()
      
      pp <- predict_profile(lrm_exp, new_obs, "Wiek")
      plot(pp) + facet_null() + ggtitle("","") +
        scale_y_continuous(i18n()$t("Mortality"), labels = scales::percent) +
        xlab(i18n()$t("Age")) +
        theme_ema
    })
    
    # i18n$set_translation_language(input$language)
    # plot
    output$bdPlotHosp <- renderPlot({
      new_obs <- new_obs_reactive()
      
      choiceVec1 <- c("Tak", "Nie") %>% stats::setNames(c(i18n()$t('Yes'), i18n()$t('No')))
      choiceVec2 <- c("Mężczyzna", "Kobieta") %>% stats::setNames(c(i18n()$t('Male'), i18n()$t('Female')))
      
      pp <- predict_parts(lrm_hosp, new_obs, order = c("Wiek", "Inne.Choroby", "Płec", "Miesiąc"))
      
      pp$variable <- c(i18n()$t("Intercept"),
                       paste0(i18n()$t("Age"), " = ", input$age),
                       paste0(i18n()$t("Comorbidities"), " = ", names(choiceVec1)[choiceVec1 == input$comorbidities]),
                       paste0(i18n()$t("Sex"), " = ", names(choiceVec2)[choiceVec2 == input$sex]),
                       i18n()$t("Prediction"))
      
      plot(pp) + facet_null() + ggtitle(i18n()$t("Variable attribution Break-Down plot")) +
        scale_y_continuous(i18n()$t("Risk of hospitalization"), limits = c(0,max(pp$cumulative)*1.2), labels = scales::percent) +
        theme_ema
    })
    
    # plot
    output$bdCeteriaParibusHosp <- renderPlot({
      new_obs <- new_obs_reactive()
      
      pp <- predict_profile(lrm_hosp, new_obs, "Wiek")
      plot(pp) + facet_null() + ggtitle("","") +
        scale_y_continuous(i18n()$t("Risk of hospitalization"), labels = scales::percent) +
        xlab(i18n()$t("Age")) +
        theme_ema
    })
    
    # plot
    output$bdROC <- renderPlot({
      lrm_exp$label = i18n()$t("Mortality")
      lrm_hosp$label = i18n()$t("Hospitalization")
      
      pp1 <- model_performance(lrm_exp)
      pp2 <- model_performance(lrm_hosp)
      plot(pp1, pp2, geom = "roc") +  ggtitle("","") +
        theme_ema
    })
    
    output$messageMenu <- renderMenu({
      dropdownMenu(type = "messages", badgeStatus = "success", headerText = i18n()$t("About authors"),
                   messageItem("MOCOS",
                               "The MOCOS Group (MOdelling COronavirus Spread).",
                               href = "http://mocos.pl/"
                   ),
                   messageItem("MI2DataLab",
                               i18n()$t("MI² is a group of statisticians and data scientists."),
                               href = "http://mi2.mini.pw.edu.pl/"
                   ))
    })
    
    output$title <- renderText({
      i18n()$t("Covid-19 risk calculator")
    })
    
    output$tab_title1 <- renderText({
      i18n()$t("Your risks")
    })
    
    output$tab_title2 <- renderText({
      i18n()$t("What influences the calculated mortality?")
    })
    
    output$tab_title3 <- renderText({
      i18n()$t("How does mortality depend on age?")
    })
    
    output$tab_title4 <- renderText({
      i18n()$t("Model performance")
    })
    
    output$tab_title5 <- renderText({
      i18n()$t("What influences the hospitalization risk?")
    })
    
    output$tab_title6 <- renderText({
      i18n()$t("How does the risk of hospitalization depend on age?")
    })
    
    output$widget_title <- renderText({
      i18n()$t("About the model")
    })
    
    output$widget_info <- renderText({
      i18n()$t("More details about how the model works and how it was created")
    })
    
    output$menu_name1 <- renderText({
      i18n()$t("Your risk")
    })
    
    output$menu_name2 <- renderText({
      i18n()$t("About the CRS-19 model")
    })
    
    output$input_name1 <- renderText({
      i18n()$t("Age")
    })
    
    output$input_name2 <- renderText({
      i18n()$t("Sex")
    })
    
    output$input_name3 <- renderText({
      i18n()$t("Comorbidities")
    })
    
    output$input_sex <- renderUI({
      selectInput("sex",
                  textOutput("input_name2"),
                  c("Mężczyzna", "Kobieta") %>% stats::setNames(c(i18n()$t('Male'), i18n()$t('Female'))),
                  selected = "Mężczyzna")
      
    })
    
    output$input_comorbidities <- renderUI({
      selectInput("comorbidities",
                  textOutput("input_name3"),
                  c("Tak", "Nie") %>% stats::setNames(c(i18n()$t('Yes'), i18n()$t('No'))),
                  selected = "Nie")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
