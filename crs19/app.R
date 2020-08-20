library(shiny)
library(rms)
library(ggplot2)
library(DALEX)
library(shinydashboard)
library(shiny.i18n)
library(shinycssloaders)
library(shinyjs)
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


translator <- Translator$new(translation_csvs_path = "translations/")

lrm_exp <- readRDS("lrm_exp_test.rds")
lrm_hosp <- readRDS("lrm_hosp_test.rds")

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = p(id = "title", "Kalkulator ryzyka Covid-19"),
                  dropdownMenuOutput("messageMenu"),
                  tags$li(class = "dropdown", 
                          radioButtons(inputId = "language",
                                       label = "",
                                       inline = TRUE,
                                       choices = c("Polski" = "pl", "English" = "en"),
                                       selected = "pl")
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem(#textOutput("menu_name1"),
               p(id = "menu1", "Twoje ryzyko"),
               tabName = "dashboard", icon = icon("dashboard")),
      sliderInput("age",
                  "Wiek",
                  min = 1,
                  max = 100,
                  value = 30),
      selectInput("sex",
                  "Płeć",
                  c("Mężczyzna", "Kobieta"),
                  selected = "Mężczyzna"),
      selectInput("comorbidities",
                  "Inne choroby",
                  c("Tak", "Nie"),
                  selected = "Nie"),
      menuItem(p(id = "menu2", "O modelu CRS-19"), tabName = "widgets", icon = icon("th")),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = p(id = "tab1", "Twoje ryzyka"),
                    background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    uiOutput("textPred") %>% withSpinner(hide.ui = FALSE)),
                box(title = p(id = "tab2", "Co wpływa na wyliczoną śmiertelność?"),
                    background = "yellow", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdPlot", height = 300) %>% withSpinner(hide.ui = FALSE)),
                box(title = p(id = "tab3", "Jak śmiertelność zależy od wieku?"),
                    background = "yellow", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdCeteriaParibus", height = 300) %>% withSpinner(hide.ui = FALSE)),
                box(title = p(id = "tab4", "Skuteczność modelu"),
                    background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdROC", height = 300) %>% withSpinner(hide.ui = FALSE)),
                box(title = p(id = "tab5", "Co wpływa na ryzyko hospitalizacji?"),
                    background = "green", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdPlotHosp", height = 300)  %>% withSpinner(hide.ui = FALSE)),
                box(title = p(id = "tab6", "Jak ryzyko hospitalizacji zależy od wieku?"),
                    background = "green", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdCeteriaParibusHosp", height = 300) %>% withSpinner(hide.ui = FALSE))
              )
      ),

      # Second tab content
      tabItem(tabName = "widgets",
              p(id = "widget1", "O modelu"),
              p(id = "widget2", "Więcej szczegółów o tym jak ten model działa i jak był zbudowany")
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

    observeEvent(input$language, {
      updateSelectInput(session, "sex", i18n()$t("Sex"),
                        c("Mężczyzna", "Kobieta") %>% stats::setNames(c(i18n()$t('Male'), i18n()$t('Female'))),
                        selected = "Mężczyzna")
        
      updateSelectInput(session, "comorbidities", i18n()$t("Comorbidities"),
                        c("Tak", "Nie") %>% stats::setNames(c(i18n()$t('Yes'), i18n()$t('No'))),
                        selected = "Nie")
      updateSliderInput(session, "age", i18n()$t("Age"))
      html("title", i18n()$t("Covid-19 risk calculator"))
      html("menu1", i18n()$t("Your risk"))
      html("menu2", i18n()$t("About the CRS-19 model"))
      html("widget1", i18n()$t("About the model"))
      html("widget2", i18n()$t("More details about how the model works and how it was created"))
      html("tab1", i18n()$t("Your risks"))
      html("tab2", i18n()$t("What influences the calculated mortality?"))
      html("tab3", i18n()$t("How does mortality depend on age?"))
      html("tab4", i18n()$t("Model performance"))
      html("tab5", i18n()$t("What influences the hospitalization risk?"))
      html("tab6", i18n()$t("How does the risk of hospitalization depend on age?"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
