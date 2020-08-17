library(shiny)
library(rms)
library(ggplot2)
library(DALEX)
library(shinydashboard)
library(shiny.i18n)

theme_ema <- theme(text = element_text(color = "black", size = 12),
                   plot.title = element_text(color = "black", size = 14, hjust = 0), 
                   plot.subtitle = element_text(color = "black", hjust = 0), 
                   axis.text = element_text(color = "black", size = 12), 
                   axis.text.x = element_text(color = "black", size = 12), 
                   axis.text.y = element_text(color = "black", size = 12), 
                   axis.title = element_text(color = "black", size = 12), 
                   legend.text = element_text(color = "black", size = 12), 
                   strip.text = element_text(color = "black", size = 12, hjust = 0))

i18n <- Translator$new(translation_json_path = "translations/translation.json")
lrm_exp <- readRDS("lrm_exp_test.rds")
lrm_hosp <- readRDS("lrm_hosp_test.rds")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "Kalkulator ryzyka Covid-19",
                  dropdownMenu(type = "messages", badgeStatus = "success", headerText = "O autorach",
                               messageItem("MOCOS",
                                           "The MOCOS Group (MOdelling COronavirus Spread.",
                                           href = "http://mocos.pl/"
                               ),
                               messageItem("MI2DataLab",
                                           "MI² is a group of statisticians and data scientists.",
                                           href = "http://mi2.mini.pw.edu.pl/"
                               )
                  ),
                  
                  tags$li(class = "dropdown", 
                          radioButtons(inputId = "language",
                                       label = "",
                                       inline = TRUE,
                                       choices = c("Polski" = "pl", "English" = "en"),
                                       selected = "pl")
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Twoje ryzyko", tabName = "dashboard", icon = icon("dashboard")),
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
                  "Inne.Choroby",
                  c("Tak", "Nie"),
                  selected = "Nie"),
#      selectInput("infection",
#                  i18n$t("Miesiąc zakażenia"),
#                  c("March", "April", "May", "June"),
#                  selected = "June"),
      menuItem("O modelu CRS-19", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Twoje ryzyka", 
                    background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    uiOutput("textPred")),
                box(title = "Co wpływa na wyliczoną śmiertelność?", 
                    background = "yellow", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdPlot", height = 300)),
                box(title = "Jak śmiertelność zależy od wieku?", 
                    background = "yellow", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdCeteriaParibus", height = 300)),
                box(title = "Skuteczność modelu", 
                    background = "blue", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdROC", height = 300)),
                box(title = "Co wpływa na ryzyko hospitalizacji?", 
                    background = "green", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdPlotHosp", height = 300)),
                box(title = "Jak ryzyko hospitalizacji zależy od wieku?", 
                    background = "green", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    plotOutput("bdCeteriaParibusHosp", height = 300))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("O modelu"),
              "Więcej szczegółów o tym jak ten model działa i jak był zbudowany"
      )
    )
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    new_obs_reactive <- reactive({
      new_obs <- data.frame(#Miesiąc = factor(input$infection, c("March","April","May","June")),
                            Płeć = factor(input$sex, c("Kobieta", "Mężczyzna")),
                            Wiek = input$age,
                            Inne.Choroby = factor(input$comorbidities, c("Nie", "Tak")))
    })
  
  
    output$textPred <- renderUI({
      new_obs <- new_obs_reactive()
      pred_mort <- ceiling(predict(lrm_exp, new_obs)*1000)/10
      pred_hosp <- ceiling(predict(lrm_hosp, new_obs)*1000)/10

      div(
      "Ryzyka wyznaczone są dla osoby w wieku ",strong(input$age)," lat, płci ",strong(input$sex), ", ",
      strong(ifelse(input$comorbidities == "Tak", "z chorobami towarzyszącymi","bez chorób towarzyszących")),
      br(),
      br(),
      "W przypadku zachorowania ryzyko zgonu wynosi", strong(pred_mort), "%",
      br(),
      "W przypadku zachorowania ryzyko hospitalizacji wynosi", strong(pred_hosp), "%")
    })

    # i18n$set_translation_language(input$language)
    # plot
    output$bdPlot <- renderPlot({
      new_obs <- new_obs_reactive()
      
      pp <- predict_parts(lrm_exp, new_obs, order = c("Wiek", "Inne.Choroby", "Płeć", "Miesiąc"))
      plot(pp) + facet_null() + ggtitle("Wykres składowych Break-Down") +
        scale_y_continuous("Śmiertelność", limits = c(0,max(pp$cumulative)*1.2), labels = scales::percent) +
        theme_ema
    })
    
    # plot
    output$bdCeteriaParibus <- renderPlot({
      new_obs <- new_obs_reactive()
      
      pp <- predict_profile(lrm_exp, new_obs, "Wiek")
      plot(pp) + facet_null() + ggtitle("","") +
        scale_y_continuous("Śmiertelność", labels = scales::percent) +
        xlab("Wiek") +
        theme_ema
    })
    
    # i18n$set_translation_language(input$language)
    # plot
    output$bdPlotHosp <- renderPlot({
      new_obs <- new_obs_reactive()
      
      pp <- predict_parts(lrm_hosp, new_obs, order = c("Wiek", "Inne.Choroby", "Płeć", "Miesiąc"))
      plot(pp) + facet_null() + ggtitle("Wykres składowych Break-Down") +
        scale_y_continuous("Ryzyko hospitalizacji", limits = c(0,max(pp$cumulative)*1.2), labels = scales::percent) +
        theme_ema
    })
    
    # plot
    output$bdCeteriaParibusHosp <- renderPlot({
      new_obs <- new_obs_reactive()
      
      pp <- predict_profile(lrm_hosp, new_obs, "Wiek")
      plot(pp) + facet_null() + ggtitle("","") +
        scale_y_continuous("Ryzyko hospitalizacji", labels = scales::percent) +
        xlab("Wiek") +
        theme_ema
    })
    
    # plot
    output$bdROC <- renderPlot({
      lrm_exp$label = "Śmiertelność"
      lrm_hosp$label = "Hospitalizacja"
      
      pp1 <- model_performance(lrm_exp)
      pp2 <- model_performance(lrm_hosp)
      plot(pp1, pp2, geom = "roc") +  ggtitle("","") +
        theme_ema
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
