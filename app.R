library(shiny)
library(bs4Dash)
library(gapminder)
library(dplyr)
library(ggplot2)
library(bbplot)


year_range <- range(gapminder[["year"]])


ui <- bs4DashPage(
  header = bs4DashNavbar(disable = TRUE),
  
  controlbar = bs4DashControlbar(
    id = "controlbar",
    width = 250,
    overlay = FALSE,
    skin = "light",
    pinned = TRUE,
    collapsed = FALSE,
    
    controlbarMenu(
      id = "controlbarmenu",
      
      controlbarItem(
        title = "Filter:",
        
        selectInput("continent", "Select A Continent",
                    choices = unique(gapminder$continent)),
        
        selectInput("country", "Select A Country",
                    choices = NULL),

      )
    )
  ),
  
  sidebar = bs4DashSidebar(disable = T),
  
  body = bs4DashBody(
    fluidRow(
      box(
        height = "600px",
        actionButton("updateyear", "Update Year"),
        br(),
        br(),
        plotOutput("plot"),
        width = 12,
        sidebar = boxSidebar(
          id = "plotsidebar",
          width = 40,
          background = "#333a40",
          sliderInput("year",
                      "Select The Year Range:",
                      min = year_range[[1]],
                      max = year_range[[2]],
                      value = c(year_range[[1]], year_range[[2]]),
                      sep = "",
                      step = 1)
        )
      )
    )
  )
  
)


server <- function(input, output, session){
  
  continent_data <- reactive({
    filter(gapminder, continent == input$continent)
  })
  
  
  observeEvent(continent_data(), {
    freezeReactiveValue(input, "country")
    choices = unique(continent_data()$country)
    updateSelectInput(inputId = "country",
                      choices = choices)
  })
  
  
  
  country_data <- reactive({
    req(input$country)
    continent_data() %>%
      filter(country == input$country
             & year >= input$year[[1]] & year <= input$year[[2]])
  })
  
  observeEvent(input$updateyear,{
    updateBoxSidebar("plotsidebar")
  })
  
  
  output$plot <- renderPlot({
    req(input$country)
    ggplot(country_data(), aes(year, lifeExp)) +
      geom_line(colour = "#1380A1", size = 1) +
      geom_hline(yintercept = 0, size = 1, colour="#333333") +
      bbc_style() +
      labs(title="Humans Are Living longer",
           subtitle = paste0("Life expanctancy in ", 
                             input$country," ", input$year[[1]], "-", input$year[[2]]))
  }, res = 96)
}



shinyApp(ui = ui, server = server)