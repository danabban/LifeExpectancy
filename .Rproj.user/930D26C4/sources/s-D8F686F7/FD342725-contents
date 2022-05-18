library(shiny)
library(gapminder)
library(bbplot)

year_range <- range(gapminder[["year"]])


ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "cerulean"),
    
    
      titlePanel(
          h2("A visual Representation of Life Expectancy", align = "center")
      ),
    
      br(),

      fluidRow(
        column(3, 
               selectInput("continent", "Select A Continent",
                           choices = unique(gapminder$continent))
        ),
        
        column(3, offset = 1,
               selectInput("country", "Select A Country",
                           choices = NULL),
        ),
        
        column(3, offset = 1,
               sliderInput("year",
                           "Select The Year Range:",
                           min = year_range[[1]],
                           max = year_range[[2]],
                           value = c(year_range[[1]], year_range[[2]]),
                           sep = "",
                           step = 1)
        )
      ),
    
      br(),


       fluidRow(
         column(11,
                plotOutput("plot")
         )
       )

    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
      thematic::thematic_shiny()
  
      continent_data <- reactive({
          gapminder %>%
              filter(continent == input$continent
                     & year >= input$year[[1]] | year <= input$year[[2]])
        })
      
      
      observeEvent(continent_data(), {
              freezeReactiveValue(input, "country")
              choices <- unique(continent_data()$country)
              updateSelectInput(session, "country", choices = choices)
      })
      
      country_data <- reactive({
            req(input$continent)
            continent_data() %>%
                filter(country == input$country
                       & year >= input$year[[1]] & year <= input$year[[2]])
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

# Run the application 
shinyApp(ui = ui, server = server)
