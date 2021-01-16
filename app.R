
# libs
library(shiny)
library(shinythemes)
library(tidyverse)
library(tidyquant)


# setup
t_cost <- read_csv("scripts/data/transit_cost.csv")


# -------------------------------------------------------#
# -------------------------------------------------------#


ui <- fluidPage(
  title = "Shiny Tuesday 2021-01-05",
  theme = shinytheme("cyborg"),
  fluidRow(
    column(width = 12,
           align = 'center',
           br(),
           h4("Connect a Plot to a Table with a Brush")
    )
    
  ),
  hr(),
  fluidRow(
    column(width = 2,
           column(width = 12,
                  style = "background-color: #fdbf6f",
                  br(),
                  br(),
                  sliderInput("nmbr_start_year",
                              "Miminum Start Year",
                              min = 1982,
                              max = 2025,
                              value = 2015,
                              sep = ""
                  )
           )
    ),
    column(width = 10,
           fluidRow(
             column(width = 12,
                    align = 'center',
                    tabsetPanel(
                      tabPanel('Plot',
                               hr(),
                               column(width = 6,
                                      plotOutput('outpt_t_cost_plt', brush = 't_cost_plt_brsh')       
                               ),
                               column(width = 6,
                                      tableOutput('outpt_filtrd_cities'),
                                      hr(),
                                      verbatimTextOutput('outpt_filtrd_tbl')
                               )
                               
                      ),
                      tabPanel('wip table',
                               hr(),
                               verbatimTextOutput('outpt_t_cost_rctv')
                      ),
                      tabPanel('wip brush',
                               hr(),
                               verbatimTextOutput('outpt_t_cost_plt_brsh')
                      )
                      
                    )       
             )
             
             
           )
           
           
           
    )
  )
  
  
  
)



server <- function(input, output, session) {
  
  #### < Reactives > ####
  
  burshed_rctv <- reactive({
    
    brushedPoints(t_cost_rctv(), brush = input$t_cost_plt_brsh) %>% 
      select(country) %>% 
      left_join(t_cost, by = 'country')
    
  })
  
  t_cost_rctv <- reactive({
    
    t_cost %>% 
      filter(
        start_year >= input$nmbr_start_year
      ) %>% 
      select(country, real_cost) %>% 
      mutate(real_cost = as.double(real_cost)) %>%
      group_by(country) %>% 
      summarise(total_real_cost = sum(real_cost)) %>% 
      mutate(country = factor(country)) %>% 
      mutate(country = fct_reorder(country, total_real_cost)) 
    
  })
  
  #### < Observers > ####
  
  #### < Outputs > ####
  
  
  output$outpt_filtrd_cities <- renderTable({
    
    burshed_rctv() %>% 
      select(country, city, real_cost,length) %>% 
      distinct() %>% 
      arrange() %>% 
      group_by(country, city) %>% 
      mutate(
        city_cost = sum(real_cost),
        total_length = sum(length)
      ) %>% 
      select(-real_cost, -length) %>% 
      distinct()
  })
  
  output$outpt_filtrd_tbl <- renderPrint({
    
    burshed_rctv()
    
  })
  
  
  output$outpt_t_cost_plt <- renderPlot({
    
    t_cost_rctv() %>% 
      ggplot(aes(x = country, y = total_real_cost, fill = country)) +
      geom_bar(stat = "identity", show.legend = F) +
      labs(
        title = "Total Transit Cost to Country",
        subtitle = paste0('Projects Starting after startA'),
        y = "Cost (Currency Unspecified)",
        x = "Country"
      ) +
      coord_flip() +
      theme_tq() +
      scale_fill_tq() 
    
    
  })
  
  output$outpt_t_cost_rctv <- renderPrint({
    
    t_cost_rctv()
    
  })
  
  output$outpt_t_cost_plt_brsh <- renderPrint({
    
    input$t_cost_plt_brsh
    
  })
  
  
  
}

shinyApp(ui, server)