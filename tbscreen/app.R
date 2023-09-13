#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)
library(tidyverse)
library(reshape2)


tests <- c("QuantiFERON", "T-SPOT.TB", "Tuberculin Skin Test")
age_uk <- data.frame("A_0to5" =c(7),
                     "A_6to15"=c(12),
                     "A_16to25"=c(12),
                     "A_26to45"=c(26),
                     "A_46to65"=c(26),
                     "A_65plus"=c(18))

names(age_uk)<-paste(c("0-5" , "6-15","16-25","26-45","46-65","65+"))


cea1 <- data.frame("Total Costs(£)" =c(250000,150000),
                     "Total QALY"=c(20000,30000))

row.names(cea1)<-paste(c("Status-Quo","Intervention"))

cea2 <- data.frame("Cost savings(£)" =c(250000-150000),
                  "QALYs gained"=c(10000))





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Impact and cost-effectiveness of LTBI testing"),
  
  tabsetPanel(
    tabPanel("Epidemiology", fluid = TRUE,
             
             ########## Cohort size
             numericInput("num", "Cohort Size", value = 0, min = 0, max = Inf),
             
             ##########LTBI test
             radioButtons("test", "Prefered test", tests),
             
             ########## Cohort age distribution
             p(strong("UK age distribution (%)")),
             tableOutput("Agedistr"), 
             
             p(strong("Input age distribution (%) or use UK default")),
             
             # Sidebar with a slider input for distribution
             sidebarLayout(
               sidebarPanel(
                 DTOutput("my_datatable"),
                 actionButton("go",label = "Plot")),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot")
               )
             ),
             
             ########## LTBI prevalence 
             p(strong("Input expected LTBI prevalence (%) by age")),
             
             # Sidebar with a slider input for distribution
             sidebarLayout(
               sidebarPanel(
                 DTOutput("my_datatable2"),
                 actionButton("go2",label = "Plot")),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("LtbiPlot")
               )
             ) ),
    
    ############## CEA 
    tabPanel("Costs and QoL", fluid = TRUE,
             
             ### Costs
             numericInput("num", "Cost of test (£)", value = 0, min = 0, max = Inf),
             
             numericInput("num", "Cost of testing campaign (£)", value = 0, min = 0, max = Inf),
             
             p(strong("Health related Quality of Life by disease state (HRQoL)")),
             
             numericInput("num", "Pulmonary TB", value = 0, min = 0, max = 1),
             numericInput("num", "Extrapulmonary TB", value = 0, min = 0, max = 1),
             numericInput("num", "Post-TB lung disease", value = 0, min = 0, max = 1)
    ),
    
    ################### Projections
    
    tabPanel("Projections", fluid = TRUE,
             
             ### Costs
             p(strong("Cost-Effectiveness Results")),
             
             tableOutput("ceares1"),
             
             tableOutput("ceares2")
             
             

    ),
    
    
    
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$Agedistr <- renderTable(age_uk)
  
  v <- reactiveValues(data = { 
    age_uk
  })
  
  #output the datatable based on the dataframe (and make it editable)
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE, rownames=FALSE,
                  options = list(searching = FALSE, dom = 't'))
  })
  
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    v$data[i,j] <- k
  })
  
  # Dist plot
  output$distPlot <- renderPlot({
    req(input$go) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    v$data %>% melt() %>% 
      ggplot(aes(x = variable, y=value )) + 
      geom_bar(stat = "identity", fill="orange")+
      scale_y_continuous(
        labels = abs
      ) + 
      coord_flip() + 
      
      # theme_minimal() +
      labs(
        x = "Age", 
        y = "Population (%)", 
        title = "Population Pyramid"
      )+
      theme(
        legend.position = "none",
        axis.text = element_text(colour = "black", size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
      )
  })
  
  
  ############ LTBI
  v2 <- reactiveValues(data = { 
    age_uk
  })
  
  output$my_datatable2 <- renderDT({
    DT::datatable(v2$data, editable = TRUE, rownames=FALSE,
                  options = list(searching = FALSE, dom = 't'))
  })
  
  observeEvent(input$my_datatable2_cell_edit, {
    #get values
    info = input$my_datatable2_cell_edit
    ii = as.numeric(info$row)
    jj = as.numeric(info$col)
    kk = as.numeric(info$value)
    if(kk < 0){ #convert to positive if negative
      kk <- kk * -1
    }
    
    #write values to reactive
    v2$data[ii,jj] <- kk
  })
  
  # Dist plot
  output$LtbiPlot <- renderPlot({
    req(input$go2) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    v2$data %>% melt() %>% 
      ggplot(aes(x = variable, y=value )) + 
      geom_bar(stat = "identity", fill="firebrick")+
      scale_y_continuous(
        labels = abs
      ) + 
      coord_flip() + 
      
      # theme_minimal() +
      labs(
        x = "Age", 
        y = "Prevalence (%)", 
        title = "Expected LTBI prevalence"
      )+
      theme(
        legend.position = "none",
        axis.text = element_text(colour = "black", size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
      )
  })
  
  
  #### Results
  
  output$ceares1 <- renderTable(cea1)
  
  output$ceares2 <- renderTable(cea2)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
