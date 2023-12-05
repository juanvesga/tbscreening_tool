#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list = ls()) 




#library(shiny)
library(shinydashboard)
library(shinyvalidate)
#library(shinydashboardPlus)
library(DT)
library(tidyverse)
library(reshape2)
library(rstpm2)
library(rms)
library(lubridate)
library(glue)
library(matrixStats)
#library(hesim)
library("magrittr") # Use pipes
library(data.table)
library(BCEA)
library(ggplot2)
library(purrr)

path_app<-rstudioapi::getSourceEditorContext()$path
setwd(gsub('/app.R','', path_app))

source("Categorical.R")
source("rpert.R")

# Load required objects into working directory

load("qfn_lookup") # QFT percentile look-up table
load("tspot_lookup") # T=SPOT percentile look-up table
load("tst_lookup") # TST percentile look-up table
load("fit_final_github_2020-08-14") # This is the model object

model<-fit.final.github
coeff<-model@lm$coefficients
# Load WHO TB incidence by country data and preprocess for merging later

country_tb_inc <- read.csv("TB_burden_countries_2020-08-14.csv")
country_tb_inc <- country_tb_inc %>% select(country, year, e_inc_100k) %>% 
  rename(country_of_birth=country, year_of_entry=year)




tests <- c("QuantiFERON", "T-SPOT.TB", "Tuberculin Skin Test")
age_uk <- t(data.frame("A_16to35"=c(14),
                       "A_36to45"=c(32),
                       "A_46to65"=c(32),
                       "A_65plus"=c(22)))

prev_uk <- t(data.frame("A_16to35"=c(10),
                        "A_36to45"=c(25),
                        "A_46to65"=c(35),
                        "A_65plus"=c(45)))

rownames(age_uk)<-paste(c("16-35","36-45","46-65","65+"))
rownames(prev_uk)<-paste(c("16-35","36-45","46-65","65+"))




##### temp Values
## Epi
cohort_size<-5000
test<-"quantiferon" # tst or tspot
year_test <- 2019
country <- "Pakistan"
time_horizon<-20


## Costs
tpt_cost <- 100  # once
test_cost <- 50 # once
tbtx_cost_yr <- 100 ## per year


## QoL from Kind eta l 1999
qol_full <- t(data.frame("A_16to35"=c(0.93),
                         "A_36to45"=c(0.91),
                         "A_46to65"=c(0.82),
                         "A_65plus"=c(0.75)))



X <- Categorical(rownames(age_uk), p = age_uk/100)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "LTBI CEA Tool"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("How to use the tool", tabName = "how", icon = icon("check")),
      menuItem("Demographics", tabName = "epi", icon = icon("bar-chart")),
      menuItem("LTBI tests", tabName = "ltbi", icon = icon("vial")),
      menuItem("LTBI cascade", tabName = "cascade", icon = icon("chart-simple")),
      menuItem("Costs", tabName = "cost", icon = icon("sterling-sign")),
      menuItem("Quality of Life", tabName = "qol", icon = icon("staff-snake")),
      sliderInput("t_hor", "Time-horizon (years)", 2, 50, 5),
      menuItem("Results", icon = icon("bar-chart"), startExpanded = TRUE,
               menuSubItem("TB Incidence", tabName = "res_inc"),
               menuSubItem("Costs", tabName = "res_costs"),
               menuSubItem("ICER", tabName = "res_icer")
      ),
      menuItem("About", tabName = "about", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # 5 About tab
      tabItem(tabName = "how",
              fluidPage(
                p("How to use this tool",style = "font-size:25px"),
                p("1) Define the size, age distribution and ethnic origin 
                  in the baseline cohort in the ",  
                  span(tags$a("Demographics Tab", onclick="Shiny.onInputChange('tab', 'epi')"),
                       style = "color:blue"),
                  style = "font-size:18px"),
                
                p("2) Provide details on the expected positivity of the LTBI, 
                  and if known HIV prevalence in the baseline cohort.",style = "font-size:18px"),
                
                
                p("3) Select your choice of LTBI test, TPT regimen and eligibility 
                  and main parameters on completion",style = "font-size:18px"),
                
                p("4) Select costs and QoL for relevant TB outcomes. Here you can 
                  define the shape of the background distributions for these inputs",style = "font-size:18px"),
                
                p("5) Choose your time-horizon for analysis",style = "font-size:18px"),
                
                p("6) Explore costs and epidemilogical estimates in the",
                  span(tags$a("Results Tab", onclick="Shiny.onInputChange('tab', 'epi')"),
                       style = "color:blue",
                       style = "font-size:18px"),
                  style = "font-size:18px"),
                
                p("7) Run CEA analsys using the run button in the ICER panel. 
                  Here you can download a report of your scenario",
                  style = "font-size:18px")
              )
      ),
      
      
      # 1 Epidemiology
      tabItem(tabName = "epi",
              
              fluidRow(
                box(
                  numericInput(inputId ="n", label="Cohort Size", value = 5000, min = 0, max = Inf),
                ),
                box(
                  selectInput("country",label = "Choose country of origin",
                              choices = c("Bangladesh","India","Lesotho" ,"Pakistan", "Uganda")),
                )
              ),
              
              fluidRow(
                
                box(
                  title = "Age distribution (%)",
                  sliderInput("age4", "65+", 0, 100, age_uk[4]),
                  sliderInput("age3", "46-65", 0, 100, age_uk[3]),
                  sliderInput("age2", "36-45 ", 0, 100, age_uk[2]),
                  sliderInput("age1", "16-35 ", 0, 100, age_uk[1]),
                  
                  verbatimTextOutput('result'),
                  tags$head(tags$style("#result{color: red}"
                  )
                  )
                ),
                
                box(plotOutput("distPlot")),
                
              )
      ),
      # 2 LTBI Test
      tabItem(tabName = "ltbi",
              
              fluidRow(
                box(
                  
                  sliderInput("hiv_prev4", "65+", 0, 100, 0),
                  sliderInput("hiv_prev3", "46-65", 0, 100, 0),
                  sliderInput("hiv_prev2", "36-45 ", 0, 100, 0),
                  sliderInput("hiv_prev1", "16-35 ", 0, 100, 0),
                ),
                
                
                box(plotOutput("hiv_prevPlot")),
                
                
              ),
              
              fluidRow(
                
                box(
                  title = "TB infection prevalence (%)",
                  sliderInput("prev4", "65+", 0, 100, prev_uk[4]),
                  sliderInput("prev3", "46-65", 0, 100, prev_uk[3]),
                  sliderInput("prev2", "36-45 ", 0, 100, prev_uk[2]),
                  sliderInput("prev1", "16-35 ", 0, 100, prev_uk[1]),
                ),
                
                box(plotOutput("prevPlot")),
                
              )
      ),
      
      # 3 LTBI cascade
      tabItem(tabName = "cascade",
              
              fluidRow(
                box(
                  column(4,
                         numericInput(inputId ="year_test", label="Year of test", value = 2020, min = 0, max = Inf),
                  ),
                  column(4,
                         radioButtons("test", "Prefered test", tests),
                  ),
                  column(2,
                         radioButtons("tpt", "Choose TPT regimen", c("6INH","3HP", "3RH")),
                  )
                ),
                box(width = 4,
                    checkboxGroupInput(inputId="age_tpt", 
                                       label="Age groups eligible for TPT:",
                                       choices=c(
                                         "16-35" = "16-35",
                                         "36-45" = "36-45",
                                         "46-65" = "46-65",
                                         "65+" = "65+"),
                                       selected = c(
                                         "36-45" = "36-45",
                                         "46-65" = "46-65",
                                         "65+" = "65+")),
                )
              ),
              fluidRow(
                valueBoxOutput(width=3,"tpt_effectiveness"),
                
                valueBoxOutput(width=3,"tpt_AE"),
                
                box(width = 2,
                    checkboxInput("manualae", "Manually input adverse events", FALSE),
                ),
                conditionalPanel(
                  condition = "input.manualae == 1",
                  box(width = 2,
                      numericInput("manualeamean", "% developing adverse events", value = 8, min = 0, max = 100),
                  ),
                )
              ),
              
              
              fluidRow(
                
                box( width =6,
                     column(5,
                            sliderInput("casc1", "LTBI testing positive that start TPT (%)", 0, 100, 0)             
                     ),
                     
                     column(5,
                            sliderInput("casc2", "On TPT and completing their regimen (%) ", 0, 100, 0)
                     )
                ),
                
                
              ),
              
              fluidRow(
                
                box(
                  title = "LTBI testing and treatment cascade",
                  plotOutput("cascadeplot"))
              )
              
      ),
      
      
      # 4 Cost
      tabItem(tabName = "cost",
              # Tets cost
              fluidRow(
                box(width = 4,
                    title="Unit cost of LTBI test (£)",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("costPERT1", height = "35vh")
                    
                ),
                box(width = 2,
                    title="Select distribution to be used",
                    status = "info", solidHeader = FALSE,
                    radioButtons("testcost_dist", "", c("Gamma","PERT")),
                ),
                conditionalPanel(
                  condition = "input.testcost_dist == 'PERT'",
                  box(width = 3,
                      numericInput("cost_test_pert", "Most likely", value = 50, min = 0, max = Inf),
                      numericInput("cost_testmin_pert", "Min", value = 10, min = 0, max = Inf),
                      numericInput("cost_testmax_pert", "Max", value = 70, min = 0, max = Inf),
                      sliderInput("cost_testlam", "Shape", value = 4, min = 0, max = 10)
                  ),
                ),
                conditionalPanel(
                  condition = "input.testcost_dist == 'Gamma'",
                  box(width = 3,
                      numericInput("cost_test_gamma", "Mean", value = 50, min = 0, max = Inf),
                      numericInput("cost_testsd_gamma", "SD", value = 10, min = 0, max = Inf)
                  ),
                )
              ),
              
              # Campaign cost
              fluidRow(  
                box(width = 4,
                    title="Cost of testing campaign (£) - Incurred only at start",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("costPERT2", height = "35vh")
                    
                ),
                box(width = 2,
                    title="Select distribution to be used",
                    status = "info", solidHeader = FALSE,
                    radioButtons("campcost_dist", "", c("Gamma","PERT")),
                ),
                
                conditionalPanel(
                  condition = "input.campcost_dist == 'PERT'",
                  box(width = 3,
                      numericInput("cost_camp_pert", "Most likely", value = 1000, min = 0, max = Inf),
                      numericInput("cost_campmin_pert", "Min", value = 800, min = 0, max = Inf),
                      numericInput("cost_campmax_pert", "Max", value = 1200, min = 0, max = Inf),
                      sliderInput("cost_camplam", "Shape", value = 4, min = 0, max = 10)
                  ),
                ),
                conditionalPanel(
                  condition = "input.campcost_dist == 'Gamma'",
                  box(width = 3,
                      numericInput("cost_camp_gamma", "Mean", value = 1000, min = 0, max = Inf),
                      numericInput("cost_campsd_gamma", "SD", value = 200, min = 0, max = Inf)
                  ),
                )
              ),
              # cost TPT
              fluidRow(  
                box(width = 4,
                    title="Unit Cost of completed TPT regimen (£)",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("costPERT3", height = "35vh")
                ),
                box(width = 2,
                    title="Select distribution to be used",
                    status = "info", solidHeader = FALSE,
                    radioButtons("tptcost_dist", "", c("Gamma","PERT")),
                ),
                conditionalPanel(
                  condition = "input.tptcost_dist == 'PERT'",
                  box(width = 3,
                      numericInput("cost_tpt_pert", "Most likely", value = 1000, min = 0, max = Inf),
                      numericInput("cost_tptmin_pert", "Min", value = 800, min = 0, max = Inf),
                      numericInput("cost_tptmax_pert", "Max", value = 1200, min = 0, max = Inf),
                      sliderInput("cost_tptlam", "Shape", value = 4, min = 0, max = 10)
                  ),
                ),
                conditionalPanel(
                  condition = "input.tptcost_dist == 'Gamma'",
                  box(width = 3,
                      numericInput("cost_tpt_gamma", "Mean", value = 1000, min = 0, max = Inf),
                      numericInput("cost_tptsd_gamma", "SD", value = 200, min = 0, max = Inf)
                  ),
                )
              ),
              # LTFUP cost
              fluidRow(  
                box(
                  title = "Proportion of TPT regimen cost incurred by those lost to follow-up (%) ",
                  status = "primary", solidHeader = TRUE,
                  sliderInput("cost_ltfup", "(%)", 0, 100, 0),
                ),
              ),
              
      ),
      
      # 5 QoL
      tabItem(tabName = "qol",
              
              fluidRow(
                
                box(
                  title = "Baseline Quality of Life (QoL) by age",
                  status = "primary", solidHeader = TRUE,
                  plotOutput("fullQOL_Plot")),
                
                box(
                  sliderInput("qolfull_1", "16-35 ", 0, 1, qol_full[1]),
                  sliderInput("qolfull_2", "36-45 ", 0, 1, qol_full[2]),
                  sliderInput("qolfull_3", "46-65", 0, 1, qol_full[3]),
                  sliderInput("qolfull_4", "65+", 0, 1, qol_full[4])
                )  
              ),
              
              # PTB QoL
              fluidRow(
                box(width = 4,
                    title="QALY loss due to pulmonary TB disease",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("qolPERT1", height = "35vh")
                    
                ),
                box(width = 2,
                    title="Select distribution to be used",
                    status = "info", solidHeader = FALSE,
                    radioButtons("ptbqol_dist", "", c("Beta","PERT")),
                ),
                conditionalPanel(
                  condition = "input.ptbqol_dist == 'PERT'",
                  box(width = 3,
                      sliderInput("qol_ptb_pert", "Most likely", value = 0.0838, min = 0, max = 1),
                      sliderInput("qol_ptbmin_pert", "Min", value = 0.06, min = 0, max = 1),
                      sliderInput("qol_ptbmax_pert", "Max", value = 0.1, min = 0, max = 1),
                      sliderInput("qol_ptblam_pert", "Shape", value = 4, min = 0, max = 10)
                  ),
                ),
                conditionalPanel(
                  condition = "input.ptbqol_dist == 'Beta'",
                  box(width = 3,
                      sliderInput("qol_ptb_beta", "Mean", value = 0.0838, min = 0, max = 1),
                      sliderInput("qol_ptbsd_beta", "SD", value = 0.016, min = 0, max = 1)
                  ),
                )
              ),
              
              # EPTB QoL
              fluidRow(
                box(width = 4,
                    title="QALY loss due to Extra-pulmonary TB disease",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("qolPERT2", height = "35vh")
                    
                ),
                box(width = 2,
                    title="Select distribution to be used",
                    status = "info", solidHeader = FALSE,
                    radioButtons("eptbqol_dist", "", c("Beta","PERT")),
                ),
                conditionalPanel(
                  condition = "input.eptbqol_dist == 'PERT'",
                  box(width = 3,
                      sliderInput("qol_eptb_pert", "Most likely", value = 0.05, min = 0, max = 1),
                      sliderInput("qol_eptbmin_pert", "Min", value = 0.02, min = 0, max = 1),
                      sliderInput("qol_eptbmax_pert", "Max", value = 0.08, min = 0, max = 1),
                      sliderInput("qol_eptblam_pert", "Shape", value = 4, min = 0, max = 10)
                  ),
                ),
                conditionalPanel(
                  condition = "input.eptbqol_dist == 'Beta'",
                  box(width = 3,
                      sliderInput("qol_eptb_beta", "Mean", value = 0.05, min = 0, max = 1),
                      sliderInput("qol_eptbsd_beta", "SD", value = 0.01, min = 0, max = 1)
                  ),
                )
              ),
              
              # QoL postTB
              
              fluidRow(
                box(width = 4,
                    title="QALY loss due Post-TB Lung disease",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("qolPERT3", height = "35vh")
                    
                ),
                box(width = 2,
                    title="Select distribution to be used",
                    status = "info", solidHeader = FALSE,
                    radioButtons("postqol_dist", "", c("Beta","PERT")),
                ),
                conditionalPanel(
                  condition = "input.postqol_dist == 'PERT'",
                  box(width = 3,
                      sliderInput("qol_post_pert", "Most likely", value = 0.03, min = 0, max = 1),
                      sliderInput("qol_postmin_pert", "Min", value = 0.01, min = 0, max = 1),
                      sliderInput("qol_postmax_pert", "Max", value = 0.1, min = 0, max = 1),
                      sliderInput("qol_postlam_pert", "Shape", value = 4, min = 0, max = 10)
                  ),
                ),
                conditionalPanel(
                  condition = "input.postqol_dist == 'Beta'",
                  box(width = 3,
                      sliderInput("qol_post_beta", "Mean", value = 0.03, min = 0, max = 1),
                      sliderInput("qol_postsd_beta", "SD", value = 0.01, min = 0, max = 1)
                  ),
                )
              ),
              
              # QoL TPT Adevrese events (AE)
              
              fluidRow(
                box(width = 4,
                    title="QALY loss due adverse events (AE) from TPT",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("qolPERT4", height = "35vh")
                    
                ),
                box(width = 2,
                    title="Select distribution to be used",
                    status = "info", solidHeader = FALSE,
                    radioButtons("aeqol_dist", "", c("Beta","PERT")),
                ),
                conditionalPanel(
                  condition = "input.aeqol_dist == 'PERT'",
                  box(width = 3,
                      numericInput("qol_ae_pert", "Most likely", value = 0.0046, min = 0, max = 1),
                      numericInput("qol_aemin_pert", "Min", value = 0.001, min = 0, max = 1),
                      numericInput("qol_aemax_pert", "Max", value = 0.1, min = 0, max = 1),
                      sliderInput("qol_aelam_pert", "Shape", value = 4, min = 0, max = 10)
                  ),
                ),
                conditionalPanel(
                  condition = "input.aeqol_dist == 'Beta'",
                  box(width = 3,
                      numericInput("qol_ae_beta", "Mean", value = 0.0046, min = 0, max = 1),
                      numericInput("qol_aesd_beta", "SD", value = 0.001, min = 0, max = 1)
                  ),
                )
              )
              
      ),
      
      # 6 Incidenceplots
      tabItem(tabName = "res_inc",
              fluidRow(column(9,
                              box(width=12,
                                  plotOutput("plot_inc")
                              )
              )
              )
              
      ),
      
      # 6 costplots
      
      
      tabItem(tabName = "res_costs",
              fluidRow(column(9,
                              box(width=12,
                                  plotOutput("plot_costs")
                              )
              )
              )
      ),
      
      # 7 ICER
      tabItem(tabName = "res_icer",
              fluidRow(
                valueBoxOutput("ICER"),
                valueBoxOutput("qalygained"),
                valueBoxOutput("costsaved")
              ),
              
              fluidRow(
                box(
                  sliderInput("will_to_pay", "Wilingness to pay (£)", 5000, 50000, 20000)
                ),
                actionButton("add","Run"),
                downloadButton("btn", "Generate Report")
                
              ),
              fluidRow(
                box(
                  plotOutput("plot_icer")
                ),
                box(
                  plotOutput("plot_wtp")
                )
              )
      ),
      # 5 About tab
      tabItem(tabName = "about",
              fluidPage(
                p("TB infection testing cost-effectiveness calculator",style = "font-size:25px"),
                p("This tool was created to help quantify and visualise the 
                  potential impact of performing TB infection tests, and providing
                  TB preventive regimens among immigrant populations in the UK.",
                  style = "text-align: justify; font-size:18px"),
                p("The user is prompted to input information on demographic characteristics
                  of the cohort, type of TB infection test, and the time
                  horizon of analisis. With this informmation, the tool is able 
                  to retrieve estimations of expected TB disease cases, 
                  cost projections of the testing and TB prevention teratment  
                  intervention, and importantly, a full set of cost-effectiveness 
                  analysis (CEA) output.",style = "text-align: justify; font-size:18px"),
                
                p("How it works",style = "font-size:25px"),
                
                p("This tool builds on a previosuly developed personalised risk 
                  predictor for incident TB", 
                  span(tags$a(href="http://www.periskope.org/", 
                              "PERISKOPE-TB"),style = "color:blue"),
                  ". At the core of PERISKOPE is a flexible parametric survival model 
                  designed for estimating TB risk at the individual level. We 
                  exploit this engine to recreate population-level estimations 
                  of TB incidence. We build the health economics assesment on top of this 
                  estimations, using the input provided by the user trhough the interface",
                  style = "text-align: justify; font-size:18px"),
                
                p("For further details on the methods behind PERISKOPE-TB, please consult 
                here: ", 
                  span(tags$a(href="https://www.nature.com/articles/s41591-020-1076-0.epdf?sharing_token=WrllWDo82ksX5S2UVcva3tRgN0jAjWel9jnR3ZoTv0N7Cf4tktJr3XujjQUdrBMIc-wX36PquVEMu0shOkAcLeC3EB4y0xOLdE2FujQxD8QEEmD_kM6-ycVNXD5NMCzSf_7O5GYkZkAFnxebkh5K1D9hXJKjHtyEqS_mEObTt9E%3D", 
                              "Gupta RK et al, Nature Medicine 2020."),style = "color:blue"),
                  style = "text-align: justify; font-size:18px")
                
                
              )
              
      )
    )
  )
)


###################################################################################
server <- function(input, output,session) {
  
  ## Epidemiology Tab
  
  set.seed(122)
  histdata <- rnorm(500)
  
  observeEvent( input$n, {
    updateNumericInput(session, "n_selected", value = input$n)
  })
  
  
  output$Agedistr <- renderTable(age_uk, rownames=TRUE, colnames = FALSE)
  
  
  v <- reactiveValues(data = { 
    (age_uk)
  })
  
  valfun<-  reactive({
    (     as.numeric(input$age1) +  
            as.numeric(input$age2) +  
            as.numeric(input$age3)+ 
            as.numeric(input$age4) )
  })
  
  # iv<-InputValidator$new()
  # 
  # iv$add_rule(as.numeric(valfun()) ,sv_equal(100))
  # 
  # iv$enable()
  
  dfage <- reactive({ 
    
    x<-data.frame(
      age=c("16-35","36-45","46-65","65+"),
      value=as.numeric(c(input$age1,input$age2,input$age3,input$age4)))
    return(x)
  })
  
  
  output$result <- renderText({
    
    val <- as.numeric(input$age1) + as.numeric(input$age2) + as.numeric(input$age3) + as.numeric(input$age4)
    
    out<-NA
    if (val != 100){
      
      out<-paste0(val,": Sum must be equal to 100")
      
    }else{
      
      out<-val
    }
    
    out
  })
  
  
  v2 <- reactiveValues(data = { 
    (prev_uk)
  })
  
  dfprev <- reactive({ 
    x<-data.frame(
      age=c("16-35","36-45","46-65","65+"),
      value=as.numeric(c(input$prev1,input$prev2,input$prev3,input$prev4)))
    return(x)
  })
  
  dfhivprev <- reactive({ 
    x<-data.frame(
      age=c("16-25","26-45","46-65","65+"),
      value=as.numeric(c(input$hiv_prev1,input$hiv_prev2,input$hiv_prev3,input$hiv_prev4)))
    return(x)
  })
  
  dffullqol <- reactive({ 
    x<-data.frame(
      age=c("16-25","26-45","46-65","65+"),
      value=as.numeric(c(input$qolfull_1,input$qolfull_2,input$qolfull_3,input$qolfull_4)))
    return(x)
  })
  
  
  #output the datatable based on the dataframe (and make it editable)
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE, rownames=TRUE, colnames = '%',
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
    
    # req(input$age1) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    dfage() %>% 
      ggplot(aes(x = age, y=value )) +
      geom_bar(stat = "identity", fill="aquamarine4")+
      scale_y_continuous(
        labels = abs
      ) +
      coord_flip() +
      
      # theme_minimal() +
      labs(
        x = "Age",
        y = "Population (%)"
        # title = "Population Pyramid"
      )+
      theme_minimal()+
      theme(
        legend.position = "none",
        axis.text = element_text(colour = "black", size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
      )
  })
  
  
  # Prev plot
  output$prevPlot <- renderPlot({
    # req(input$age1) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    dfprev() %>% 
      ggplot(aes(x = age, y=value )) +
      geom_bar(stat = "identity", fill="firebrick1")+
      scale_y_continuous(
        labels = abs
      ) +
      coord_flip() +
      
      # theme_minimal() +
      labs(
        x = "Age",
        y = "TBI Prevalence (%)"
        # title = "Population Pyramid"
      )+
      theme_minimal()+
      theme(
        legend.position = "none",
        axis.text = element_text(colour = "black", size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
      )
  })
  # Prev plot
  output$hiv_prevPlot <- renderPlot({
    # req(input$age1) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    dfhivprev() %>% 
      ggplot(aes(x = age, y=value )) +
      geom_bar(stat = "identity", fill="darkcyan")+
      scale_y_continuous(
        labels = abs
      ) +
      coord_flip() +
      
      # theme_minimal() +
      labs(
        x = "Age",
        y = "HIV Prevalence (%)"
        # title = "Population Pyramid"
      )+
      theme_minimal()+
      theme(
        legend.position = "none",
        axis.text = element_text(colour = "black", size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
      )
  })
  
  
  
  
  #### LTBI cascade 
  
  observeEvent(input$tpt, {
    
    
    output$tpt_effectiveness <- renderValueBox({
      
      tpt_eff<-0
      if (input$tpt == "6INH"){
        tpt_eff <- 0.6 
      } else if(input$tpt =="3HP"){
        tpt_eff <- 0.53
      } else if(input$tpt =="3RH"){
        tpt_eff <- 0.6
      }
      
      valueBox(
        value = paste0((1-tpt_eff)*100,"%"),
        subtitle = "TB disease reduction, i.e, 1-RR",
        icon = icon("medkit"),
        color = "teal"
      )
    })
  })
  
  output$tpt_AE <- renderValueBox({
    
    tpt_ae<-0
    if (input$tpt == "6INH"){
      tpt_ae <- 0.04 
    } else if(input$tpt =="3HP"){
      tpt_ae <- 0.08
    } else if(input$tpt =="3RH"){
      tpt_ae <- 0.039
    }
    
    valueBox(
      value = paste0((tpt_ae)*100,"%"),
      subtitle = "% developing adverse events",
      icon = icon("medkit"),
      color = "orange"
    )
  })
  
  
  
  output$cascadeplot <- renderPlot({
    req(input$casc1,input$casc2)
    cohort_size<-input$n
    start_tpt<-input$casc1/100
    complete_tpt<-input$casc2/100
    
    tpt_eff<-0
    if (input$tpt == "6INH"){
      tpt_eff <- 0.6  
    } else if(input$tpt =="3HP"){
      tpt_eff <- 0.53 
    } else if(input$tpt =="3RH"){
      tpt_eff <- 0.6 
    }
    
    
    
    age<-dfage()
    prev<-dfprev()
    eligible<-data.frame(age=age$age, value=age$age%in%input$age_tpt)
    ca2<-sum(((age$value)/100)*(prev$value/100)) * cohort_size
    ca3<-sum((age$value/100)*(prev$value/100) * eligible$value) * cohort_size
    ca4<-ca3*start_tpt
    ca5<-ca4*complete_tpt
    ca6<- ca5 * tpt_eff
    
    
    df<-data.frame(label=c("Tested", "LTBI+","Eligible for TPT","Starting TPT","Completing TPT","TBI cleared"),
                   val = c(cohort_size,
                           ca2,ca3,ca4,ca5,ca6))
    df$label<- factor(df$label, levels = df$label)
    
    p<-ggplot(data=df, aes(x=label,y=val))+
      geom_bar(stat = "identity", fill=c("gold4","yellow4","yellow3","yellow2","yellow1","yellow"))+
      ylab("Number")+ xlab("Cascade stage")+
      theme_minimal()+
      theme(
        text = element_text(size=20),
        axis.text.x = element_text(angle=60, hjust=1))
    
    
    
    gridExtra::grid.arrange(p)
    
    
  })
  
  
  ###### Costs
  
  output$costPERT1<- renderPlot({
    
    n<-input$n
    
    if (input$testcost_dist =="Gamma"){
      xmean<-input$cost_test_gamma
      #xmin<-input$cost_testmin_gamma
      #xmax<-input$cost_testmax_gamma
      sd<- input$cost_testsd_gamma# (xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      x  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$testcost_dist =="PERT"){
      xmean<-input$cost_test_pert
      xmin<-input$cost_testmin_pert
      xmax<-input$cost_testmax_pert
      lam<- input$cost_testlam
      x<-rpert(n,xmin,xmax,xmean,lam)
    }
    
    hist(x,
         breaks=20,
         main = " ",
         xlab = "£",
         col = "gold2")
    
  })
  
  output$costPERT2<- renderPlot({
    
    n<-input$n
    
    if (input$campcost_dist =="Gamma"){
      xmean<-input$cost_camp_gamma
      # xmin<-input$cost_campmin_gamma
      # xmax<-input$cost_campmax_gamma
      sd<- input$cost_campsd_gamma#(xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      x  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$campcost_dist =="PERT"){
      xmean<-input$cost_camp_pert
      xmin<-input$cost_campmin_pert
      xmax<-input$cost_campmax_pert
      lam<- input$cost_camplam
      x<-rpert(n,xmin,xmax,xmean,lam)
    }
    
    
    hist(x,
         20,
         main = " ",
         xlab = "£",
         col = "gold2")
    
  })
  
  output$costPERT3<- renderPlot({
    
    
    n<-input$n
    
    if (input$tptcost_dist =="Gamma"){
      xmean<-input$cost_tpt_gamma
      # xmin<-input$cost_tptmin_gamma
      # xmax<-input$cost_tptmax_gamma
      sd<- input$cost_tptsd_gamma#(xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      x  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$tptcost_dist =="PERT"){
      
      xmean<-input$cost_tpt_pert
      xmin<-input$cost_tptmin_pert
      xmax<-input$cost_tptmax_pert
      lam<- input$cost_tptlam
      x<-rpert(n,xmin,xmax,xmean,lam)
    }
    hist(x,
         20,
         main = " ",
         xlab = "£",
         col = "gold2")
    
  })
  
  
  ###### QOL
  estBetaParams <- function(mu, var) {
    alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
    beta <- alpha * (1 / mu - 1)
    return(params = list(alpha = alpha, beta = beta))
  }
  
  # Prev plot
  output$fullQOL_Plot <- renderPlot({
    
    
    
    dffullqol() %>% 
      ggplot(aes(x = age, y=value )) +
      geom_bar(stat = "identity", fill="darkcyan")+
      scale_y_continuous(
        labels = abs
      ) +
      labs(
        x = "Age",
        y = "Quality of Life"
        # title = "Population Pyramid"
      )+
      theme_minimal()+
      theme(
        legend.position = "none",
        axis.text = element_text(colour = "black", size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
      )
  })
  
  # Qol PtTB
  output$qolPERT1<- renderPlot({
    
    
    n<-input$n
    
    if (input$ptbqol_dist =="Beta"){
      
      xmean<-input$qol_ptb_beta
      # xmin<-input$qol_ptbmin_beta
      # xmax<-input$qol_ptbmax_beta
      sd<- input$qol_ptbsd_beta# (xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      x <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$ptbqol_dist =="PERT"){
      
      xmean<-input$qol_ptb_pert
      xmin<-input$qol_ptbmin_pert
      xmax<-input$qol_ptbmax_pert
      lam<- input$qol_ptblam_pert
      x<-rpert(n,xmin,xmax,xmean,lam)
      
    }
    hist(x,
         20,
         main = " ",
         xlab = "QoL losses",
         col = "violetred3")
    
  })
  
  
  # Qol EPtTB
  output$qolPERT2<- renderPlot({
    
    
    n<-input$n
    
    if (input$eptbqol_dist =="Beta"){
      
      xmean<-input$qol_eptb_beta
      # xmin<-input$qol_eptbmin_beta
      # xmax<-input$qol_eptbmax_beta
      sd<- input$qol_eptbsd_beta#(xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      x <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$eptbqol_dist =="PERT"){
      
      xmean<-input$qol_eptb_pert
      xmin<-input$qol_eptbmin_pert
      xmax<-input$qol_eptbmax_pert
      lam<- input$qol_eptblam_pert
      x<-rpert(n,xmin,xmax,xmean,lam)
      
    }
    hist(x,
         20,
         main = " ",
         xlab = "QoL losses",
         col = "violetred3")
    
  })
  
  # Qol PostTB
  output$qolPERT3<- renderPlot({
    
    
    n<-input$n
    
    if (input$postqol_dist =="Beta"){
      
      xmean<-input$qol_post_beta
      # xmin<-input$qol_postmin_beta
      # xmax<-input$qol_postmax_beta
      sd<- input$qol_postsd_beta#(xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      x <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$postqol_dist =="PERT"){
      
      xmean<-input$qol_post_pert
      xmin<-input$qol_postmin_pert
      xmax<-input$qol_postmax_pert
      lam<- input$qol_postlam_pert
      x<-rpert(n,xmin,xmax,xmean,lam)
      
    }
    hist(x,
         20,
         main = " ",
         xlab = "QoL losses",
         col = "violetred3")
    
  })
  
  # Qol TPT AE
  output$qolPERT4<- renderPlot({
    
    
    n<-input$n
    
    if (input$aeqol_dist =="Beta"){
      
      xmean<-input$qol_ae_beta
      # xmin<-input$qol_aemin_beta
      # xmax<-input$qol_aemax_beta
      sd<- input$qol_aesd_beta#(xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      x <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$aeqol_dist =="PERT"){
   
      xmean<-input$qol_ae_pert
      xmin<-input$qol_aemin_pert
      xmax<-input$qol_aemax_pert
      lam<- input$qol_aelam_pert
      x<-rpert(n,xmin,xmax,xmean,lam)
      
    }
    hist(x,
         20,
         main = " ",
         xlab = "QoL losses",
         col = "violetred3")
    
    
    
  })
  
  
  
  
  
  ### Get dataset for Periskope   
  df<- reactive({
    
    cohort_size<-input$n
    test<-input$test # tst or tspot
    year_test <- input$year_test
    country <- input$country
    time_horizon<-input$t_hor
    
    prev<-v2$data[,1]/100
    
    # Global
    lookage <- read.table(header = TRUE,
                          stringsAsFactors = FALSE,
                          text="Age_cat Age
  16-35         21
  36-45         36
  46-65         56
  65+         83")
    
    
    ## Costs
    tpt_cost <- 100  # once
    test_cost <- 50 # once
    tbtx_cost_yr <- 100 ## per year
    
    samps_age<-random.Categorical(X, cohort_size)
    
    largetable <- data.frame(
      Age_cat = samps_age,
      stringsAsFactors = FALSE)
    
    base1 <- (merge(largetable,lookage,  by = 'Age_cat'))
    base1$result<-"Negative"
    
    na1<-which(base1$Age_cat=="A_16to35")
    na2<-which(base1$Age_cat=="A_36to45")
    na3<-which(base1$Age_cat=="A_46to65")
    na4<-which(base1$Age_cat=="A_65plus")
    
    a1pos<-round(length(na1)*prev[1])
    base1$result[na1[1:a1pos]]<-"Positive"
    
    a2pos<-round(length(na2)*prev[2])
    base1$result[na2[1:a2pos]]<-"Positive"
    
    a3pos<-round(length(na3)*prev[3])
    base1$result[na3[1:a3pos]]<-"Positive"
    
    a4pos<-round(length(na4)*prev[4])
    base1$result[na4[1:a4pos]]<-"Positive"
    
    npositives<-length(which(base1$result=="Positive"))
    
    # Results
    base1$pct_qfn <-NA
    base1$pct_tspot <- NA
    base1$pct_tst <- NA
    base1$qfn_result <-NA
    base1$tspot_result <- NA
    base1$tst_result <- NA
    
    if(test=="QuantiFERON"){
      base1$qfn_result <-base1$result
    }else if(test=="T-SPOT.TB"){
      base1$tspot_result <-base1$result
      
    }else{
      base1$tst_result <-base1$result
      
    }
    # Impute result from qualitative result
    base1 <- base1 %>% mutate(pct_testspl1 = case_when(!is.na(pct_qfn) ~ as.integer(pct_qfn),
                                                       !is.na(pct_tspot) ~ as.integer(pct_tspot),
                                                       qfn_result=="Positive" ~ as.integer(87),
                                                       qfn_result=="Negative" ~ as.integer(1),
                                                       tspot_result=="Positive" ~ as.integer(87),
                                                       tspot_result=="Borderline positive" ~ as.integer(79),
                                                       tspot_result=="Borderline negative" ~ as.integer(76),
                                                       tspot_result=="Negative" ~ as.integer(1),
                                                       !is.na(pct_tst) ~ as.integer(pct_tst)))
    
    ### Add test result splines (5 knots at fixed positions)
    pct_test_spline5 <- as.data.frame(rcspline.eval(base1$pct_testspl1, knots = c(5, 27.5, 50, 72.5, 95)))
    colnames(pct_test_spline5) <- c("pct_testspl2", "pct_testspl3", "pct_testspl4")
    base1 <- data.frame(base1,pct_test_spline5)
    
    ## Age splines (5 knots at fixed positions)
    base1 <- base1 %>% rename(agespl1=Age)
    age_spline5 <- as.data.frame(rcspline.eval(base1$agespl1, knots = c(8, 25, 33.07, 45, 64)))
    colnames(age_spline5) <- c("agespl2", "agespl3", "agespl4")
    base1 <- data.frame(base1,age_spline5)
    
    # Status
    base1$exposure_cat4b <- "No contact, migrant"
    base1$year_of_entry <- year_test-1
    
    base1$country_of_birth<- country
    
    base1 <- left_join(base1, country_tb_inc)
    base1$exposure_cat4b[base1$e_inc_100k>=100 & base1$contact=="No"] <- "No contact, migrant"
    base1$months_migrant<-12
    base1$hivpos <- "No"
    base1$transplant<-"No"
    base1$ltbi_treatment<-"No"
    
    base1 <- base1 %>% select(agespl1, agespl2, agespl3, agespl4,
                              pct_testspl1, pct_testspl2, pct_testspl3, pct_testspl4,
                              exposure_cat4b, months_migrant,
                              hivpos, transplant,
                              ltbi_treatment, qfn_result, tspot_result) %>%
      rename(transplant_assumed=transplant)
    
    
    return(base1)
  })
  
  
  
  ### Get predictions object
  
  preds <- reactive({
    
    #req(input$t_hor)
    cohort_size<-input$n
    time_horizon<-input$t_hor
    camp_cost<-input$cost_camp
    test_cost<-input$cost_test
    tpt_cost<-input$cost_tpt
    tpt_cost_ltfup<-input$cost_ltfup/100
    start_tpt<-input$casc10/100
    complete_tpt<-input$casc2/100
    tpt_eff<-0
    if (input$tpt == "6INH"){
      tpt_eff <- 0.6  
    } else if(input$tpt =="3HP"){
      tpt_eff <- 0.53 
    } else if(input$tpt =="3RH"){
      tpt_eff <- 0.6 
    }
    
    #Define variables
    base1<-df()
    npositives<-length(which(base1$result=="Positive"))
    
    # Yearly predictions
    predictions<-matrix(0,nrow = cohort_size, ncol = time_horizon)
    predictions_low<-matrix(0,nrow = cohort_size, ncol = time_horizon)
    predictions_high<-matrix(0,nrow = cohort_size, ncol = time_horizon)
    
    
    
    for (ii in 2:time_horizon){
      base1$studytime <- (365*ii)-42
      preds <- as.data.frame((predict(model, base1, type="fail", se.fit=T)))
      predictions[,ii]<- preds[,1]
      predictions_low[,ii]<- preds[,2]
      predictions_high[,ii]<- preds[,3]
    }
    
    predictions_cost = predictions*cohort_size* tbtx_cost_yr
    predictions_cost_low= predictions_low*cohort_size* tbtx_cost_yr
    predictions_cost_high= predictions_high*cohort_size* tbtx_cost_yr
    
    predictions_itv=predictions*tpt_eff*cohort_size
    predictions_low_itv=predictions_low*tpt_eff*cohort_size
    predictions_high_itv=predictions_high*tpt_eff*cohort_size
    
    start_cost= test_cost*cohort_size + 
      npositives*start_tpt*complete_tpt*tpt_cost + 
      npositives*start_tpt*(1-complete_tpt)*tpt_cost*tpt_cost_ltfup + camp_cost
    
    predictions_cost_itv=predictions*tpt_eff*cohort_size*tbtx_cost_yr + start_cost
    predictions_cost_low_itv=predictions_low*tpt_eff*cohort_size*tbtx_cost_yr + start_cost
    predictions_cost_high_itv=predictions_high*tpt_eff*cohort_size*tbtx_cost_yr + start_cost
    
    
    out<-list(
      # Useful objects 
      database=base1,
      npositives=npositives,
      
      # Epi predictions baseline
      predictions = predictions,
      predictions_low = predictions_low,
      predictions_high = predictions_high,
      
      # Cost pedictions baseline
      predictions_cost =predictions_cost,
      predictions_cost_low= predictions_cost_low,
      predictions_cost_high= predictions_cost_high,
      
      # Epi pedictions intervention
      predictions_itv=predictions_itv,
      predictions_low_itv=predictions_low_itv,
      predictions_high_itv=predictions_high_itv,
      
      # Cost pedictions intervention
      start_cost= start_cost,
      predictions_cost_itv=predictions_cost_itv,
      predictions_cost_low_itv=predictions_cost_low_itv,
      predictions_cost_high_itv=predictions_cost_high_itv,
      
      # cases averted
      casesaverted= predictions_itv-predictions*cohort_size,
      casesaverted_low= predictions_low_itv-predictions_low*cohort_size,
      casesaverted_high= predictions_high_itv-predictions_high*cohort_size,
      
      # Costs saved
      costsaved=  predictions_cost_itv-predictions_cost,
      costsaved_low= predictions_cost_low_itv - predictions_cost_low,
      costsaved_high= predictions_cost_high_itv - predictions_cost_high)
    
    
    return(out)
    
  })
  
  # Incidence plot
  output$plot_inc <- renderPlot({
    
    req(input$t_hor)
    cohort_size<-input$n
    time_horizon<-input$t_hor
    pred<-preds()
    npositives<-pred$npositives
    
    
    ################### Plots
    # Base line incidence
    
    qtls <- as.data.frame(
      rowQuantiles(t(pred$predictions),
                   probs = c(0.5)))
    qtls_low <- as.data.frame(
      rowQuantiles(t(pred$predictions_low),
                   probs = c(0.5)))
    qtls_high <- as.data.frame(
      rowQuantiles(t(pred$predictions_high),
                   probs = c(0.5)))
    
    df<-bind_cols(qtls*cohort_size,qtls_low*cohort_size,qtls_high*cohort_size)
    colnames(df)<-paste(c("mean","low","high"))
    df$x<-1:time_horizon
    
    GIcol<-"#69b3a2"
    inc_plot<- ggplot(data = df, aes(x = x)) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = GIcol, alpha = 0.2) +
      geom_line(aes(y = mean), col = GIcol, lwd = 1) +
      labs(title = "Expected TB cases over time horizon ", x = "year after test", y = "Incident cases") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        axis.text = element_text(colour = "black", size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11), legend.key = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)
      )
    
    
    ##### Intervention test and treat positives   
    qtls <- as.data.frame(
      rowQuantiles(t(pred$predictions_itv),
                   probs = c(0.5)))
    qtls_low <- as.data.frame(
      rowQuantiles(t(pred$predictions_low_itv),
                   probs = c(0.5)))
    qtls_high <- as.data.frame(
      rowQuantiles(t(pred$predictions_high_itv),
                   probs = c(0.5)))
    
    df<-bind_cols(qtls,qtls_low,qtls_high)
    colnames(df)<-paste(c("mean","low","high"))
    df$x<-1:time_horizon
    
    GIcol<-"#69b3a2"
    inc_itv <- ggplot(data = df, aes(x = x)) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = GIcol, alpha = 0.2) +
      geom_line(aes(y = mean), col = GIcol, lwd = 1) +
      labs(title = "Expected TB cases over time horizon in scenario 1", x = "year after test", y = "Incident cases") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        axis.text = element_text(colour = "black", size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11), legend.key = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)
      )
    
    
    ## Cases averted
    
    qtls <- as.data.frame(
      rowQuantiles(t(pred$casesaverted),
                   probs = c(0.5)))
    qtls_low <- as.data.frame(
      rowQuantiles(t(pred$casesaverted_low),
                   probs = c(0.5)))
    qtls_high <- as.data.frame(
      rowQuantiles(t(pred$casesaverted_high),
                   probs = c(0.5)))
    
    df<-bind_cols(qtls,qtls_low,qtls_high)
    colnames(df)<-paste(c("mean","low","high"))
    df$x<-1:time_horizon
    
    GIcol<-"#69b3a2"
    cases_av <- ggplot(data = df, aes(x = x)) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = GIcol, alpha = 0.2) +
      geom_line(aes(y = mean), col = GIcol, lwd = 1) +
      labs(title = "Cases averted with intervention", x = "year after test", y = "Cases averted") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        axis.text = element_text(colour = "black", size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11), legend.key = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)
      )
    
    
    
    gridExtra::grid.arrange(inc_plot,  inc_itv,cases_av)
    
    
  })
  
  
  ### Costs plots
  
  output$plot_costs <- renderPlot({
    
    #req(input$t_hor)
    cohort_size<-input$n
    time_horizon<-input$t_hor
    pred<-preds()
    npositives<-pred$npositives
    
    # Base line cost
    
    qtls <- as.data.frame(
      rowQuantiles(t(pred$predictions_cost),
                   probs = c(0.5)))
    qtls_low <- as.data.frame(
      rowQuantiles(t(pred$predictions_cost_low),
                   probs = c(0.5)))
    qtls_high <- as.data.frame(
      rowQuantiles(t(pred$predictions_cost_high),
                   probs = c(0.5)))
    
    df<-bind_cols(qtls,qtls_low,qtls_high)
    colnames(df)<-paste(c("mean","low","high"))
    df$x<-1:time_horizon
    
    costcol<-"#FF3333"
    cost <- ggplot(data = df, aes(x = x)) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = costcol, alpha = 0.2) +
      geom_line(aes(y = mean), col = costcol, lwd = 1) +
      labs(title = "Expected costs in baseline (not testing or TPT) ", x = "year after test", y = "£") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        axis.text = element_text(colour = "black", size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11), legend.key = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)
      )
    
    
    
    
    #### Cost interventions
    qtls <- as.data.frame(
      rowQuantiles(t(pred$predictions_cost_itv),
                   probs = c(0.5)))
    qtls_low <- as.data.frame(
      rowQuantiles(t(pred$predictions_cost_low_itv),
                   probs = c(0.5)))
    qtls_high <- as.data.frame(
      rowQuantiles(t(pred$predictions_cost_high_itv),
                   probs = c(0.5)))
    
    df<-bind_cols(qtls,qtls_low,qtls_high)
    colnames(df)<-paste(c("mean","low","high"))
    df$x<-1:time_horizon
    
    costcol<-"#FF3333"
    cost_itv <- ggplot(data = df, aes(x = x)) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = costcol, alpha = 0.2) +
      geom_line(aes(y = mean), col = costcol, lwd = 1) +
      labs(title = "Expected costs in scenario ", x = "year after test", y = "£") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        axis.text = element_text(colour = "black", size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11), legend.key = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)
      )
    
    
    ## Costs saved
    
    
    
    qtls <- as.data.frame(
      rowQuantiles(t(pred$costsaved),
                   probs = c(0.5)))
    qtls_low <- as.data.frame(
      rowQuantiles(t(pred$costsaved_low),
                   probs = c(0.5)))
    qtls_high <- as.data.frame(
      rowQuantiles(t(pred$costsaved_high),
                   probs = c(0.5)))
    
    df<-bind_cols(qtls,qtls_low,qtls_high)
    colnames(df)<-paste(c("mean","low","high"))
    df$x<-1:time_horizon
    
    costcol<-"#FF3333"
    cost_saved <- ggplot(data = df, aes(x = x)) +
      geom_ribbon(aes(ymin = low, ymax = high), fill = costcol, alpha = 0.2) +
      geom_line(aes(y = mean), col = costcol, lwd = 1) +
      labs(title = "Expected costs in scenario ", x = "year after test", y = "£") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        axis.text = element_text(colour = "black", size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11), legend.key = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)
      )
    
    gridExtra::grid.arrange(cost, cost_itv,cost_saved)
    
    
  })
  
  
  ## Get ICERs 
  
  
  ### ICER plots
  
  
  icer_object <- eventReactive(input$add,{
    
    
    
    #req(input$t_hor)
    set.seed(131)
    n_samples <- 1000
    
    estBetaParams <- function(mu, var) {
      alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
      beta <- alpha * (1 / mu - 1)
      return(params = list(alpha = alpha, beta = beta))
    }
    tpt_eff<-0
    if (input$tpt == "6INH"){
      tpt_eff <- 0.6  
    } else if(input$tpt =="3HP"){
      tpt_eff <- 0.53 
    } else if(input$tpt =="3RH"){
      tpt_eff <- 0.6 
    }
    cohort_size<-input$n
    time_horizon<-input$t_hor
    will_to_pay=input$will_to_pay
    # camp_cost<-input$cost_camp
    # camp_costmin<-input$cost_campmin
    # camp_costmax<-input$cost_campmax
    # mean_camp_cost<-input$cost_camp
    # sd_camp_cost<- (camp_costmax-camp_costmin)/3.92 
    # shape_camp_cost<- (mean_camp_cost^2)/(sd_camp_cost^2)
    
    n<-1000
    # Campaign
    if (input$campcost_dist =="Gamma"){
      xmean<-input$cost_camp_gamma
      sd<- input$cost_campsd_gamma#(xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      sim_camp_cost  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$campcost_dist =="PERT"){
      xmean<-input$cost_camp_pert
      xmin<-input$cost_campmin_pert
      xmax<-input$cost_campmax_pert
      lam<- input$cost_camplam
      sim_camp_cost<-rpert(n,xmin,xmax,xmean,lam)
    }
    
    
    # 
    # #sim_camp_cost  <- rgamma(n_samples,shape=shape_camp_cost, rate=shape_camp_cost/mean_camp_cost)
    # sim_camp_cost<-rpert(n_samples,camp_costmin,camp_costmax,camp_cost)
    # 
    
    
    ## test
    
    if (input$testcost_dist =="Gamma"){
      xmean<-input$cost_test_gamma
      #xmin<-input$cost_testmin_gamma
      #xmax<-input$cost_testmax_gamma
      sd<- input$cost_testsd_gamma# (xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      sim_test_cost  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$testcost_dist =="PERT"){
      xmean<-input$cost_test_pert
      xmin<-input$cost_testmin_pert
      xmax<-input$cost_testmax_pert
      lam<- input$cost_testlam
      sim_test_cost<-rpert(n,xmin,xmax,xmean,lam)
    }
    
    
    # test_cost<-input$cost_test
    # test_costmin<-input$cost_testmin
    # test_costmax<-input$cost_testmax
    # mean_test_cost<-input$cost_test
    # sd_test_cost<- (test_costmax-test_costmin)/3.92 
    # shape_test_cost<- (mean_test_cost^2)/(sd_test_cost^2)
    # #sim_test_cost  <- rgamma(n_samples,shape=shape_test_cost, rate=shape_test_cost/mean_test_cost)
    # sim_test_cost<-rpert(n_samples,test_costmin,test_costmax,test_cost)
    # 
    
    
    ## TPT cost
    if (input$tptcost_dist =="Gamma"){
      xmean<-input$cost_tpt_gamma
      # xmin<-input$cost_tptmin_gamma
      # xmax<-input$cost_tptmax_gamma
      sd<- input$cost_tptsd_gamma#(xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      sim_tpt_cost  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$tptcost_dist =="PERT"){
      
      xmean<-input$cost_tpt_pert
      xmin<-input$cost_tptmin_pert
      xmax<-input$cost_tptmax_pert
      lam<- input$cost_tptlam
      sim_tpt_cost<-rpert(n,xmin,xmax,xmean,lam)
    }
    
    
    
    # tpt_cost<-input$cost_tpt
    # tpt_costmin<-input$cost_tptmin
    # tpt_costmax<-input$cost_tptmax
    # mean_tpt_cost<-input$cost_tpt
    # sd_tpt_cost<- (tpt_costmax-tpt_costmin)/3.92 
    # shape_tpt_cost<- (mean_tpt_cost^2)/(sd_tpt_cost^2)
    # #sim_tpt_cost  <- rgamma(n_samples,shape=shape_tpt_cost, rate=shape_tpt_cost/mean_tpt_cost)
    # sim_tpt_cost<-rpert(n_samples,tpt_costmin,tpt_costmax,tpt_cost)
    # 
    cfr<- 0.052
    frac_eptb<-0.2
    av_tbdur <- 2
    frac_post<-0.25
    
    # TB QOL
    if (input$ptbqol_dist =="Beta"){
      
      xmean<-input$qol_ptb_beta
      # xmin<-input$qol_ptbmin_beta
      # xmax<-input$qol_ptbmax_beta
      sd<- input$qol_ptbsd_beta# (xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      sim_tb_qol <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$ptbqol_dist =="PERT"){
      
      xmean<-input$qol_ptb_pert
      xmin<-input$qol_ptbmin_pert
      xmax<-input$qol_ptbmax_pert
      lam<- input$qol_ptblam_pert
      sim_tb_qol<-rpert(n,xmin,xmax,xmean,lam)
      
    }
    
    
    # tb_qol<-input$qol_ptb
    # tb_qolmin<-input$qol_ptbmin
    # tb_qolmax<-input$qol_ptbmax
    # mean_tb_qol<-input$qol_ptb
    # sd_tb_qol<- (tb_qolmax-tb_qolmin)/3.92 
    # pars_beta<-estBetaParams(mean_tb_qol,sd_tb_qol^2 )
    # #sim_tb_qol  <- rbeta(n_samples,pars_beta$alpha, pars_beta$beta)
    # sim_tb_qol  <- rpert(n_samples,tb_qolmin,tb_qolmax,tb_qol)
    
    
    
    #EPTB QoL
    if (input$eptbqol_dist =="Beta"){
      
      xmean<-input$qol_eptb_beta
      # xmin<-input$qol_eptbmin_beta
      # xmax<-input$qol_eptbmax_beta
      sd<- input$qol_eptbsd_beta#(xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      sim_eptb_qol  <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$eptbqol_dist =="PERT"){
      
      xmean<-input$qol_eptb_pert
      xmin<-input$qol_eptbmin_pert
      xmax<-input$qol_eptbmax_pert
      lam<- input$qol_eptblam_pert
      sim_eptb_qol <-rpert(n,xmin,xmax,xmean,lam)
      
    }
    
    
    # eptb_qol<-input$qol_eptb
    # eptb_qolmin<-input$qol_eptbmin
    # eptb_qolmax<-input$qol_eptbmax
    # mean_eptb_qol<-input$qol_eptb
    # sd_eptb_qol<- (eptb_qolmax-eptb_qolmin)/3.92 
    # pars_beta<-estBetaParams(mean_tb_qol,sd_tb_qol^2 )
    # #sim_eptb_qol  <- rbeta(n_samples,pars_beta$alpha, pars_beta$beta)
    # sim_eptb_qol  <- rpert(n_samples,eptb_qolmin,eptb_qolmax,eptb_qol)
    
    # POstTB QoL
    if (input$postqol_dist =="Beta"){
      
      xmean<-input$qol_post_beta
      # xmin<-input$qol_postmin_beta
      # xmax<-input$qol_postmax_beta
      sd<- input$qol_postsd_beta#(xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      sim_post_qol <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$postqol_dist =="PERT"){
      
      xmean<-input$qol_post_pert
      xmin<-input$qol_postmin_pert
      xmax<-input$qol_postmax_pert
      lam<- input$qol_postlam_pert
      sim_post_qol<-rpert(n,xmin,xmax,xmean,lam)
      
    }
    # post_qol<-input$qol_post
    # post_qolmin<-input$qol_postmin
    # post_qolmax<-input$qol_postmax
    # mean_post_qol<-input$qol_post
    # sd_post_qol<- (post_qolmax-post_qolmin)/3.92 
    # pars_beta<-estBetaParams(mean_post_qol,sd_post_qol^2 )
    # #sim_post_qol  <- rbeta(n_samples,pars_beta$alpha, pars_beta$beta)
    # sim_post_qol  <- rpert(n_samples,post_qolmin,post_qolmax,post_qol)
    
    
    #AE TPT QOL
    if (input$aeqol_dist =="Beta"){
      
      xmean<-input$qol_ae_beta
      # xmin<-input$qol_aemin_beta
      # xmax<-input$qol_aemax_beta
      sd<- input$qol_aesd_beta#(xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      sim_ae_qol <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$aeqol_dist =="PERT"){
      
      xmean<-input$qol_ae_pert
      xmin<-input$qol_aemin_pert
      xmax<-input$qol_aemax_pert
      lam<- input$qol_aelam_pert
      sim_ae_qol<-rpert(n,xmin,xmax,xmean,lam)
      
    }
    
    
    #Define variables
    base1<-df()
    npositives<-length(which(base1$result=="Positive"))
    
    # Yearly predictions
    predictions<-matrix(0,nrow = cohort_size, ncol = time_horizon)
    predictions_sd<-matrix(0,nrow = cohort_size, ncol = time_horizon)
    
    for (ii in 2:time_horizon){
      base1$studytime <- (365*ii)-42
      #head(base1)
      preds <- as.data.frame((predict(model, base1, type="fail", se.fit=T)))
      p_in<-preds[,1]
      predictions[,ii]<-p_in 
      predictions_sd[,ii]<- (preds[,3] - preds[,2])/3.92
      
    }
    
    mean_prob_tb<-predictions[,time_horizon]
    sd_prob_tb<-predictions_sd[,time_horizon]
    tb_prob<-rnorm(mean_prob_tb,mean_prob_tb,sd_prob_tb)
    tb_prob[which(tb_prob<0)]<-0
    m_prob<-matrix(replicate(n_samples,tb_prob),n_samples)
    
    sim_cases<-matrix(rbinom(length(m_prob),prob=m_prob,size=1),
                      nrow=nrow(m_prob)) %>% rowSums()
    
    sim_cases_itv<-(sim_cases * tpt_eff)
    
    
    deaths<-rbinom(sim_cases,sim_cases,cfr)
    deaths_itv<-rbinom(sim_cases_itv,sim_cases,cfr)
    
    # QALY

    sim_healthy_qaly<-sim_cases*0 + cohort_size * time_horizon
    sim_healthy_qaly_itv<-sim_cases_itv*0 + cohort_size * time_horizon
    
    sim_tb_qaly <- sim_cases*(1- frac_eptb) * sim_tb_qol * av_tbdur
    sim_eptb_qaly <- sim_cases*(frac_eptb) * sim_eptb_qol * av_tbdur
    
    sim_tb_qaly_itv <- sim_cases_itv*(1- frac_eptb) * sim_tb_qol * av_tbdur
    sim_eptb_qaly_itv <- sim_cases_itv*(frac_eptb) * sim_eptb_qol * av_tbdur
    
    sim_post_qaly <- sim_cases*(1- frac_eptb) * frac_post *  sim_post_qol * time_horizon/2
    sim_post_qaly_itv <- sim_cases_itv*(1-frac_eptb) * frac_post* sim_post_qol * time_horizon/2
    
    qaly<- sim_healthy_qaly - sim_tb_qaly - sim_eptb_qaly - sim_post_qaly - deaths
    qaly_itv<- sim_healthy_qaly_itv - sim_tb_qaly_itv - sim_eptb_qaly_itv - sim_post_qaly_itv - deaths_itv
    
    
    # Costs
    start_cost<- sim_test_cost*cohort_size + npositives*sim_tpt_cost + sim_camp_cost
    
    sim_cost <- sim_cases * tbtx_cost_yr
    sim_cost_itv <- start_cost + sim_cases * tbtx_cost_yr
    
    # ICER  objects
    treats=c("No intervention", "Intervention")
    eff=cbind(qaly,qaly_itv)
    cost=cbind(sim_cost,sim_cost_itv)
    
    out <-list(
      cohort_size=input$n,
      time_horizon=input$t_hor,
      will_to_pay=input$will_to_pay,
      treats=treats,
      eff=eff,
      cost=cost,
      bcea_tb=bcea(eff,cost, ref=2,interventions=treats)
    )
    
    return(out)
    
  })
  
  
  # Icer plane plot
  
  observeEvent(input$add, {
    
    
    output$ICER <- renderValueBox({
      #req(input$will_to_pay)
      will_to_pay=input$will_to_pay
      obj<-icer_object()
      bceares<- obj$bcea_tb
      icer<-bceares$ICER
      
      valueBox(
        value = formatC(icer, digits = 0, format = "f"),
        subtitle = "ICER (£/QALY)",
        icon = icon("bullseye"),
        color = if (icer <= will_to_pay) "yellow" else "red"
      )
    })
    
    
    
    output$qalygained <- renderValueBox({
      obj<-icer_object()
      bceares<- obj$bcea_tb
      x<-mean(unlist(obj$bcea_tb$delta_c),na.rm=TRUE)
      valueBox(
        value = formatC(x, digits = 0, format = "f"),
        subtitle = "Mean Incremental cost (£)",
        icon = icon("sterling-sign"),
        color =  "aqua"
      )
    })
    
    
    output$costsaved <- renderValueBox({
      obj<-icer_object()
      bceares<- obj$bcea_tb
      x<-mean(unlist(obj$bcea_tb$delta_e),na.rm=TRUE)
      valueBox(
        value = formatC(x, digits = 0, format = "f"),
        subtitle = "Mean Incremental QALY",
        icon = icon("person"),
        color =  "aqua"
      )
    })
    
    
    output$plot_icer <- renderPlot({
      #req(input$will_to_pay)
      obj<-icer_object()
      bceares<- obj$bcea_tb
      will_to_pay=input$will_to_pay
      icercol<-"#FF3333"
      plot(bceares, 
           wtp =will_to_pay,
           graph="ggplot2",
           line = list(color = "grey40"),
           point = list(color = "blue"),
           icer = list(color = icercol, size = 3),
           area = list(fill = "lightcyan"),
           theme = theme_minimal())
      
    })
    
    output$plot_contour <- renderPlot({
      #req(input$will_to_pay)
      obj<-icer_object()
      bceares<- obj$bcea_tb
      will_to_pay=input$will_to_pay
      
      contour2(bceares, 
               wtp =will_to_pay,
               graph = "ggplot2",
               xlabel = "Incremental QALY" )
    })
    
    output$plot_wtp <- renderPlot({
      # req(input$will_to_pay)
      obj<-icer_object()
      bceares<- obj$bcea_tb
      icers<- bceares$delta_c/bceares$delta_e
      val1<-length(which(icers<20000))/dim(icers)[1] 
      val2<-length(which(icers<30000))/dim(icers)[1]
      
      
      
      
      df<-data.frame(
        pos = c(1 ,3),
        val = c(val1*1e2,val2*1e2),
        cat = c("£20k","£30k")
      )
      
      p<-ggplot(df, aes(x=cat, y=val))+
        geom_bar(stat="identity", fill=c('firebrick',"darkgreen"),  alpha=0.35)+
        ylim(c(0,100))+
        geom_segment(aes(x = 0.75, y = 50, xend = 1.25, yend = 50), size=1,linetype='dotted', col = 'firebrick')+
        geom_segment(aes(x = 1.75, y = 90, xend = 2.25, yend = 90),size=1,linetype='dotted', col = 'darkgreen')+
        labs(
          title = "Cost-effectiveness thresholds at £20k and £30k per QALY"
        )+
        xlab(" ")+
        ylab("Simulations under specific WTP (%)")+
        theme_minimal()+
        theme(
          text = element_text(size=20),
          axis.text.x = element_text(angle=60, hjust=1))
      
      
      gridExtra::grid.arrange(p)  
      
    })
    
    ## Produce report
    
    output$btn <- downloadHandler(
      
      filename = function(){"myreport.pdf"},
      content = function(file) {
        
        tempReport <- file.path(tempdir(),"markdown_template.Rmd")
        file.copy("markdown_template.Rmd", tempReport, overwrite = TRUE)
        rmarkdown::render("markdown_template.Rmd", output_format = "pdf_document", output_file = file,
                          params = list(data = icer_object()), # here I'm passing data in params
                          envir = new.env(parent = globalenv()),clean=F,encoding="utf-8"
        )
        
        
      }
    )
    
    
    
  })
  
  
  
  
}

shinyApp(ui, server)


shinyApp(ui = ui, server = server)
