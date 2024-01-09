
rm(list = ls()) 

require(shiny)
require(shinythemes)
require(xlsx)
library(shinydashboard)
library(shinyvalidate)
library(DT)
library(tidyverse)
library(reshape2)
library(rstpm2)
library(rms)
library(lubridate)
library(glue)
library(matrixStats)
library(magrittr) 
library(data.table)
library(BCEA)
library(ggplot2)
library(purrr)



# path_app<-rstudioapi::getSourceEditorContext()$path
# setwd(gsub('/app.R','', path_app))


# Load necessary input ----------------------------------------------------


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

################# required data for QALY app (from https://github.com/LSHTM-GHECO/COVID19_QALY_App/tree/master) 
q.male <- as.data.table(read.xlsx("Inputs/inputs.xlsx", 1))
q.female <- as.data.table(read.xlsx("Inputs/inputs.xlsx", 2)) 
qol <- as.data.table(read.xlsx("Inputs/inputs.xlsx", 3))
covid.age <- as.data.table(read.xlsx("Inputs/inputs.xlsx", 4))



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
tbtx_cost_yr <- 6000 ## per year


## QoL from Kind eta l 1999
qol_full <- t(data.frame("A_16to35"=c(0.93),
                         "A_36to45"=c(0.91),
                         "A_46to65"=c(0.82),
                         "A_65plus"=c(0.75)))



X <- Categorical(rownames(age_uk), p = age_uk/100)


# UI  Menu---------------------------------------------------------------------
ui <- dashboardPage(
  
  
   skin = "black",
  dashboardHeader(title = "LTBI CEA Tool", tags$li(class = "dropdown", 
                                                   actionButton("home","Home", icon = icon("house")))),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Home", tabName = "home", icon = icon("house")),
                menuItem("Single Run", tabName = "single", icon = icon("bar-chart"), startExpanded = TRUE,
                         menuSubItem("How to...", tabName = "how", icon = icon("check")),
                         menuSubItem("Demographics", tabName = "epi", icon = icon("bar-chart")),
                         #menuSubItem("LTBI tests", tabName = "ltbi", icon = icon("vial")),
                         menuSubItem("LTBI cascade", tabName = "cascade", icon = icon("chart-simple")),
                         menuSubItem("Costs", tabName = "cost", icon = icon("sterling-sign")),
                         menuSubItem("QoL", tabName = "qol", icon = icon("staff-snake")),
                         #menuSubItem("TB Incidence", tabName = "res_inc"),
                         #menuSubItem("Costs", tabName = "res_costs"),
                         menuSubItem("ICER", tabName = "res_icer", icon = icon("bullseye"))
                ),
                menuItem("Scenarios", icon = icon("bar-chart"), startExpanded = TRUE,
                         menuSubItem("How to...", tabName = "how_scen"),
                         menuSubItem("Comparison", tabName = "comp")
                ),
                menuItem("Advanced", icon = icon("bar-chart"), startExpanded = TRUE,
                         menuSubItem("How to...", tabName = "how_adv"),
                         menuSubItem("Batch", tabName = "batch")
                ),
                menuItem("About", tabName = "about", icon = icon("th"))
    )
  ),
  
  # UI body---------------------------------------------------------------------
  
  dashboardBody(
    tabItems(
      
      
      # UI home -----------------------------------------------------------------
      
      
      tabItem(tabName = "home",
              
              
              
              
              fluidPage(
                
                theme = shinythemes::shinytheme("flatly"),
                tags$head(
                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                ),
                
                
                h1("TB infection test cost-effectiveness calculator",style = "text-align: center"),
                br(),
                br(),
                p("This tool was created to help quantify and visualise the 
                  potential impact of delivering TB infection tests and
                  TB preventive regimens among immigrant populations in the UK.",
                  style = "text-align: justify; font-size:18px"),
                p("Three different modalities for analysis are available: 
                'Single Run', 'Scenarios', and 'Advanced'.",style = "text-align: justify; font-size:18px"),
                p("Choose your prefered mode and 
                navigate using the 'Go' button.",style = "text-align: justify; font-size:18px"),
                
                br(),
                br(),
                br(),
                br(),
                
                box(
                  style = "text-align: justify; font-size:18px",
                  title = "Single Run",
                  width = 4, background = "navy",
                  p("Define a baseline cohort, input epidemiological and cost
                  parameterers to retrieve estimations of expected TB disease cases, 
                  cost projections of the testing intervention and a full set of
                  cost-effectiveness analysis output."),
                  tags$p( actionButton("single", "Go", class = "dark")),
                  
                ),
                box(
                  style = "text-align: justify; font-size:18px",
                  title = "Scenarios",
                  width = 4, background = "orange",
                  p("Upload pre-populated scenarios created with this tool -or 
                    add new ones using the csv template, and run a scenario 
                    comparison analysis"),
                  tags$p(actionButton("compare", "Go", class = "dark"))
                ),
                box(
                  style = "text-align: justify; font-size:18px",
                  title = "Advanced",
                  width = 4, background = "maroon",
                  p("Upload multiple scenarios using batch files, and download 
                    simulation results as .csv"),
                  tags$p( actionButton("advanced", "Go", class = "dark"))
                )
              ),
      ),
      
      
      
      
      # UI single: how  ---------------------------------------------------------
      
      
      tabItem(tabName = "how",
              
              
              fluidPage(
                h2("How to use this tool"),
                
                fluidRow(column(12, div(style="display: inline-block;",
                                        tags$p("1) Define the size, demographics,
                                              and expected positivity of the LTBI test  
                                              in the baseline cohort" , 
                                               style = "font-size:18px")),
                                actionButton("demog", "Demographics", 
                                             icon = icon("bar-chart"),
                                             class = "light"))),
                
                fluidRow(column(12, div(style="display: inline-block;",
                                        tags$p("2) Select your choice of LTBI 
                                        test and define the steps in the cascade
                                               from test to regimen completion" , 
                                               style = "font-size:18px")),
                                actionButton("ltbicasc", "LTBI cascade", 
                                             icon = icon("chart-simple"),
                                             class = "light"))),
                
                
                fluidRow(column(12, div(style="display: inline-block;",
                                        tags$p("3) Select costs for relevant 
                                        TB outcomes. Here you can define the 
                                               shape of the background distributions for these inputs" , 
                                               style = "font-size:18px")),
                                actionButton("cost_tab", "Costs", 
                                             icon = icon("sterling-sign"),
                                             class = "light"))),
                
                fluidRow(column(12, div(style="display: inline-block;",
                                        tags$p("4) Select Qality of Life (QoL)
                                        for relevant TB outcomes. Here you can define the 
                                               shape of the background distributions for these inputs" , 
                                               style = "font-size:18px")),
                                actionButton("qol_tab", "QoL", 
                                             icon = icon("staff-snake"),
                                             class = "light"))),
                
                
                p("5) Choose a time-horizon for analysis in the slider below",style = "font-size:18px"),
                
                sliderInput("t_hor", "Time-horizon (years)", 2, 50, 5),


                p("6) Choose a discount rate (applied to costs and QoL) in the slider below",style = "font-size:18px"),
                
                numericInput("disc_rate", em("Discount rate (0%-10%)"), 3.5, min = 0, 
                             max = 100),
                
                
                fluidRow(column(12, div(style="display: inline-block;",
                                        tags$p("7) Run CEA analsys using the run 
                                        button in the ICER panel. Here you can 
                                               download a report of your scenario" , 
                                               style = "font-size:18px")),
                                actionButton("icer_tab", "ICER", 
                                             icon = icon("bullseye"),
                                             class = "light")))
              )
      ),
      
      
      # UI single: Demog  ---------------------------------------------------------
      tabItem(tabName = "epi",
              
              tabsetPanel(type = "tabs", 
                          tabPanel("Demographics",
              
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
                
              )),
              
              tabPanel("HIV prevalence",
              
              fluidRow(
                box(
                  title = "HIV prevalence (%)",
                  sliderInput("hiv_prev4", "65+", 0, 100, 0),
                  sliderInput("hiv_prev3", "46-65", 0, 100, 0),
                  sliderInput("hiv_prev2", "36-45 ", 0, 100, 0),
                  sliderInput("hiv_prev1", "16-35 ", 0, 100, 0),
                ),
                
                
                box(plotOutput("hiv_prevPlot")),
                
                
              )),
              
              tabPanel("LTBI prevalence",
              
              fluidRow(
                
                box(
                  title = "Expected TB infection prevalence (%)",
                  sliderInput("prev4", "65+", 0, 100, prev_uk[4]),
                  sliderInput("prev3", "46-65", 0, 100, prev_uk[3]),
                  sliderInput("prev2", "36-45 ", 0, 100, prev_uk[2]),
                  sliderInput("prev1", "16-35 ", 0, 100, prev_uk[1]),
                ),
                
                box(plotOutput("prevPlot")),
                
              )
              ))
      ),
      
      # UI single: LTBI casc ----------------------------------------------------
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
      
      
      # UI single: Costs ----------------------------------------------------
      tabItem(tabName = "cost",
              
              tabsetPanel(type = "tabs", 
                          tabPanel("LTBI test",
              
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
              )
                          ),
              
              tabPanel("Test campaign",
                       
              # Campaign cost
              fluidRow(  
                box(width = 4,
                    title="Overall cost (£) - Incurred only at start",
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
              )
              ),
              
              
              # cost TPT
              tabPanel("TPT regimen",
                       
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
                  sliderInput("cost_ltfup", "(%)", 0, 100, 0.5),
                ),
              ),
              )
              )
              
      ),
      
      # UI single: QoL ----------------------------------------------------
      tabItem(tabName = "qol",
              
              tabsetPanel(type = "tabs", 
                          tabPanel("Baseline QoL",
                                   
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
              )
              ),
              
              # PTB QoL
              tabPanel("Pulmonary TB",
                       
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
              )
              ),
              
              tabPanel("Extra-pulmonary TB",
                       
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
              )
              ),
              
              tabPanel("Post-TB",
                       
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
              )
              ),
              
              # QoL TPT Adevrese events (AE)
              tabPanel("TPT adverse events",
                       
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
              )
              )
              
      ),
      
      # UI single: Incidence plots ----------------------------------------------------
      tabItem(tabName = "res_inc",
              fluidRow(column(9,
                              box(width=12,
                                  plotOutput("plot_inc")
                              )
              )
              )
              
      ),
      
      # UI single: Cost plots ----------------------------------------------------
      
      tabItem(tabName = "res_costs",
              fluidRow(column(9,
                              box(width=12,
                                  plotOutput("plot_costs")
                              )
              )
              )
      ),
      
      # UI single: ICER plane ----------------------------------------------------
      
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
                downloadButton("btn", "Generate Report"),
                downloadButton("btncsv", "Download scenario parameters (.csv)")
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
      
      # UI scenarios: how... ----------------------------------------------------
      
      tabItem(tabName = "how_scen",
              fluidPage(
                p("INPUT PARAMETERS FROM .CSV: Saved scenario parameters can be 
                uploaded again with the button below. Select a file to upload and 
                go straight to the ICER panel",
                  style = "font-size:18px"),
                
                fileInput("upload", "Upload scenario parameters")
              )
      ),
      
      # UI advanced: how... ----------------------------------------------------
      tabItem(tabName = "how_adv",
              fluidPage(
                p("INPUT PARAMETERS FROM .CSV: Saved scenario parameters can be 
                uploaded again with the button below. Select a file to upload and 
                go straight to the ICER panel",
                  style = "font-size:18px"),
                
                fileInput("upload", "Upload scenario parameters")
              )
      ),
      
      
      
      # UI About ----------------------------------------------------
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
                
                p("About this tool",style = "font-size:25px"),
                
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



# server Function ------------------------------------------------------------------


server <- function(input, output,session) {
  
  # Home button tag
  observeEvent(input$home, {
    updateTabItems(session, "sidebar", "home")
  })
  
  # Single run button tag
  observeEvent(input$single, {
    updateTabItems(session, "sidebar", "how")
  })
  
  # Compare button tag
  observeEvent(input$compare, {
    updateTabItems(session, "sidebar", "how_scen")
  })
  
  # Compare button tag
  observeEvent(input$advanced, {
    updateTabItems(session, "sidebar", "how_adv")
  })
  
  # Compare button tag
  observeEvent(input$demog, {
    updateTabItems(session, "sidebar", "epi")
  })
  
  # Compare button tag
  observeEvent(input$ltbicasc, {
    updateTabItems(session, "sidebar", "cascade")
  })
  
  # Compare button tag
  observeEvent(input$cost_tab, {
    updateTabItems(session, "sidebar", "cost")
  })
  
  # Compare button tag
  observeEvent(input$qol_tab, {
    updateTabItems(session, "sidebar", "qol")
  })
  
  # Compare button tag
  observeEvent(input$icer_tab, {
    updateTabItems(session, "sidebar", "res_icer")
  })
  

  # Read input tables ------------------------------------------------------------
  
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
  
  dfage <- reactive({ 
    
    x<-data.frame(
      age=c("16-35","36-45","46-65","65+"),
      value=as.numeric(c(input$age1,input$age2,input$age3,input$age4)))
    return(x)
  })
  
  
  # Demog tab contents------------------------------------------------------------
  
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
  
  
  
  
  # Epi tab plots -----------------------------------------------------------
  
  
  
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
  
  
  
  
  
  
  # LTBI cascade ------------------------------------------------------------
  
  
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
  
  
  
  # Costs tab contents ------------------------------------------------------
  
  
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
  
  
  
  
  # QoL content -------------------------------------------------------------
  
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
  
  
  
  
  
  # Build Periskope dataset -------------------------------------------------
  
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
  
  
  
  
  # Create predictions object ------------------------------------------------
  
  
  
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
  
  
  
  # Epi plots ----------------------------------------------------------
  
  
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
    
    
    # Cases averted
    
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
  
  
  
  # Cost plots --------------------------------------------------------------
  
  
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
  
  # QALY App components -----------------------------------------------------
  
  # Reactive dependencies - if these change then MODEL will run again and update values
  xxchange <- reactive({
    paste(input$disc_rate)
  })
  
  
  QoLmodel <- eventReactive(xxchange(), {
    country <- "UK"
    smr <- 1#input$smr
    qcm <- 1
    r <- input$disc_rate/100
  
    shiny::validate(
      need(input$disc_rate <=10, "Please choose a valid discount rate between 0% and 10%"),
      need(input$disc_rate >=0, "Please choose a valid discount rate between 0% and 10%"),
    )

    myvector <- c("Age",country)
    
    l_x_est <- function(dt, countr, smr){
      ## dt = data table with q(x) vaues
      ## country = selected country
      ## smr = smr
      myvector <- c("Age",countr)
      
      y <- dt[, ..myvector]
      colnames(y) <- c("x","q_x")
      
      y[ , d_x := -log(1-y$q_x)]
      
      y[ 1, l_x := 100000] 
      
      for (i in 2:nrow(y)){
        y[i, l_x := y$l_x[[i-1]] * 
            exp((-y$d_x[[i-1]])*smr)] 
      }
      return(y)
    }
    
    q.male <- l_x_est(q.male, country, smr)
    q.female <- l_x_est(q.female, country, smr)
    
    q.person <- merge(q.male, q.female, by="x")
    colnames(q.person) <- c("x","q_male","d_male","l_male",
                            "q_female","d_female","l_female")
    q.person[ , p.f := l_female/(l_female+l_male)]
    q.person[ , l_person := (p.f*l_female)+
                ((1-p.f)*l_male)]
    
    for (i in 1:(nrow(q.person)-1)){
      q.person[i, bigl_x := (q.person$l_person[[i]]+ q.person$l_person[[i+1]])/2]
    }
    
    q.person[nrow(q.person), bigl_x := (q.person$l_person[[nrow(q.person)]])/2]
    
    for (i in 1:nrow(q.person)){
      q.person[i, t_x := sum(q.person$bigl_x[i:nrow(q.person)])]
    }
    
    q.person[ , LE_x := t_x/l_person]
    
    ########### calculating QALE ########
    myvector.qol <- c("low","high",country)
    
    dt.qol <- qol[, ..myvector.qol]
    colnames(dt.qol) <- c("low","high","qol_age")
    
    qale <- q.person[dt.qol, on = .(x >= low, x <= high), nomatch = 0,
                     .(x.x, l_person, bigl_x, t_x, LE_x,qol_age)]
    
    qale[ , z_x := bigl_x*qol_age*qcm]
    
    for (i in 1:nrow(qale)){
      qale[i , t_adj := sum(qale$z_x[i:nrow(qale)])]
    }
    
    qale[ , qale_x := t_adj/l_person]
    
    qaly.calc <- qale[ , c("x.x","z_x")]
    
    temp.q <- list()
    for (i in 1:nrow(qaly.calc)){
      temp.q[[i]] <- qaly.calc[i:nrow(qaly.calc),]
    }
    
    temp.q <- bind_rows(temp.q, .id = "column_label")
    temp.q %>% setDT() ## creating a copy as otherwise there is a warning
    ## message (still runs but just for "clean" code), so this stops attempts of .internal.selfref detected
    temp.q_copy <- copy(temp.q)
    temp.q_copy[ , column_label := as.numeric(column_label)-1]
    temp.q_copy[ , b_x := z_x/((1+r))^(x.x-(column_label))] ## n.b x.x = u and column_label = x in the corresponding formulae in the CodeBook
    
    
    total.b <- temp.q_copy[,.(bigb_x=sum(b_x)), by=column_label]
    colnames(total.b) <- c("x.x","bigb_x")
    qale <- merge(qale, total.b, by="x.x")
    
    qale[ , dQALY := bigb_x/l_person]
    
    ######### calculating covid19 loss #######
    myvector.cov <- c("low","high",country)
    
    dt.cov <- covid.age[, ..myvector.cov]
    colnames(dt.cov) <- c("low","high","cov_age")
    
    dt.cov[ , midpoint := ceiling((low+high)/2)]
    cov <- merge(qale, dt.cov, by.x="x.x", by.y="midpoint", all=FALSE)
    
    cov[ , weight.LE := cov_age*LE_x]
    cov[ , weight.qale := cov_age*qale_x]
    cov[ , weight.qaly := cov_age*dQALY]
    
    estimates <- colSums(cov)
    resultstab <- data.table("Weighted LE Loss"=estimates["weight.LE"],
                             "Weighted QALE Loss"=estimates["weight.qale"],
                             "Weighted dQALY loss"=estimates["weight.qaly"])
    ### ADDING AGE GROUP BREAKDOWN TABLE
    cov[,"Age Group":=paste(cov[,low],cov[,high],sep="-")]
    cov[ , "Age at Death (% of all deaths)" := cov_age*100]
    setnames(cov, old=c("LE_x","qale_x","dQALY"),
             new=c("LE","QALE","dQALY"))
    
    agetab <- cov[ , c("Age Group","Age at Death (% of all deaths)",
                       "LE","QALE","dQALY")]

    list(resultstab=resultstab, agetab=agetab)
  })
  
  


  # Single run ICER object -----------------------------------------------------
  
  
  
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
    cost_camp<- 0
    cost_campsd<-  0 
    cost_campmin<-  0
    cost_campmax<-  0
    cost_campshape<-  0
    
    if (input$campcost_dist =="Gamma"){
      cost_camp<- input$cost_camp_gamma
      cost_campsd<-  input$cost_campsd_gamma#(xmax-xmin)/3.92 
      cost_campmin<-  NA
      cost_campmax<-  NA
      cost_campshape<-  NA
      
      xmean<-input$cost_camp_gamma
      sd<- input$cost_campsd_gamma#(xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      sim_camp_cost  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$campcost_dist =="PERT"){
      
      cost_campsd<-  NA 
      cost_camp<-  input$cost_camp_pert
      cost_campmin<-  input$cost_campmin_pert
      cost_campmax<-  input$cost_campmax_pert
      cost_campshape<-  input$cost_camplam
      
      xmean<-input$cost_camp_pert
      xmin<-input$cost_campmin_pert
      xmax<-input$cost_campmax_pert
      lam<- input$cost_camplam
      sim_camp_cost<-rpert(n,xmin,xmax,xmean,lam)
    }
    
    #campaing
    
    
    # 
    # #sim_camp_cost  <- rgamma(n_samples,shape=shape_camp_cost, rate=shape_camp_cost/mean_camp_cost)
    # sim_camp_cost<-rpert(n_samples,camp_costmin,camp_costmax,camp_cost)
    # 
    
    
    ## test
    cost_test<- 0
    cost_testsd<-  0 
    cost_testmin<-  0
    cost_testmax<-  0
    cost_testshape<-  0
    if (input$testcost_dist =="Gamma"){
      
      cost_test<- input$cost_test_gamma
      cost_testsd<-  input$cost_testsd_gamma
      cost_testmin<-  NA
      cost_testmax<-  NA
      cost_testshape<-  NA
      
      xmean<-input$cost_test_gamma
      #xmin<-input$cost_testmin_gamma
      #xmax<-input$cost_testmax_gamma
      sd<- input$cost_testsd_gamma# (xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      sim_test_cost  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$testcost_dist =="PERT"){
      
      cost_test<-input$cost_test_pert
      cost_testsd<-  NA 
      cost_testmin<-  input$cost_testmin_pert
      cost_testmax<-  input$cost_testmax_pert
      cost_testshape<-  input$cost_testlam
      
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
    cost_tpt<- 0
    cost_tptsd<-  0 
    cost_tptmin<-  0
    cost_tptmax<-  0
    cost_tptshape<-  0
    if (input$tptcost_dist =="Gamma"){
      cost_tpt<- input$cost_tpt_gamma
      cost_tptsd<-  input$cost_tptsd_gamma 
      cost_tptmin<-  NA
      cost_tptmax<-  NA
      cost_tptshape<-  NA
      xmean<-input$cost_tpt_gamma
      # xmin<-input$cost_tptmin_gamma
      # xmax<-input$cost_tptmax_gamma
      sd<- input$cost_tptsd_gamma#(xmax-xmin)/3.92 
      shape<- (xmean^2)/(sd^2)
      sim_tpt_cost  <- rgamma(n,shape=shape, rate=shape/xmean)
      
    } else if (input$tptcost_dist =="PERT"){
      cost_tpt<- input$cost_tpt_pert
      cost_tptsd<-  NA 
      cost_tptmin<- input$cost_tptmin_pert 
      cost_tptmax<-  input$cost_tptmax_pert
      cost_tptshape<-  input$cost_tptlam
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
    
    
    qol<- QoLmodel()$agetab
   browser()
    
    # TB QOL
    qol_tb<- 0
    qol_tbsd<-  0 
    qol_tbmin<-  0
    qol_tbmax<-  0
    qol_tbshape<-  0
    if (input$ptbqol_dist =="Beta"){
      qol_tb<- input$qol_ptb_beta
      qol_tbsd<-  input$qol_ptbsd_beta 
      qol_tbmin<-  NA
      qol_tbmax<-  NA
      qol_tbshape<-  NA
      xmean<-input$qol_ptb_beta
      # xmin<-input$qol_ptbmin_beta
      # xmax<-input$qol_ptbmax_beta
      sd<- input$qol_ptbsd_beta# (xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      sim_tb_qol <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$ptbqol_dist =="PERT"){
      qol_tb<- input$qol_ptb_pert
      qol_tbsd<-  NA 
      qol_tbmin<-  input$qol_ptbmin_pert
      qol_tbmax<-  input$qol_ptbmax_pert
      qol_tbshape<- input$qol_ptblam_pert
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
    qol_eptb<- 0
    qol_eptbsd<-  0 
    qol_eptbmin<-  0
    qol_eptbmax<-  0
    qol_eptbshape<-  0
    if (input$eptbqol_dist =="Beta"){
      qol_eptb<- input$qol_eptb_beta
      qol_eptbsd<- input$qol_eptbsd_beta
      qol_eptbmin<-  NA
      qol_eptbmax<-  NA
      qol_eptbshape<- NA
      xmean<-input$qol_eptb_beta
      # xmin<-input$qol_eptbmin_beta
      # xmax<-input$qol_eptbmax_beta
      sd<- input$qol_eptbsd_beta#(xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      sim_eptb_qol  <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$eptbqol_dist =="PERT"){
      qol_eptb<- input$qol_eptb_pert
      qol_eptbsd<-  NA 
      qol_eptbmin<-  input$qol_eptbmin_pert
      qol_eptbmax<-  input$qol_eptbmax_pert
      qol_eptbshape<- input$qol_eptblam_pert
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
    qol_postb<- 0
    qol_postbsd<-  0 
    qol_postbmin<-  0
    qol_postbmax<-  0
    qol_postbshape<-  0
    if (input$postqol_dist =="Beta"){
      qol_postb<- input$qol_post_beta
      qol_postbsd<-  input$qol_postsd_beta 
      qol_postbmin<-  NA
      qol_postbmax<-  NA
      qol_postbshape<-  NA
      xmean<-input$qol_post_beta
      # xmin<-input$qol_postmin_beta
      # xmax<-input$qol_postmax_beta
      sd<- input$qol_postsd_beta#(xmax-xmin)/3.92 
      pars_beta<-estBetaParams(xmean,sd^2 )
      sim_post_qol <- rbeta(n,pars_beta$alpha, pars_beta$beta)
      
    } else if (input$postqol_dist =="PERT"){
      qol_postb<- input$qol_post_pert
      qol_postbsd<-  NA 
      qol_postbmin<-  input$qol_postmin_pert
      qol_postbmax<-  input$qol_postmax_pert
      qol_postbshape<-  input$qol_postlam_pert
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
    
    # Table of inputs
    Table_input <- data.frame(
      Parameter = c("Cohort size", 
                    "Time horizon", 
                    "Willingness to pay",
                    "TPT",
                    "TPT effectiveness",
                    "Cost of campaign",
                    "Cost of campaign SD (Gamma)",
                    "Cost of campaign min (PERT)",
                    "Cost of campaign max (PERT)",
                    "Cost of campaign shape (PERT)",
                    "Cost of TBI test",
                    "Cost of TBI test SD (Gamma)",
                    "Cost of TBI test min (PERT)",
                    "Cost of TBI test max (PERT)",
                    "Cost of TBI test shape (PERT)",
                    "Cost of TPT",
                    "Cost of TPT SD (Gamma)",
                    "Cost of TPT min (PERT)",
                    "Cost of TPT max (PERT)",
                    "Cost of TPT shape (PERT)",
                    "QoL of TB disease",
                    "QoL of TB disease SD (Gamma)",
                    "QoL of TB disease min (PERT)",
                    "QoL of TB disease max (PERT)",
                    "QoL of TB disease shape (PERT)",
                    "QoL of EPTB disease",
                    "QoL of EPTB disease SD (Gamma)",
                    "QoL of EPTB disease min (PERT)",
                    "QoL of EPTB disease max (PERT)",
                    "QoL of EPTB disease shape (PERT)",
                    "QoL of Post-TB disease",
                    "QoL of Post-TB disease SD (Gamma)",
                    "QoL of Post-TB disease min (PERT)",
                    "QoL of Post-TB disease max (PERT)",
                    "QoL of Post-TB disease shape (PERT)"
                    
      ),
      Current_scenario =c( input$n, 
                           input$t_hor, 
                           input$will_to_pay,
                           input$tpt,
                           tpt_eff,
                           cost_camp,
                           cost_campsd,
                           cost_campmin,
                           cost_campmax,
                           cost_campshape,
                           cost_test,
                           cost_testsd, 
                           cost_testmin,
                           cost_testmax,
                           cost_testshape,
                           cost_tpt,
                           cost_tptsd, 
                           cost_tptmin,
                           cost_tptmax,
                           cost_tptshape,
                           qol_tb,
                           qol_tbsd, 
                           qol_tbmin,
                           qol_tbmax,
                           qol_tbshape,
                           qol_eptb,
                           qol_eptbsd, 
                           qol_eptbmin,
                           qol_eptbmax,
                           qol_eptbshape,
                           qol_postb,
                           qol_postbsd, 
                           qol_postbmin,
                           qol_postbmax,
                           qol_postbshape
                           
                           
      )
    )
    
    cohort_size<-input$n
    time_horizon<-input$t_hor
    will_to_pay=input$will_to_pay
    
    
    #Define variables
    base1<-df()
    npositives<-length(which(base1$result=="Positive"))
    
    # Yearly predictions
    predictions<-matrix(0,nrow = cohort_size, ncol = time_horizon)
    predictions_sd<-matrix(0,nrow = cohort_size, ncol = time_horizon)
    
    for (ii in 2:time_horizon){
      base1$studytime <- (365*ii)-42
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
    
    
    #browser()
    deaths<-rbinom(sim_cases,sim_cases,cfr)
    deaths_itv<-rbinom(sim_cases_itv,sim_cases,cfr)
    
    # QALY
    age<-dfage()
    dfqol<-dffullqol()
    healthy<-sum(((age$value)/100)*(dfqol$value))
    
    sim_healthy_qaly<-sim_cases*0 + healthy*cohort_size * time_horizon
    sim_healthy_qaly_itv<-sim_cases_itv*0 + healthy*cohort_size * time_horizon
    
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
      bcea_tb=bcea(eff,cost, ref=2,interventions=treats),
      table=Table_input
    )
    
    return(out)
    
  })
  
  
  
  # ICER plane --------------------------------------------------------------
  
  
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
    
    # Download CSV
    output$table <- renderTable({
      dfage()
    })
    
    # Download single parameters ----------------------------------------------
    
    output$btndcsv <-
      downloadHandler(
        filename = function () {
          paste("parameters.csv", sep = "")
        },
        content = function(file) {
          write.csv(dfage(), file)
        }
      )
    
    
    
    
    # Produce markdown report -------------------------------------------------
    
    
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
    
    ########### Scenarios 
    
    # Scenarios ---------------------------------------------------------------
    
    
    # Table_params
    output$table_params <- renderTable({
      icer_object()$table
    })
    
    
  })
  
  
  
  
}

shinyApp(ui, server)

