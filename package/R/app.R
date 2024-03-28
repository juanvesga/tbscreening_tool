#' Tool
#'
#' @import shiny
#' @import shinydashboard
#' @import waiter
#' @import data.table
#' @import DT
#' @import ggplot2
#' @import rstpm2
#' @import BCEA
#' @import shinyBS
#' @import igraph
#'
#' @export
tool <- function(...) {

    # The following objects are constructed in data-raw/DATASET.R and made
    # available on load:
    #     - model
    #     - country_tb_inc
    #     - qaly_input
    #     - batch_temp
    #     - dictionary
    #     - c_matrix (contact matrix)

    tests <- c("QuantiFERON", "T-SPOT.TB", "Tuberculin Skin Test")

    regimens<-c("6INH","3HP", "3RH")

    age_uk <- as.matrix(c(
        "16-35" = 14,
        "36-45" = 32,
        "46-65" = 32,
        "65+"   = 22
    ))

    age.categorical <- distributions3::Categorical(rownames(age_uk), p = age_uk/100)

    prev_uk <- as.matrix(c(
        "16-35" = 10,
        "36-45" = 25,
        "46-65" = 35,
        "65+"   = 45
    ))

    qaly_input <- lapply(qaly_input, setDT)

    qol_full <- as.matrix(c(
        "A_16to35" = 0.94,
        "A_36to45" = 0.911,
        "A_46to65" = 0.823,
        "A_65plus" = 0.7525
    ))


    # -------------------------------------------------------------------------
    ui <- dashboardPage(

        skin = "black",

        dashboardHeader(
            title = "LTBI CEA Tool",
            tags$li(
                class = "dropdown",
                actionButton("home", "Home", icon = icon("house"))
            )
        ),

        dashboardSidebar(
            sidebarMenu(
                id = "sidebar",

                menuItem("Home", tabName = "home", icon = icon("house")),

                menuItem(
                    "Single Run",
                    tabName = "single",
                    icon = icon("bar-chart"),
                    startExpanded = TRUE,

                    menuSubItem(
                        "How to...",
                        tabName = "how",
                        icon = icon("check")
                    ),

                    menuSubItem(
                        "Demographics",
                        tabName = "epi",
                        icon = icon("bar-chart")
                    ),

                    menuSubItem(
                        "LTBI cascade",
                        tabName = "cascade",
                        icon = icon("chart-simple")
                    ),

                    menuSubItem(
                        "Costs",
                        tabName = "cost",
                        icon = icon("sterling-sign")
                    ),

                    menuSubItem(
                        "QoL",
                        tabName = "qol",
                        icon = icon("staff-snake")
                    ),

                    menuSubItem(
                        "Output",
                        tabName = "res_icer",
                        icon = icon("bullseye")
                    )
                ),

                menuItem("Scenarios", tabName = "scenarios", icon = icon("bar-chart")),

                menuItem("Batch run", tabName = "advanced", icon = icon("bar-chart")),

                menuItem("About", tabName = "about", icon = icon("th"))
            )
        ),

        dashboardBody(
            tabItems(

                # UI home -----------------------------------------------------------------
                tabItem(tabName = "home",
                    fluidPage(
                        theme = shinythemes::shinytheme("spacelab"),
                        tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),

                        h1("TB infection test cost-effectiveness calculator",style = "text-align: center"),
                        br(),br(),

                        p("This tool was created to help quantify and visualise the
                            potential impact of delivering TB infection tests and
                            TB preventive regimens among immigrant populations in the UK.",
                          style = "text-align: justify; font-size:18px"),

                        p("Three different modalities for analysis are available:
                            'Single Run', 'Scenarios', and 'Advanced'.",
                          style = "text-align: justify; font-size:18px"),

                        p("Choose your prefered mode and navigate using the 'Go' button.",
                          style = "text-align: justify; font-size:18px"),

                        br(), br(), br(), br(),

                        box(
                            style = "text-align: justify; font-size:18px",
                            title = h3("Single Run",style = 'font-size:20px;color:white;font-weight: bold;'),
                            width = 4,
                            background = "navy",
                            p("Define a baseline cohort, input epidemiological and cost
                                parameterers to retrieve estimations of expected TB disease cases,
                                cost projections of the testing intervention and a full set of
                                cost-effectiveness analysis output."),
                            tags$p(actionButton("single", "Go", class = "dark")),
                        ),

                        box(
                            style = "text-align: justify; font-size:18px",
                            title = h3("Compare scenarios",style = 'font-size:20px;color:white;font-weight: bold;'),
                            width = 4,
                            background = "orange",
                            p("Upload pre-populated scenarios created with this tool -or
                                add new ones using the csv template, and run a scenario
                                comparison analysis"),
                            tags$p(actionButton("compare", "Go", class = "dark"))
                        ),

                        box(
                            style = "text-align: justify; font-size:18px",
                            title = h3("Batch run",style = 'font-size:20px;color:white;font-weight: bold;'),
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

                        fluidRow(column(12,
                            div(
                                style="display: inline-block;",
                                tags$p(
                                    "1) Define the size, demographics, and expected positivity of the LTBI test in the baseline cohort  ",
                                    style = "font-size:18px"
                                )
                            ),
                            actionButton("demog", "Demographics", icon = icon("bar-chart"), class = "light")
                        )),

                        br(),

                        fluidRow(column(12,
                            div(
                                style="display: inline-block;",
                                tags$p(
                                    "2) Select your choice of LTBI test and define the steps in the cascade from test to regimen completion  ",
                                    style = "font-size:18px"
                                )
                            ),
                            actionButton("ltbicasc", "LTBI cascade", icon = icon("chart-simple"), class = "light")
                        )),

                        br(),

                        fluidRow(column(12,
                            div(
                                style="display: inline-block;",
                                tags$p(
                                    "3) Select costs for relevant TB outcomes. Here you can define the shape of the background distributions for these inputs  ",
                                    style = "font-size:18px"
                                )
                            ),
                            actionButton("cost_tab", "Costs", icon = icon("sterling-sign"), class = "light")
                        )),

                        br(),

                        fluidRow(column(12,
                            div(
                                style="display: inline-block;",
                                tags$p(
                                    "4) Select Quality of Life (QoL) for TB outcomes. Define the discount rate, time-horizon, and shape of the background distributions for QoL inputs  ",
                                    style = "font-size:18px"
                                )),
                            actionButton("qol_tab", "QoL", icon = icon("staff-snake"), class = "light")
                        )),

                        br(),

                        fluidRow(column(12,
                            div(style="display: inline-block;",
                                tags$p(
                                    "5) Run CEA analsys using the run button in the ICER panel. Here you can download a report of your scenario  ",
                                    style = "font-size:18px"
                                )
                            ),
                            actionButton("icer_tab", "ICER", icon = icon("bullseye"), class = "light")
                        ))
                    )
                ),


                # UI single: Demog  ---------------------------------------------------------
                tabItem(tabName = "epi",
                    tabsetPanel(type = "tabs",

                        tabPanel("Demographics",
                            sidebarPanel(
                                tags$h4("Age distribution (%)"),
                                sliderInput("age4", "65+", 0, 100, age_uk[4], width='90%' ),
                                sliderInput("age3", "46-65", 0, 100, age_uk[3], width='90%' ),
                                sliderInput("age2", "36-45 ", 0, 100, age_uk[2], width='90%' ),
                                sliderInput("age1", "16-35 ", 0, 100, age_uk[1], width='90%' ),

                                verbatimTextOutput('result'),
                                tags$head(tags$style("#result{color: red}"))
                            ),

                            mainPanel(
                                br(),
                                plotOutput("distPlot")
                            )
                        ),

                        tabPanel("Epidemiology",
                            sidebarPanel(
                                fluidRow(
                                    column(
                                        width=3,
                                        numericInput(inputId ="n", label="Cohort Size", value = 5000, min = 0, max = Inf)
                                    ),

                                    column(
                                        width=4,
                                        selectInput("cohort_mode", "Cohort type", c( "Contacts"="contact","New entrants"="new"))
                                    ),

                                    conditionalPanel(
                                        condition = "input.cohort_mode == 'contact'",
                                        column(
                                            width=4,
                                            selectInput("contact_type", "Contact type", c( "Household"="household","Other"="other"))
                                        )
                                    )
                                ),

                                # Only show this panel if the plot type is a histogram
                                conditionalPanel(
                                    condition = "input.cohort_mode == 'new'",
                                    tags$h5("What fraction of the cohort comes from a 'High TB burden' country?"),
                                    tags$h6("(i.e., WHO TB incidence > 100 per 100k (Use the TB burden finder)"),
                                    sliderInput("burden_100k", "%", 0, 100, 50, width='90%' ),
                                    fluidRow(
                                        br(),
                                        tags$h5("TB burden finder"),
                                        column(
                                            width=4,
                                            selectInput("country", "Country", country_tb_inc$country_of_birth)
                                        ),
                                        column(
                                            width=4,
                                            br(),
                                            verbatimTextOutput('country_burden')
                                        )
                                    )
                                ),

                                fluidRow(
                                    br(),
                                    tags$h5("How many new cases are generated by each active TB case?"),
                                    column(
                                        width=3,
                                        numericInput(inputId ="new_cases", label="Secondary cases", value = 1, min = 0, max = Inf)
                                    ),
                                    column(
                                        width=4,
                                        numericInput(
                                            "ser_int",
                                            label = tags$span(
                                                "Serial interval",
                                                tags$i(
                                                    id = "i_si",
                                                    class = "glyphicon glyphicon-info-sign",
                                                    style = "color:#0072B2;",
                                                    title ="Average interval (years) between the onset of symptoms
                                                            in the primary (infector) and secondary case (infectee)"
                                                )
                                            ),
                                            value = 1
                                        ),
                                        # add hover message
                                        bsTooltip("i_si", " ", placement = "right", trigger = "hover", options = NULL)
                                    )
                                )
                            ),

                            mainPanel(br(), br(), plotOutput("networkPlot"))
                        ),

                        tabPanel("LTBI prevalence",
                            sidebarPanel(
                                tags$h4("Expected TB infection prevalence (%)"),
                                sliderInput("prev4", "65+", 0, 100, prev_uk[4], width='90%' ),
                                sliderInput("prev3", "46-65", 0, 100, prev_uk[3], width='90%' ),
                                sliderInput("prev2", "36-45 ", 0, 100, prev_uk[2], width='90%' ),
                                sliderInput("prev1", "16-35 ", 0, 100, prev_uk[1], width='90%' )
                            ),

                            mainPanel(br(), plotOutput("prevPlot"))
                        ),

                        tabPanel("HIV prevalence",
                            sidebarPanel(
                                tags$h4( "Expected HIV prevalence (%)"),
                                sliderInput("hiv_prev4", "65+", 0, 100, 0, width='90%' ),
                                sliderInput("hiv_prev3", "46-65", 0, 100, 0, width='90%' ),
                                sliderInput("hiv_prev2", "36-45 ", 0, 100, 0, width='90%' ),
                                sliderInput("hiv_prev1", "16-35 ", 0, 100, 0, width='90%' )
                            ),
                            mainPanel(br(), plotOutput("hiv_prevPlot"))
                        ),

                    )
                ),

                # UI single: LTBI casc ----------------------------------------------------
                tabItem(tabName = "cascade",
                    sidebarPanel(
                        fluidRow(
                            column(5, selectInput("test",label = "Prefered test", choices = tests)),
                            column(6, selectInput("tpt", "Choose TPT regimen", regimens))
                        ),

                        conditionalPanel(
                            condition = "input.test == 'Tuberculin Skin Test'",
                            tags$h5("TST attrition (i.e, lost to follow-up between injection and test reading)"),
                            sliderInput("tst_attr", "%", 0, 100, 10),
                        ),

                        fluidRow(
                            column(7, checkboxInput("manualAE", "Manually input AE (%)", FALSE)),
                            conditionalPanel(
                                condition = "input.manualAE == 1",
                                column(3,numericInput("manualAEnum", label=NULL, value = 8, min = 0, max = 100))
                            )
                        ),

                        br(),

                        checkboxGroupInput(
                            inputId="age_tpt",
                            label="TPT eligible age groups:",
                            choices=c(
                                "16-35" = "16-35",
                                "36-45" = "36-45",
                                "46-65" = "46-65",
                                "65+" = "65+"
                            ),
                            selected = c(
                                "36-45" = "36-45",
                                "46-65" = "46-65",
                                "65+" = "65+"
                            )
                        ),

                        sliderInput("casc1", "LTBI positive that start TPT (%)", 0, 100, 100,width='90%'),

                        sliderInput("casc2", "TPT regimens completed (%) ", 0, 100, 90,width='90%')
                    ),

                    mainPanel(
                        fluidRow(
                            valueBoxOutput(width=5,"tpt_effectiveness"),
                            valueBoxOutput(width=5,"tpt_AE")
                        ),

                        tags$h4("LTBI testing and treatment cascade"),

                        conditionalPanel(
                            condition = "input.test != 'Tuberculin Skin Test'",
                            plotOutput("cascadeplot")
                        ),

                        conditionalPanel(
                            condition = "input.test == 'Tuberculin Skin Test'",
                            plotOutput("cascadeplot_tst")
                        ),

                        br(),

                        h5("Abbreviations: AE - Adverse events, TPT - TB Preventive Therapy")
                    )

                ),


                # UI single: Costs ----------------------------------------------------
                tabItem(tabName = "cost",
                    tabsetPanel(type = "tabs",
                        tabPanel("LTBI test",
                            # Tets cost
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("Unit cost of LTBI test (£)"),
                                    plotOutput("cost_test", height = "35vh")
                                ),

                                box(
                                    width = 4,

                                    title="Select distribution to be used",

                                    column(4, radioButtons("testcost_dist", "", c("Gamma","PERT"))),

                                    column(4,
                                        conditionalPanel(
                                            condition = "input.testcost_dist == 'PERT'",
                                            numericInput("cost_test_pert", "Most likely", value = 50, min = 0, max = Inf),
                                            numericInput("cost_testmin_pert", "Min", value = 10, min = 0, max = Inf),
                                            numericInput("cost_testmax_pert", "Max", value = 70, min = 0, max = Inf),
                                            sliderInput("cost_testlam", "Shape", value = 4, min = 0, max = 10)
                                        ),
                                        conditionalPanel(
                                            condition = "input.testcost_dist == 'Gamma'",
                                            numericInput("cost_test_gamma", "Mean", value = 50, min = 0, max = Inf),
                                            numericInput("cost_testsd_gamma", "SD", value = 10, min = 0, max = Inf)
                                        )
                                    )
                                )
                            )
                        ),

                        tabPanel("Test campaign",
                            # Campaign cost
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("Overall cost (£) - Incurred only at start"),
                                    plotOutput("cost_campaign", height = "35vh")
                                ),

                                box(
                                    width = 4,
                                    title="Select distribution to be used",
                                    column(4, radioButtons("campcost_dist", "", c("Gamma","PERT"))),
                                    column(4,
                                        conditionalPanel(
                                            condition = "input.campcost_dist == 'PERT'",
                                            numericInput("cost_camp_pert", "Most likely", value = 1000, min = 0, max = Inf),
                                            numericInput("cost_campmin_pert", "Min", value = 800, min = 0, max = Inf),
                                            numericInput("cost_campmax_pert", "Max", value = 1200, min = 0, max = Inf),
                                            sliderInput("cost_camplam", "Shape", value = 4, min = 0, max = 10)
                                        ),
                                        conditionalPanel(
                                            condition = "input.campcost_dist == 'Gamma'",
                                            numericInput("cost_camp_gamma", "Mean", value = 1000, min = 0, max = Inf),
                                            numericInput("cost_campsd_gamma", "SD", value = 200, min = 0, max = Inf)
                                        )
                                    )
                                )
                            )
                        ),

                        # cost TPT
                        tabPanel("TPT regimen",
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("Unit Cost of TPT regimen drugs(£)"),
                                    plotOutput("cost_tpt", height = "35vh")
                                ),

                                box(
                                    width = 4,
                                    title="Select distribution to be used",
                                    column(4, radioButtons("tptcost_dist", "", c("Gamma","PERT"))),
                                    column(4,
                                        conditionalPanel(
                                            condition = "input.tptcost_dist == 'PERT'",
                                            numericInput("cost_tpt_pert", "Most likely", value = 1000, min = 0, max = Inf),
                                            numericInput("cost_tptmin_pert", "Min", value = 800, min = 0, max = Inf),
                                            numericInput("cost_tptmax_pert", "Max", value = 1200, min = 0, max = Inf),
                                            sliderInput("cost_tptlam", "Shape", value = 4, min = 0, max = 10)
                                        ),

                                        conditionalPanel(
                                            condition = "input.tptcost_dist == 'Gamma'",
                                            numericInput("cost_tpt_gamma", "Mean", value = 1000, min = 0, max = Inf),
                                            numericInput("cost_tptsd_gamma", "SD", value = 200, min = 0, max = Inf)
                                        )
                                    )
                                )
                            ),

                            # Other costs
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("Other TPT related costs(£)"),
                                    h5("This includes lab tests, staff time and others"),
                                    plotOutput("cost_tpt2", height = "35vh")
                                ),

                                box(
                                    width = 4,
                                    title="Select distribution to be used",
                                    column(4, radioButtons("tpt2cost_dist", "", c("Gamma","PERT"))),
                                    column(4,
                                        conditionalPanel(
                                            condition = "input.tpt2cost_dist == 'PERT'",
                                            numericInput("cost_tpt2_pert", "Most likely", value = 100, min = 0, max = Inf),
                                            numericInput("cost_tpt2min_pert", "Min", value = 0, min = 0, max = Inf),
                                            numericInput("cost_tpt2max_pert", "Max", value = 600, min = 0, max = Inf),
                                            sliderInput("cost_tpt2lam", "Shape", value = 4, min = 0, max = 10)
                                        ),
                                        conditionalPanel(
                                            condition = "input.tpt2cost_dist == 'Gamma'",
                                            numericInput("cost_tpt2_gamma", "Mean", value = 100, min = 0, max = Inf),
                                            numericInput("cost_tpt2sd_gamma", "SD", value = 20, min = 0, max = Inf)
                                        )
                                    )
                                )
                            ),

                            # LTFUP cost
                            fluidRow(
                                box(
                                    width=4,
                                    h3("Unit Cost of TPT regimen in LTFUP (£)"),
                                    plotOutput("cost_ltfup_tpt", height = "35vh")
                                ),

                                box(
                                    width=4,
                                    sliderInput("cost_ltfup", "Proportion of TPT regimen cost incurred by those lost to follow-up (%)" , 0, 100, 50),
                                )
                            )
                        ),

                        # cost TB Tx
                        tabPanel("TB treatment",
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("Unit cost of complete TB treatment (£)"),
                                    plotOutput("cost_tbtx", height = "35vh")
                                ),

                                box(
                                    width = 4,
                                    title="Select distribution to be used",
                                    column(4, radioButtons("tbtxcost_dist", "", c("Gamma","PERT"))),
                                    column(4,
                                        conditionalPanel(
                                            condition = "input.tbtxcost_dist == 'PERT'",
                                            numericInput("cost_tbtx_pert", "Most likely", value = 6000, min = 0, max = Inf),
                                            numericInput("cost_tbtxmin_pert", "Min", value = 800, min = 0, max = Inf),
                                            numericInput("cost_tbtxmax_pert", "Max", value = 15000, min = 0, max = Inf),
                                            sliderInput("cost_tbtxlam", "Shape", value = 4, min = 0, max = 10)
                                        ),

                                        conditionalPanel(
                                            condition = "input.tbtxcost_dist == 'Gamma'",
                                            numericInput("cost_tbtx_gamma", "Mean", value = 6000, min = 0, max = Inf),
                                            numericInput("cost_tbtxsd_gamma", "SD", value = 1000, min = 0, max = Inf)
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),

                # UI single: QoL ----------------------------------------------------
                tabItem(tabName = "qol",
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Baseline QoL",
                            fluidRow(
                                box(
                                    title = "Baseline Quality of Life (QoL) by age",
                                    plotOutput("fullQOL_Plot")
                                ),

                                box(
                                    sliderInput("qolfull_1", "16-35 ", 0, 1, qol_full[1]),
                                    sliderInput("qolfull_2", "36-45 ", 0, 1, qol_full[2]),
                                    sliderInput("qolfull_3", "46-65", 0, 1, qol_full[3]),
                                    sliderInput("qolfull_4", "65+", 0, 1, qol_full[4])
                                )
                            )
                        ),

                        # Excess mortality QoL loss
                        tabPanel("Excess deaths QoL losses",
                            fluidRow(
                                column(3, h3("QALY loss due to TB death "), br(), tableOutput("agetab")),
                                column(3,
                                    br(), br(), br(),
                                    sliderInput("t_hor", em("Time-horizon (years)"), 2, 120, 100),
                                    br(),
                                    numericInput("disc_rate", em("Select discount rate (0%-10%)"), 3.5, min = 0, max = 100, step = 0.1)
                                )
                            ),
                            br(),
                            h5("Abbreviations: LE - Life Expectancy, QALE - Quality Adjusted Life Expectancy, dQALY - Discounted Quality Adjusted Life Years"),
                            br(),
                            h6("Notes:"),
                            h6("1. This table reflects QALY losses from excess deaths only"),
                            h6("2. It is an adaptation of the 'COVID19 QALY App' by N.R Naylor. See here:",
                               span(tags$a(href="https://github.com/LSHTM-CHIL/COVID19_QALY_App", "https://github.com/LSHTM-CHIL/COVID19_QALY_App"),style = "color:blue")
                            ),
                            h6("3. In this adaptation we do not consider the effect of comorbidities in either mortality and disability ")
                        ),

                        # PTB QoL
                        tabPanel("Pulmonary TB",
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("QALY loss due to pulmonary TB disease"),
                                    plotOutput("qolPERT1", height = "35vh")
                                ),

                                box(
                                    width = 4,
                                    title="Select distribution to be used",
                                    column(4, radioButtons("ptbqol_dist", "", c("Beta","PERT"))),
                                    column(4,
                                        conditionalPanel(
                                            condition = "input.ptbqol_dist == 'PERT'",
                                            sliderInput("qol_ptb_pert", "Most likely", value = 0.16, min = 0, max = 1),
                                            sliderInput("qol_ptbmin_pert", "Min", value = 0.06, min = 0, max = 1),
                                            sliderInput("qol_ptbmax_pert", "Max", value = 0.25, min = 0, max = 1),
                                            sliderInput("qol_ptblam_pert", "Shape", value = 4, min = 0, max = 10)
                                        ),
                                        conditionalPanel(
                                            condition = "input.ptbqol_dist == 'Beta'",
                                            sliderInput("qol_ptb_beta", "Mean", value = 0.16, min = 0, max = 1),
                                            sliderInput("qol_ptbsd_beta", "SD", value = 0.04, min = 0, max = 1)
                                        ),
                                    )
                                )
                            )
                        ),

                        tabPanel("Extra-pulmonary TB",
                            # EPTB QoL
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("QALY loss due to Extra-pulmonary TB disease"),
                                    plotOutput("qolPERT2", height = "35vh")
                                ),

                                box(
                                    width = 4,
                                    title="Select distribution to be used",
                                    column(4, radioButtons("eptbqol_dist", "", c("Beta","PERT"))),
                                    column(4,
                                        conditionalPanel(
                                            condition = "input.eptbqol_dist == 'PERT'",
                                            sliderInput("qol_eptb_pert", "Most likely", value = 0.05, min = 0, max = 1),
                                            sliderInput("qol_eptbmin_pert", "Min", value = 0.02, min = 0, max = 1),
                                            sliderInput("qol_eptbmax_pert", "Max", value = 0.08, min = 0, max = 1),
                                            sliderInput("qol_eptblam_pert", "Shape", value = 4, min = 0, max = 10)
                                        ),
                                        conditionalPanel(
                                            condition = "input.eptbqol_dist == 'Beta'",
                                            sliderInput("qol_eptb_beta", "Mean", value = 0.05, min = 0, max = 1),
                                            sliderInput("qol_eptbsd_beta", "SD", value = 0.01, min = 0, max = 1)
                                        )
                                    )
                                )
                            )
                        ),

                        tabPanel("Post-TB",
                            # QoL postTB
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("QALY loss due Post-TB Lung disease"),
                                    plotOutput("qolPERT3", height = "35vh")
                                ),

                                box(
                                    width = 4,
                                    title="Select distribution to be used",
                                    column(4, radioButtons("postqol_dist", "", c("Beta","PERT"))),
                                    column(4,
                                        conditionalPanel(
                                            condition = "input.postqol_dist == 'PERT'",
                                            sliderInput("qol_post_pert", "Most likely", value = 0.03, min = 0, max = 1),
                                            sliderInput("qol_postmin_pert", "Min", value = 0.01, min = 0, max = 1),
                                            sliderInput("qol_postmax_pert", "Max", value = 0.1, min = 0, max = 1),
                                            sliderInput("qol_postlam_pert", "Shape", value = 4, min = 0, max = 10)
                                        ),

                                        conditionalPanel(
                                            condition = "input.postqol_dist == 'Beta'",
                                            sliderInput("qol_post_beta", "Mean", value = 0.03, min = 0, max = 1),
                                            sliderInput("qol_postsd_beta", "SD", value = 0.01, min = 0, max = 1)
                                        ),
                                    )
                                )
                            )
                        ),

                        # QoL TPT Adevrese events (AE)
                        tabPanel("TPT adverse events",
                            fluidRow(
                                box(
                                    width = 4,
                                    h3("QALY loss due adverse events (AE) from TPT"),
                                    plotOutput("qolPERT4", height = "35vh")
                                ),

                                box(
                                    width = 4,
                                    title="Select distribution to be used",
                                    column(4, radioButtons("aeqol_dist", "", c("Beta","PERT"))),
                                    column(4,
                                        conditionalPanel(
                                            condition = "input.aeqol_dist == 'PERT'",
                                            numericInput("qol_ae_pert", "Most likely", value = 0.0046, min = 0, max = 1),
                                            numericInput("qol_aemin_pert", "Min", value = 0.0015, min = 0, max = 1),
                                            numericInput("qol_aemax_pert", "Max", value = 0.01, min = 0, max = 1),
                                            sliderInput("qol_aelam_pert", "Shape", value = 4, min = 0, max = 10)
                                        ),

                                        conditionalPanel(
                                            condition = "input.aeqol_dist == 'Beta'",
                                            numericInput("qol_ae_beta", "Mean", value = 0.0046, min = 0, max = 1),
                                            numericInput("qol_aesd_beta", "SD", value = 0.0001, min = 0, max = 1)
                                        ),
                                    )
                                )
                            )
                        )
                    )
                ),

                # UI single: Incidence plots ----------------------------------------------------
                tabItem(tabName = "res_inc",
                    fluidRow(column(9, box(width=12, plotOutput("plot_inc"))))
                ),

                # UI single: Cost plots ----------------------------------------------------
                tabItem(tabName = "res_costs",
                    fluidRow(column(9, box(width=12, plotOutput("plot_costs"))))
                ),

                # UI single: ICER plane ----------------------------------------------------
                tabItem(tabName = "res_icer",
                    tabsetPanel(
                        type = "tabs",

                        # ICER plane
                        tabPanel("ICER",
                            fluidRow(
                                valueBoxOutput("ICER"),
                                valueBoxOutput("qalygained"),
                                valueBoxOutput("costsaved")
                            ),

                            fluidRow(
                                box(sliderInput("will_to_pay", "Wilingness to pay (£)", 5000, 50000, 20000)),
                                actionButton("run","Run"),
                                downloadButton("btn", "Generate Report"),

                                #styles
                                tags$head(
                                    tags$style(
                                        " #btn{vertical-align:middle;
                                        background:darkslategrey;
                                        border-color:darkslategrey;
                                        color: white;}
                                        #run{vertical-align:middle;
                                        background:firebrick;border-color:firebrick;
                                        color: white;}"
                                    )
                                )
                            ),

                            fluidRow(
                                box(plotOutput("plot_icer")),
                                box(plotOutput("plot_wtp"))
                            )
                        ),

                        #INB
                        tabPanel("INB",
                            fluidPage(
                                br(), br(),
                                fluidRow(column(8, align="center", plotOutput("plot_INB"))),
                                br(),
                                h5("Abbreviations: WTP (£/QALY) - Willingness to pay per QALY"),
                                br()
                            )
                        ),

                        #QALY Breakdown
                        tabPanel("QALY breakdown",
                            fluidPage(
                                br(), br(),
                                fluidRow(column(8, align="center",plotOutput("plot_QALYbreak"))),
                                br(),
                                h5("Abbreviations: AE - Adverse events,
                                    EPTB - Extra-Pulmonary TB, PTB - Pulmonary TB,
                                    PTBLD - Post TB Lung Disease"),
                                br(),
                            )
                        ),

                        # Cost breakdown
                        tabPanel("Cost breakdown",
                            fluidPage(
                                br(), br(),
                                fluidRow(column(8, align="center", plotOutput("plot_Costbreak"))),
                                br(),
                                h5("Abbreviations: LTBI test - latent TB infection test,
                                    TPT - TB Prevention Therapy"),
                                br()
                            )
                        ),

                        # Cohort summary
                        tabPanel("Cohort summary",
                            fluidRow(
                                column(
                                    width=5,
                                    h3("Summary of events"),
                                    br(),
                                    tableOutput("cohort_tab"),
                                    br()
                                ),

                                column(
                                    width=6,
                                    align="center",
                                    br(),
                                    plotOutput("cohort_plot"),
                                    br(),
                                )
                            ),

                            h5("Abbreviations: LTBI test - latent TB infection test,
                                TPT - TB Prevention Therapy, PTBLD - Post TB Lung Disease")
                        ),

                        # Sensitivity analysis
                        tabPanel("Sensitivity analysis",
                            fluidRow(
                                sidebarPanel(
                                    fluidRow(
                                        column(5,
                                            h4("Step 1: explore a range"),
                                            br(),
                                            selectInput(
                                                "variable_sa",
                                                "Variable:",
                                                c("Test cost(£)" = "ltbi_c",
                                                  "TPT cost(£)" = "tpt_c",
                                                  "Secondary cases" = "n_second")
                                            )
                                        ),

                                        column(4,
                                            h4("Tick to run fast exploration"),
                                            checkboxInput("fast_sa", "Fast run", FALSE),
                                        )
                                    ),
                                    br(),
                                    p("Select a range of values for sampling
                                      and number of points within the range"),
                                    br(),
                                    fluidRow(
                                        column(width=3,
                                            numericInput(inputId ="min_sa", label="Min", value = 0, min = 0, max = Inf)
                                        ),
                                        column(width=3,
                                            numericInput(inputId ="max_sa", label="Max", value = 0, min = 0, max = Inf)
                                        ),
                                        column(width=3,
                                            numericInput(inputId ="n_sa", label="N points", value = 0, min = 0, max = 1000)
                                        ),
                                    ),
                                    useWaitress(),
                                    actionButton("run_sa","Run"),

                                    #styles
                                    tags$head(
                                        tags$style(
                                            " #run_sa{vertical-align:middle;
                                            background:firebrick;
                                            border-color:firebrick;
                                            color: white;}"
                                        )
                                    )
                                ),

                                mainPanel(br(), plotOutput("plot_sa",width = "55%"))
                            ),

                            fluidRow(
                                sidebarPanel(
                                    h4("Step 2: Input a fixed parameter value and run full simulation"),
                                    br(),
                                    fluidRow(numericInput(inputId ="mean_sa", label="Selected", value = 0, min = 0, max = Inf)),
                                    actionButton("run_sa2","Run"),

                                    #styles
                                    tags$head(
                                        tags$style(
                                            " #run_sa2{vertical-align:middle;
                                            background:firebrick;
                                            border-color:firebrick;
                                            color: white;}"
                                        )
                                    )
                                ),
                                mainPanel(plotOutput("plot_sa2",width = "85%"))
                            )
                        ),

                        # Table parameters
                        tabPanel("Model parameters",
                            fluidRow(
                                column(width=3, tableOutput("table")),
                                column(width = 4, downloadButton("down_pars", "Download scenario parameters (.csv)")),

                                #styles
                                tags$head(
                                    tags$style(
                                        " #down_pars{vertical-align:middle;
                                        background:darkslategrey;
                                        border-color:darkslategrey;
                                        color: white;}"
                                    )
                                )
                            )
                        )
                    )
                ),

                # UI scenarios: how... ----------------------------------------------------
                tabItem(tabName = "scenarios",
                    tabsetPanel(type = "tabs",
                        # Load
                        tabPanel("Load scenarios", fluidPage(tableOutput("table2"))),

                        # Results
                        tabPanel("Results")
                    )
                ),

                # UI advanced: how... ----------------------------------------------------
                tabItem(tabName = "advanced",
                    tabsetPanel(type = "tabs",
                        # Load
                        tabPanel(
                            "Load batch",
                            tags$h2("Run a batch of scenarios and retrieve results"),
                            br(),
                            sidebarPanel(
                                fluidRow(
                                    column(width = 8,
                                        tags$h5('1) Download the batch run template '),
                                        br(),
                                        tags$h5('2) Download the parameter dictionary'),
                                        br(),
                                        tags$h5(
                                            '3) Fill the template by adding new
                                            columns to the template, making sure
                                            to name each column appropriately*'
                                        ),
                                        br(),
                                        tags$h5('4) Upload your batch file'),
                                        br(), br(),
                                        tags$h5('5) Run the analysis on all the scenarios uploaded'),
                                        br(), br(),
                                        tags$h5('6) Download results as .csv')
                                    ),

                                    column(width = 4,
                                        downloadButton("down_temp", "Download", class="butt"),
                                        br(), br(), br(),
                                        downloadButton("down_dict", "Download", class="butt"),
                                        br(), br(), br(), br(), br(), br(), br(), br(),
                                        fileInput(
                                            "batch_file",
                                            label=NULL,
                                            multiple = FALSE,
                                            accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                            )
                                        ),

                                        # Styles
                                        tags$head(
                                            tags$style(
                                                ".butt{vertical-align:middle;
                                                height: 30px;
                                                width: 110%;
                                                font-size: 11px;
                                                background:darkslategrey;
                                                border-color:darkslategrey;
                                                color: white;}
                                                .runbutt{vertical-align:middle;
                                                height: 30px;
                                                width: 110%;
                                                font-size: 11px;
                                                background:firebrick;
                                                border-color:firebrick;
                                                color: white;}
                                                .btn-file{vertical-align:middle;
                                                font-size: 11px;
                                                background:darkslategrey;
                                                border-color:darkslategrey;
                                                color: white;}"
                                            )
                                        ),

                                        shinyjs::useShinyjs(),
                                        useWaitress(),

                                        actionButton("run_batch","Run batch",class="runbutt"),
                                        br(), br(), br(), br(),
                                        downloadButton("down_batch", "Download", class="butt")
                                    )
                                )
                            ),

                            mainPanel(
                                fluidPage(
                                    DT::dataTableOutput("batchtab"),
                                    style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
                                ),
                                h5(
                                    "* A syntactically valid name consists of letters,
                                    numbers and the dot or underline characters and starts
                                    with a letter or the dot not followed by a number.
                                    Names such as '.2way' are not valid, and neither are the reserved words.
                                    See ?make.names in R"
                                )
                            )
                        ),

                        # Results
                        tabPanel("Summary",
                            fluidPage(
                                DT::dataTableOutput("batch_results_tab"),
                                style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
                            )
                        )
                    )
                ),

                # UI About ----------------------------------------------------
                tabItem(tabName = "about",
                    fluidPage(
                        p("TB infection testing cost-effectiveness calculator", style = "font-size:25px"),

                        p(
                            "This tool was created to help quantify and visualise the
                            potential impact of performing TB infection tests, and providing
                            TB preventive regimens among new entrants in the UK or contacts
                            of TB cases.",
                            style = "text-align: justify; font-size:18px"
                        ),

                        p(
                            "The user is prompted to input information on demographic
                            characteristics of the cohort, type of TB infection test,
                            and the time horizon of analysis. With this information,
                            the tool is able to retrieve estimations of expected TB
                            disease cases, cost projections of the testing and TB
                            prevention treatment intervention, and importantly,
                            a full set of cost-effectiveness analysis (CEA) output.",
                            style = "text-align: justify; font-size:18px"
                        ),

                        p("About this tool",style = "font-size:25px"),

                        p(
                            "This tool builds on a previously developed personalised risk
                            predictor for incident TB ",
                            span(
                                tags$a(href="http://www.periskope.org/", "PERISKOPE-TB"),
                                style = "color:blue"
                            ),
                            ".At the core of PERISKOPE is a flexible parametric survival
                            model designed for estimating TB risk at the individual level.
                            We exploit this engine to recreate population-level
                            estimations of TB incidence. We build the health economics
                            assessment on top of this estimations, using the input
                            provided by the user through the interface",
                            style = "text-align: justify; font-size:18px"
                        ),

                        p(
                            "For further details on the methods behind PERISKOPE-TB,
                            please consult here: ",
                            span(
                                tags$a(
                                    href="https://www.nature.com/articles/s41591-020-1076-0.epdf?sharing_token=WrllWDo82ksX5S2UVcva3tRgN0jAjWel9jnR3ZoTv0N7Cf4tktJr3XujjQUdrBMIc-wX36PquVEMu0shOkAcLeC3EB4y0xOLdE2FujQxD8QEEmD_kM6-ycVNXD5NMCzSf_7O5GYkZkAFnxebkh5K1D9hXJKjHtyEqS_mEObTt9E%3D",
                                    "Gupta RK et al, Nature Medicine 2020."
                                ),
                                style = "color:blue"
                            ),
                            style = "text-align: justify; font-size:18px"
                        )
                    )
                )
            )
        )
    )


    server <- function(input, output,session) {

        # Home button tag
        observeEvent(input$home, {
            updateTabItems(session, "sidebar", "home")
        })

        # Single run button tag
        observeEvent(input$single, {
            updateTabItems(session, "sidebar", selected = "how")
        })

        # Compare button tag
        observeEvent(input$compare, {
            updateTabItems(session, "sidebar", selected = "scenarios")
        })

        # Compare button tag
        observeEvent(input$advanced, {
            updateTabItems(session, "sidebar", selected = "advanced")
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
            data.frame(
                age=c("16-35","36-45","46-65","65+"),
                value=as.numeric(c(input$prev1,input$prev2,input$prev3,input$prev4))
            )
        })

        dfhivprev <- reactive({
            data.frame(
                age=c("16-25","26-45","46-65","65+"),
                value=as.numeric(c(input$hiv_prev1,input$hiv_prev2,input$hiv_prev3,input$hiv_prev4))
            )
        })

        dffullqol <- reactive({
            data.frame(
                age=c("16-25","26-45","46-65","65+"),
                value=as.numeric(c(input$qolfull_1,input$qolfull_2,input$qolfull_3,input$qolfull_4))
            )
        })

        dfage <- reactive({
            data.frame(
                age=c("16-35","36-45","46-65","65+"),
                value=as.numeric(c(input$age1,input$age2,input$age3,input$age4))
            )
        })


        # Demog tab contents------------------------------------------------------------

        observeEvent( input$n, {
            updateNumericInput(session, "n_selected", value = input$n)
        })

        output$country_burden<-renderText({
            look <- dplyr::left_join(
                list2DF(list(country_of_birth=input$country, year_of_entry = 2018)),
                country_tb_inc,
                by = dplyr::join_by(country_of_birth, year_of_entry)
            )
            paste(look$e_inc_100k, " per 100k")
        })

        output$Agedistr <- renderTable(age_uk, rownames=TRUE, colnames = FALSE)


        v <- reactiveValues(data = age_uk)

        # TODO - not currently used. TT
        valfun <- reactive(
            as.numeric(input$age1) +
            as.numeric(input$age2) +
            as.numeric(input$age3) +
            as.numeric(input$age4)
        )

        output$result <- renderText({
            val <- as.numeric(input$age1) + as.numeric(input$age2) + as.numeric(input$age3) + as.numeric(input$age4)
            if (val == 100)
                return(val)
            paste0(val,": Sum must be equal to 100")
        })


        prevalence_tab <- reactiveValues(data = prev_uk)


        #output the datatable based on the dataframe (and make it editable)
        output$my_datatable <- renderDT(
            DT::datatable(
                v$data,
                editable = TRUE,
                rownames=TRUE,
                colnames = '%',
                options = list(searching = FALSE, dom = 't')
            )
        )

        observeEvent(input$my_datatable_cell_edit, {
            #get values
            info = input$my_datatable_cell_edit
            i = as.numeric(info$row)
            j = as.numeric(info$col)
            k = as.numeric(info$value)
            k <- abs(k) #convert to positive if negative

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
                scale_y_continuous(labels = abs) +
                coord_flip() +
                labs(
                    x = "Age",
                    y = "Population (%)"
                    # title = "Population Pyramid"
                ) +
                theme_minimal()+
                theme(text = element_text(size=20), legend.position = "none")
        })

        # Network plot
        output$networkPlot <- renderPlot({

            r= input$new_cases
            links <- data.frame(
                source=as.character(rep(0,r)),
                target=as.character(seq(1,r))
            )

            # create the network object
            network <- graph_from_data_frame(d=links, directed=T)

            # plot it
            plot(network,
                 vertex.size=17,
                 main="Secondary active TB cases generated by each case"
            )
        })


        # Prev plot
        output$prevPlot <- renderPlot({
            # req(input$age1) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
            dfprev() %>%
                ggplot(aes(x = age, y=value )) +
                geom_bar(stat = "identity", fill="firebrick1")+
                scale_y_continuous(labels = abs) +
                coord_flip() +
                labs(
                    x = "Age",
                    y = "TBI Prevalence (%)"
                    # title = "Population Pyramid"
                ) +
                theme_minimal()+
                theme(text = element_text(size=20), legend.position = "none")
        })

        # Prev plot
        output$hiv_prevPlot <- renderPlot({
            # req(input$age1) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
            dfhivprev() %>%
                ggplot(aes(x = age, y=value )) +
                geom_bar(stat = "identity", fill="darkcyan")+
                scale_y_continuous(labels = abs) +
                coord_flip() +
                labs(
                    x = "Age",
                    y = "HIV Prevalence (%)"
                    # title = "Population Pyramid"
                ) +
                theme_minimal() +
                theme(text = element_text(size=20), legend.position = "none")
        })


        # LTBI cascade ------------------------------------------------------------
        observeEvent(input$tpt, {

            output$tpt_effectiveness <- renderValueBox({

                lookup <- c("6INH" = 0.6, "3HP" = 0.53, "3RH" = 0.6)
                tpt_eff<- lookup[[input$tpt]]

                valueBox(
                    value = paste0((1-tpt_eff)*100,"%"),
                    subtitle = paste0("TB disease reduction with ",input$tpt, ", i.e, 1-RR"),
                    icon = icon("medkit"),
                    color = "teal"
                )
            })
        })

        output$tpt_AE <- renderValueBox({

            lookup <- c("6INH" = 0.04, "3HP" = 0.08, "3RH" = 0.039)
            tpt_ae<- lookup[[input$tpt]]

            valueBox(
                value = paste0((tpt_ae)*100,"%"),
                subtitle = paste0("% Developing AE with ",input$tpt, ", i.e, 1-RR"),
                icon = icon("medkit"),
                color = "orange"
            )
        })

        output$cascadeplot <- renderPlot({
            req(input$casc1,input$casc2)
            cohort_size<-input$n
            start_tpt<-input$casc1/100
            complete_tpt<-input$casc2/100

            lookup <- c("6INH" = 0.6, "3HP" = 0.53, "3RH" = 0.6)
            tpt_eff<- lookup[[input$tpt]]

            age<-dfage()
            prev<-dfprev()
            eligible<-list2DF(list(age=age$age, value=age$age%in%input$age_tpt))
            ca2<-sum(((age$value)/100)*(prev$value/100)) * cohort_size
            ca3<-sum((age$value/100)*(prev$value/100) * eligible$value) * cohort_size
            ca4<-ca3*start_tpt
            ca5<-ca4*complete_tpt
            ca6<- ca5 * tpt_eff


            df<-data.frame(
                label=c("Tested", "LTBI+","Eligible for TPT","Starting TPT","Completing TPT","TBI cleared"),
                val = c(cohort_size, ca2,ca3,ca4,ca5,ca6)
            )
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



        output$cascadeplot_tst <- renderPlot({
            req(input$casc1,input$casc2)
            cohort_size<-input$n
            tst_attr<-input$tst_attr/100
            start_tpt<-input$casc1/100
            complete_tpt<-input$casc2/100

            lookup <- c("6INH" = 0.6, "3HP" = 0.53, "3RH" = 0.6)
            tpt_eff<- lookup[[input$tpt]]

            age<-dfage()
            prev<-dfprev()
            eligible<-data.frame(age=age$age, value=age$age%in%input$age_tpt)
            ca1a<-cohort_size
            ca1b<-cohort_size * (1-tst_attr)
            ca2<-sum(((age$value)/100)*(prev$value/100)) * ca1b
            ca3<-sum((age$value/100)*(prev$value/100) * eligible$value) * ca1b
            ca4<-ca3*start_tpt
            ca5<-ca4*complete_tpt
            ca6<- ca5 * tpt_eff


            df<-data.frame(
                label=c("TST performed","TST read", "LTBI+","Eligible for TPT","Starting TPT","Completing TPT","TBI cleared"),
                val = c(ca1a, ca1b, ca2, ca3, ca4, ca5, ca6)
            )
            df$label<- factor(df$label, levels = df$label)

            p<-ggplot(data=df, aes(x=label,y=val))+
                geom_bar(stat = "identity", fill=c("gold3","gold4","yellow4","yellow3","yellow2","yellow1","yellow"))+
                ylab("Number")+ xlab("Cascade stage")+
                theme_minimal()+
                theme(
                    text = element_text(size=20),
                    axis.text.x = element_text(angle=60, hjust=1))

            gridExtra::grid.arrange(p)
        })



        # Costs tab contents ------------------------------------------------------
        output$cost_test<- renderPlot({

            n<-input$n

            if (input$testcost_dist =="Gamma"){
                xmean<-input$cost_test_gamma
                sd<- input$cost_testsd_gamma
                shape<- (xmean^2)/(sd^2)
                x  <- rgamma(n,shape=shape, rate=shape/xmean)

            } else if (input$testcost_dist =="PERT"){
                xmean<-input$cost_test_pert
                xmin<-input$cost_testmin_pert
                xmax<-input$cost_testmax_pert
                lam<- input$cost_testlam
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)
            }

            hist(x, breaks=20, main = " ", xlab = "£", col = "gold2")

        })

        output$cost_campaign<- renderPlot({

            n<-input$n

            if (input$campcost_dist =="Gamma"){
                xmean<-input$cost_camp_gamma
                sd<- input$cost_campsd_gamma
                shape<- (xmean^2)/(sd^2)
                x  <- rgamma(n,shape=shape, rate=shape/xmean)

            } else if (input$campcost_dist =="PERT"){
                xmean<-input$cost_camp_pert
                xmin<-input$cost_campmin_pert
                xmax<-input$cost_campmax_pert
                lam<- input$cost_camplam
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)
            }

            hist(x, 20, main = " ", xlab = "£", col = "gold2")

        })

        output$cost_tpt<- renderPlot({

            n<-input$n

            if (input$tptcost_dist =="Gamma"){
                xmean<-input$cost_tpt_gamma
                sd<- input$cost_tptsd_gamma
                shape<- (xmean^2)/(sd^2)
                x  <- rgamma(n,shape=shape, rate=shape/xmean)

            } else if (input$tptcost_dist =="PERT"){

                xmean<-input$cost_tpt_pert
                xmin<-input$cost_tptmin_pert
                xmax<-input$cost_tptmax_pert
                lam<- input$cost_tptlam
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)
            }
            hist(x, 20, main = " ", xlab = "£", col = "gold2")

        })


        output$cost_tpt2<- renderPlot({

            n<-input$n

            if (input$tpt2cost_dist =="Gamma"){
                xmean<-input$cost_tpt2_gamma
                sd<- input$cost_tpt2sd_gamma
                shape<- (xmean^2)/(sd^2)
                x  <- rgamma(n,shape=shape, rate=shape/xmean)

            } else if (input$tpt2cost_dist =="PERT"){

                xmean<-input$cost_tpt2_pert
                xmin<-input$cost_tpt2min_pert
                xmax<-input$cost_tpt2max_pert
                lam<- input$cost_tpt2lam
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)
            }
            hist(x, 20, main = " ", xlab = "£", col = "gold2")

        })

        output$cost_ltfup_tpt<- renderPlot({

            n<-input$n
            ltfup<-input$cost_ltfup/100

            if (input$tptcost_dist =="Gamma"){
                xmean<-input$cost_tpt_gamma
                sd<- input$cost_tptsd_gamma
                shape<- (xmean^2)/(sd^2)
                x  <- rgamma(n,shape=shape, rate=shape/xmean)

            } else if (input$tptcost_dist =="PERT"){

                xmean<-input$cost_tpt_pert
                xmin<-input$cost_tptmin_pert
                xmax<-input$cost_tptmax_pert
                lam<- input$cost_tptlam
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)
            }
            if (ltfup==0){
                hist(x*ltfup, 20, main = " ", xlim = c(0,1), xlab = "£", col = "gold2")
            } else {
                hist(x*ltfup, 20, main = " ", xlab = "£", col = "gold2")
            }

        })


        output$cost_tbtx<- renderPlot({

            n<-input$n

            if (input$tbtxcost_dist =="Gamma"){
                xmean<-input$cost_tbtx_gamma
                # xmin<-input$cost_tptmin_gamma
                # xmax<-input$cost_tptmax_gamma
                sd<- input$cost_tbtxsd_gamma#(xmax-xmin)/3.92
                shape<- (xmean^2)/(sd^2)
                x  <- rgamma(n,shape=shape, rate=shape/xmean)

            } else if (input$tbtxcost_dist =="PERT"){

                xmean<-input$cost_tbtx_pert
                xmin<-input$cost_tbtxmin_pert
                xmax<-input$cost_tbtxmax_pert
                lam<- input$cost_tbtxlam
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)
            }
            hist(x, 20, main = " ", xlab = "£", col = "gold2")

        })


        # QoL content -------------------------------------------------------------

        # Prev plot
        output$fullQOL_Plot <- renderPlot({

            dffullqol() %>%
                ggplot(aes(x = age, y=value )) +
                geom_bar(stat = "identity", fill="darkcyan")+
                scale_y_continuous(labels = abs) +
                labs(
                    x = "Age",
                    y = "Quality of Life"
                    # title = "Population Pyramid"
                )+
                theme_minimal()+
                theme(text = element_text(size=20), legend.position = "none")
        })

        # Qol PtTB
        output$qolPERT1<- renderPlot({

            n<-input$n

            if (input$ptbqol_dist =="Beta"){

                xmean<-input$qol_ptb_beta
                sd<- input$qol_ptbsd_beta
                pars_beta<-estBetaParams(xmean,sd^2 )
                x <- rbeta(n,pars_beta$alpha, pars_beta$beta)

            } else if (input$ptbqol_dist =="PERT"){

                xmean<-input$qol_ptb_pert
                xmin<-input$qol_ptbmin_pert
                xmax<-input$qol_ptbmax_pert
                lam<- input$qol_ptblam_pert
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)

            }

            hist(x, 20, main = " ", xlab = "QoL losses", col = "violetred3")

        })


        # Qol EPtTB
        output$qolPERT2<- renderPlot({

            n<-input$n

            if (input$eptbqol_dist =="Beta"){

                xmean<-input$qol_eptb_beta
                sd<- input$qol_eptbsd_beta
                pars_beta<-estBetaParams(xmean,sd^2 )
                x <- rbeta(n,pars_beta$alpha, pars_beta$beta)

            } else if (input$eptbqol_dist =="PERT"){

                xmean<-input$qol_eptb_pert
                xmin<-input$qol_eptbmin_pert
                xmax<-input$qol_eptbmax_pert
                lam<- input$qol_eptblam_pert
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)

            }

            hist(x, 20, main = " ", xlab = "QoL losses", col = "violetred3")

        })

        # Qol PostTB
        output$qolPERT3<- renderPlot({

            n<-input$n

            if (input$postqol_dist =="Beta"){

                xmean<-input$qol_post_beta
                sd<- input$qol_postsd_beta
                pars_beta<-estBetaParams(xmean,sd^2 )
                x <- rbeta(n,pars_beta$alpha, pars_beta$beta)

            } else if (input$postqol_dist =="PERT"){

                xmean<-input$qol_post_pert
                xmin<-input$qol_postmin_pert
                xmax<-input$qol_postmax_pert
                lam<- input$qol_postlam_pert
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)

            }

            hist(x, 20, main = " ", xlab = "QoL losses", col = "violetred3")

        })

        # Qol TPT AE
        output$qolPERT4<- renderPlot({

            n<-input$n

            if (input$aeqol_dist =="Beta"){

                xmean<-input$qol_ae_beta
                sd<- input$qol_aesd_beta
                pars_beta<-estBetaParams(xmean,sd^2 )
                x <- rbeta(n,pars_beta$alpha, pars_beta$beta)

            } else if (input$aeqol_dist =="PERT"){

                xmean<-input$qol_ae_pert
                xmin<-input$qol_aemin_pert
                xmax<-input$qol_aemax_pert
                lam<- input$qol_aelam_pert
                x<-freedom::rpert(n,xmin,xmax,xmean,lam)

            }

            hist(x, 20, main = " ", xlab = "QoL losses", col = "violetred3")

        })

        # Create main parameter input -------------------------------------------------


        # TODO - Can this be a list. TT
        pars <- reactive({
            df =  data.frame(
                cohort_size= input$n,
                time_horizon=input$t_hor,
                will_to_pay=input$will_to_pay,
                test=input$test,
                cohort_mode=input$cohort_mode,
                burden_100k=input$burden_100k/100,
                contact_type=input$contact_type,
                new_cases=input$new_cases,
                dr=input$disc_rate/100,
                tpt=input$tpt,
                tpt_cov=input$casc1/100,
                tpt_compl=input$casc2/100,
                tpt_cost_lfup=input$cost_ltfup/100,
                tst_attr=input$tst_attr/100,
                campcost_dist=input$campcost_dist,
                cost_camp_gamma=input$cost_camp_gamma,
                cost_campsd_gamma=input$cost_campsd_gamma,
                cost_camp_pert=input$cost_camp_pert,
                cost_campmin_pert=input$cost_campmin_pert,
                cost_campmax_pert=input$cost_campmax_pert,
                cost_camplam=input$cost_camplam,
                testcost_dist=input$testcost_dist,
                cost_test_gamma=input$cost_test_gamma,
                cost_testsd_gamma=input$cost_testsd_gamma,
                cost_test_pert=input$cost_test_pert,
                cost_testmin_pert=input$cost_testmin_pert,
                cost_testmax_pert=input$cost_testmax_pert,
                cost_testlam=input$cost_testlam,
                tptcost_dist=input$tptcost_dist ,
                cost_tpt_gamma=input$cost_tpt_gamma,
                cost_tptsd_gamma=input$cost_tptsd_gamma,
                cost_tpt_pert=input$cost_tpt_pert,
                cost_tptmin_pert=input$cost_tptmin_pert,
                cost_tptmax_pert=input$cost_tptmax_pert,
                cost_tptlam=input$cost_tptlam,
                tbtxcost_dist=input$tbtxcost_dist,
                cost_tbtx_gamma=input$cost_tbtx_gamma,
                cost_tbtxsd_gamma=input$cost_tbtxsd_gamma,
                cost_tbtx_pert=input$cost_tbtx_pert,
                cost_tbtxmin_pert=input$cost_tbtxmin_pert,
                cost_tbtxmax_pert=input$cost_tbtxmax_pert,
                cost_tbtxlam=input$cost_tbtxlam,
                ptbqol_dist=input$ptbqol_dist,
                qol_ptb_beta=input$qol_ptb_beta,
                qol_ptbsd_beta=input$qol_ptbsd_beta,
                qol_ptb_pert=input$qol_ptb_pert,
                qol_ptbmin_pert=input$qol_ptbmin_pert,
                qol_ptbmax_pert=input$qol_ptbmax_pert,
                qol_ptblam_pert=input$qol_ptblam_pert,
                eptbqol_dist=input$eptbqol_dist,
                qol_eptb_beta=input$qol_eptb_beta,
                qol_eptbsd_beta=input$qol_eptbsd_beta,
                qol_eptb_pert=input$qol_eptb_pert,
                qol_eptbmin_pert=input$qol_eptbmin_pert,
                qol_eptbmax_pert=input$qol_eptbmax_pert,
                qol_eptblam_pert=input$qol_eptblam_pert,
                postqol_dist=input$postqol_dist,
                qol_post_beta=input$qol_post_beta,
                qol_postsd_beta=input$qol_postsd_beta,
                qol_post_pert=input$qol_post_pert,
                qol_postmin_pert=input$qol_postmin_pert,
                qol_postmax_pert=input$qol_postmax_pert,
                qol_postlam_pert=input$qol_postlam_pert,
                aeqol_dist=input$aeqol_dist,
                qol_ae_beta=input$qol_ae_beta,
                qol_aesd_beta=input$qol_aesd_beta,
                qol_ae_pert=input$qol_ae_pert,
                qol_aemin_pert=input$qol_aemin_pert,
                qol_aemax_pert=input$qol_aemax_pert,
                qol_aelam_pert=input$qol_aelam_pert
            )})



        # QALY App components -----------------------------------------------------

        # Reactive dependencies - if these change then MODEL will run again and update values
        xxchange <- reactive({
            paste(input$disc_rate, input$t_hor)
        })


        QoLmodel <- eventReactive(xxchange(), {
            parameters<-pars()
            get_QALY_tab(parameters,qaly_input)

        })

        output$agetab <- renderTable(QoLmodel()$agetab, bordered = TRUE)

        # ICER object -----------------------------------------------------
        icer_object <- eventReactive(input$run,{

            #Call necessary objects
            parameters<-pars()
            perisk<-get_periskope_dataset(parameters,prevalence_tab$data,age.categorical)

            qol_loss_LE<- QoLmodel()$agetab$dQALY
            get_icer_obj(parameters,perisk,model,qol_loss_LE, c_matrix)

        })


        # TODO - Looks like we can pull some of this out of the loop. TT
        icer_object_sa1 <- eventReactive(input$run_sa,{

            #Call necessary objects


            params<-pars()
            n<-input$n_sa

            samps<-200
            if (input$fast_sa==1){
                samps<-10
            }

            sim_range= seq(input$min_sa,input$max_sa,length.out=n)
            at20k= seq(input$min_sa,input$max_sa,length.out=n)*0
            at30k= seq(input$min_sa,input$max_sa,length.out=n)*0

            withProgressWaitress({

                for (ii in 1:n){
                    incProgressWaitress(1)

                    if (input$variable_sa=="ltbi_c"){

                        params$testcost_dist<-"Gamma"
                        params$cost_test_gamma<-sim_range[ii]
                        params$cost_testsd_gamma <-1

                    } else if(input$variable_sa=="tpt_c"){

                        params$tptcost_dist <-"Gamma"
                        params$cost_tpt_gamma <-sim_range[ii]
                        params$cost_tptsd_gamma <-1

                    } else if(input$variable_sa=="n_second"){
                      params$new_cases <-sim_range[ii]
                    }

                    perisk<-get_periskope_dataset(params,prevalence_tab$data,age.categorical)

                    qol_loss_LE<- QoLmodel()$agetab$dQALY

                    obj<-get_icer_obj(params,perisk,model,qol_loss_LE,c_matrix,samps)

                    bceares<- obj$bcea_tb
                    icers<- bceares$delta_c/bceares$delta_e
                    at20k[ii]<-length(which(icers<20000))/dim(icers)[1]
                    at30k[ii]<-length(which(icers<30000))/dim(icers)[1]

                }
            }, selector = "#run_sa", max = n, theme = "overlay-percent")


            out<-list(
                at20k=at20k,
                at30k=at30k
            )


        })


        icer_object_sa2 <- eventReactive(input$run_sa2,{
            #Call necessary objects
            params<-pars()

            if (input$variable_sa=="ltbi_c"){

                params$testcost_dist<-"Gamma"
                params$cost_test_gamma<-input$mean_sa
                params$cost_testsd_gamma <-1

            }else if(input$variable_sa=="tpt_c"){

                params$tptcost_dist <-"Gamma"
                params$cost_tpt_gamma <-input$mean_sa
                params$cost_tptsd_gamma <-1

            }

            perisk<-get_periskope_dataset(params,prevalence_tab$data,age.categorical)

            qol_loss_LE<- QoLmodel()$agetab$dQALY

            get_icer_obj(params,perisk,model,qol_loss_LE,c_matrix)

        })


        # ICER plane --------------------------------------------------------------


        observeEvent(input$run, {


            output$ICER <- renderValueBox({
                #req(input$will_to_pay)
                will_to_pay=input$will_to_pay
                obj<-icer_object()
                bceares<- obj$bcea_tb
                icer<-bceares$ICER
                valueBox(
                    value = tags$p(scales::dollar(icer, prefix = ''),#formatC(icer, digits = 0, format = "f"),
                                   style = "color:white;font-size:28px;"),
                    subtitle = "ICER (£/QALY)",
                    icon = icon("bullseye"),
                    color = if (icer <= will_to_pay) "yellow" else "red"
                )
            })



            output$costsaved <- renderValueBox({
                obj<-icer_object()
                bceares<- obj$bcea_tb
                x<-mean(unlist(obj$bcea_tb$delta_c),na.rm=TRUE)
                valueBox(
                    value =  tags$p(scales::dollar(x, prefix = '£'),
                                    style = "color:white;font-size:28px;"),
                    subtitle = "Mean Incremental cost (£)",
                    icon = icon("sterling-sign"),
                    color =  "aqua"
                )
            })


            output$qalygained <- renderValueBox({
                obj<-icer_object()
                bceares<- obj$bcea_tb
                x<-mean(unlist(obj$bcea_tb$delta_e),na.rm=TRUE)
                valueBox(
                    value = tags$p(formatC(x, digits = 0, format = "f"),
                                   style = "color:white;font-size:28px;"),
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




            # Download single parameters ----------------------------------------------


            par_tab<-reactive({
                df= data.frame(
                    #  parameter=rownames(t(pars())),
                    Current=t(pars())
                )
            })

            output$table <- renderTable(data.frame(
                parameter=rownames(t(pars())),
                Current=t(pars())
            ))


            output$down_pars <-
                downloadHandler(
                    filename = function () {
                        paste("MyData.csv", sep = "")
                    },

                    content = function(file) {
                        write.csv(par_tab(), file)
                    }
                )


            # INB plot ----------------------------------------------------------------

            output$plot_INB <- renderPlot({
                # req(input$will_to_pay)
                df<-icer_object()$INB
                t<-input$t_hor
                icer<-mean(icer_object()$ICER[,t])



                p <- ggplot(data = df, aes(x = x)) +
                    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill ='darkorange', alpha = 0.2) +
                    geom_line(aes(y = `50%`), col = 'darkorange', lwd = 1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 25,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 20,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 15,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 10,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 5,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    annotate(geom="text", x=icer, y=1000, label="ICER",
                             color="grey40")+
                    labs(
                        title = "Incremental Net Benefit Analysis"
                    )+
                    xlab("WTP (£/QALY)")+
                    ylab("Net Benefit (£)")+
                    theme_minimal()+
                    theme(
                        text = element_text(size=20),
                        axis.text.x = element_text(angle=60, hjust=1))

                gridExtra::grid.arrange(p)


            })



            # QALY break --------------------------------------------------------------

            output$plot_QALYbreak <- renderPlot({
                # req(input$will_to_pay)
                obj<-icer_object()
                df<-data.frame(obj$QALYdist)
                df$Category<-c("Death","PTB","EPTB","PTBLD", "AE")


                dfm<-reshape2::melt(df)

                p<-ggplot(dfm,aes(x=variable, y=value, fill=Category))+
                    geom_bar(stat = "identity")+
                    labs(
                        title = "Breakdown of QALYs lost"
                    )+
                    xlab(" ")+
                    ylab("QALYs Lost")+
                    scale_fill_manual(values = c( 'navy','gold', 'darkorange','green4','grey38')) +
                    theme_minimal()+
                    theme(
                        text = element_text(size=20),
                        axis.text.x = element_text(angle=60, hjust=1))

                gridExtra::grid.arrange(p)


            })





            # Cost breakdown ----------------------------------------------------------


            output$plot_Costbreak <- renderPlot({

                obj<-icer_object()
                df<-data.frame(obj$Costdist)
                df$Category<-c("TB treatment","LTBI test","TPT","Campaign")

                dfm<-reshape2::melt(df)
                dfm1<-dfm[dfm$Category=="TB treatment",]
                dfm2<-dfm[dfm$Category != "TB treatment",]
                dfm2<-dfm2[dfm2$variable=="Intervention",]

                p1<-ggplot(dfm1,aes(x=variable, y=value))+
                    geom_bar(stat = "identity", fill='green4')+
                    labs(
                        title = "Costs of treating TB cases"
                    )+
                    xlab(" ")+
                    ylab("Cost (£)")+
                    theme_minimal()+
                    theme(
                        text = element_text(size=20),
                        axis.text.x = element_text(angle=60, hjust=1))


                p2<-ggplot(dfm2,aes(x=variable, y=value, fill=Category))+
                    geom_bar(stat = "identity")+
                    labs(
                        title = "Breakdown of Intervention Costs"
                    )+
                    xlab(" ")+
                    ylab("Cost (£)")+
                    scale_fill_manual(values = c('deeppink3','grey38','gold')) +
                    theme_minimal()+
                    theme(
                        text = element_text(size=20),
                        axis.text.x = element_text(angle=60, hjust=1))

                gridExtra::grid.arrange(p1,p2, nrow=1)


            })

            # Cohort table ------------------------------------------------------------

            output$cohort_tab <- renderTable({
                icer_object()$cohort_table}, digits = 0, bordered = TRUE,rownames = TRUE)




            output$cohort_plot<- renderPlot({

                df<- icer_object()$cohort_table
                df$x<-rownames(df)
                df$x<-factor(df$x, levels = rownames(df))
                dfm<-reshape2::melt(df)
                p<-ggplot(dfm,aes(x=x,y=value,fill=variable))+
                    geom_bar(stat="identity", position='dodge')+
                    labs(
                        title = "Simulated cohort events"
                    )+
                    ylab("N")+
                    xlab("")+
                    scale_fill_manual(values = c('grey38','gold')) +
                    theme_minimal()+
                    theme(
                        legend.title =element_blank(),
                        text = element_text(size=20),
                        axis.text.x = element_text(angle=60, hjust=1))

                gridExtra::grid.arrange(p)


            })

        })


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



        # Sensitivity analysis step 1 ---------------------------------------------------

        observeEvent(input$run_sa, {

            output$plot_sa<-renderPlot({

                obj<-icer_object_sa1()
                n<-input$n_sa
                df<-data.frame(
                    QALY20k=obj$at20k,
                    QALY30k=obj$at30k,
                    cost= seq(input$min_sa,input$max_sa,length.out=n)
                )

                dfm<-reshape2::melt(df, id="cost")

                p<-ggplot(dfm,aes(x=cost,y=value,colour=variable))+
                    geom_line(size=1.3)+
                    geom_hline(yintercept=0.9,linetype=2, color="#9999CC")+
                    geom_hline(yintercept=0.5,linetype=2, color="#CC6666")+
                    ylab("Probability of cost-effectiveness")+
                    xlab("cost(£)")+
                    scale_color_manual(values=c("#CC6666", "#9999CC"))+
                    theme_minimal()+
                    theme(
                        legend.title =element_blank(),
                        text = element_text(size=20),
                        axis.text.x = element_text(angle=60, hjust=1))

                gridExtra::grid.arrange(p)



            },height = 350, width = 800 )



        })

        # Sensitivity analysis step 2 ---------------------------------------------------
        observeEvent(input$run_sa2, {

            output$plot_sa2 <- renderPlot({
                #req(input$will_to_pay)
                obj<-icer_object_sa2()
                bceares<- obj$bcea_tb
                will_to_pay=input$will_to_pay
                icercol<-"#FF3333"


                #ICER plane
                p1 <- ceplane.plot(bceares,
                                   wtp =will_to_pay,
                                   graph="ggplot2",
                                   line = list(color = "grey40"),
                                   point = list(color = "blue"),
                                   icer = list(color = icercol, size = 3),
                                   area = list(fill = "lightcyan"),
                                   theme = theme_minimal())

                p1<-p1+ theme(
                    text = element_text(size=16),
                    axis.text.x = element_text(angle=60, hjust=1))



                # INB

                df<-obj$INB
                t<-input$t_hor
                icer<-mean(obj$ICER[,t])



                p2 <- ggplot(data = df, aes(x = x)) +
                    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill ='darkorange', alpha = 0.2) +
                    geom_line(aes(y = `50%`), col = 'darkorange', lwd = 1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 25,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 20,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 15,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 10,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    geom_point(data=data.frame(x=icer, y=0),
                               aes(x,y),
                               size = 5,
                               shape = 16,
                               color="red",
                               alpha=0.1) +
                    annotate(geom="text", x=icer, y=1000, label="ICER",
                             color="grey40")+
                    labs(
                        title = "Incremental Net Benefit Analysis"
                    )+
                    xlab("WTP (£/QALY)")+
                    ylab("Net Benefit (£)")+
                    theme_minimal()+
                    theme(
                        text = element_text(size=16),
                        axis.text.x = element_text(angle=60, hjust=1))

                gridExtra::grid.arrange(p1,p2,nrow=1)






            },height = 350 )
        })


        # Scenarios ---------------------------------------------------------------
        myReactives <- reactiveValues()
        temp_tab<-reactive({df=data.frame(batch_temp)})
        dict_tab<-reactive({df=data.frame(dictionary)})
        observe(  myReactives$template <-  temp_tab())
        observe(  myReactives$dictionary <-  dict_tab())

        observe(myReactives$batch <-  input_file())


        output$down_temp <-
            downloadHandler(
                filename = function () {
                    paste("template.csv", sep = "")
                },

                content = function(fname) {
                    write_csv(myReactives$template, fname)
                }
            )



        output$down_dict <-
            downloadHandler(
                filename = function () {
                    paste("dictionary.csv", sep = "")
                },

                content = function(file) {
                    write.csv(myReactives$dictionary, file)
                }
            )

        output$down_batch <-
            downloadHandler(
                filename = function () {
                    paste("batch_runs.csv", sep = "")
                },

                content = function(file) {

                    df<-data.frame(myReactives$batch_results)
                    write.csv(df, file)
                }
            )


        output$table2 <- renderTable(data.frame(
            parameter=rownames(t(pars())),
            Current=t(pars())
        ))




        # Errors: Batch file Warnings --------------------------------------------------------------

        input_file <- reactive({
            if (is.null(input$batch_file)) {
                shinyjs::disable("run_batch")
                return(NULL)
            }
            else
            {

                data<-read.csv(file = input$batch_file$datapath)


                if(ncol(data)<ncol(myReactives$template))
                {
                    shinyalert("Error","Uploaded Data has less cols than required",type="error")
                    returnValue()
                }
                else if(ncol(data)>ncol(myReactives$template))
                {
                    shinyalert("Error","Uploaded Data has more cols than required",type = "error")
                    returnValue()
                }
                else if(prod(data$test %in% tests)<1)
                {
                    shinyalert("Error","The test specified is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$cohort_mode %in% c('contact','new'))<1)
                {
                    shinyalert("Error","The cohort mode specified is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$contact_type %in% c('household','other'))<1)
                {
                    shinyalert("Error","The contact type specified is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$tpt %in% regimens)<1)
                {
                    shinyalert("Error","The TPT regimen specified is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$campcost_dist %in% c("Gamma","PERT"))<1)
                {
                    shinyalert("Error","The campaing cost distribution is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$testcost_dist %in% c("Gamma","PERT"))<1)
                {
                    shinyalert("Error","The test cost distribution is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$tptcost_dist %in% c("Gamma","PERT"))<1)
                {
                    shinyalert("Error","The TPT cost distribution is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$tbtxcost_dist %in% c("Gamma","PERT"))<1)
                {
                    shinyalert("Error","The TP tx cost distribution is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$ptbqol_dist %in% c("Beta","PERT"))<1)
                {
                    shinyalert("Error","The TB QoL distribution is not recognised",type = "error")
                    returnValue()
                }
                else if(prod(data$eptbqol_dist %in% c("Beta","PERT"))<1)
                {
                    shinyalert("Error","The EPTB QoL distribution is not recognised",type = "error")
                    returnValue()
                }
                else if(sum(data<0))
                {
                    shinyalert("Error","Negative values",type = "error")
                    returnValue()
                }


                else
                {
                    shinyjs::enable("run_batch")
                    return(data)
                }
            }

        })



        # Show batch table

        output$batchtab <- DT::renderDataTable({
            df<-t(data.frame(myReactives$batch))
            colnames(df)<-myReactives$batch[,1]
            df<-df[2:nrow(df),]
            DT::datatable(df,options = list(paging = FALSE) )

        })




        # Run batch  ---------------------------------------------------------

        observeEvent(input$run_batch, {


            shinyjs::disable("down_batch")

            req(input_file())

            #Transform data
            df<-input_file()

            results<-data.frame(id=1:200)
            #Loop over scenarios
            withProgressWaitress({
                for (ii in 1:nrow(df)){
                    incProgressWaitress(1)

                    parameters<-df[ii,]
                    perisk<-get_periskope_dataset(parameters,prevalence_tab$data,age.categorical)
                    qol_loss_LE<-get_QALY_tab(parameters,qaly_input)
                    res<-get_icer_obj(parameters,perisk,model,qol_loss_LE$agetab$dQALY,c_matrix)

                    obj<-res$bcea_tb

                    icername <- as.name(paste("ICER_",parameters$Scenario,sep=""))
                    costname <- as.name(paste("margin.cost_",parameters$Scenario,sep=""))
                    qalyname<- as.name(paste("margin.qaly_",parameters$Scenario,sep=""))

                    results[[icername]]<- obj$delta_c$`No intervention`/obj$delta_e$`No intervention`
                    results[[costname]]<- obj$delta_c$`No intervention`
                    results[[qalyname]]<- obj$delta_e$`No intervention`

                }
            }, selector = "#run_batch", max = nrow(df), theme = "overlay-percent")

            myReactives$batch_results<-results


            shinyjs::enable("down_batch")

        })

        # observeEvent(input$run_batch, {
        #   withProgressWaitress({
        #     for (i in 1:15) {
        #       incProgressWaitress(1)
        #       Sys.sleep(0.25)
        #     }
        #   }, selector = "#run_batch", max = 15, theme = "overlay-percent")
        # })

        output$batch_results_tab <- DT::renderDataTable({
            df<-myReactives$batch_results
            DT::datatable(df,options = list(paging = FALSE) )

        })




        # EXtra plots -------------------------------------------------------------



        output$plot_inc <- renderPlot({

            req(input$t_hor)
            cohort_size<-input$n
            time_horizon<-input$t_hor
            pred<-preds()
            npositives<-pred$npositives


            ################### Plots
            # Base line incidence

            qtls <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions),
                             probs = c(0.5)))
            qtls_low <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions_low),
                             probs = c(0.5)))
            qtls_high <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions_high),
                             probs = c(0.5)))

            df<- dplyr::bind_cols(qtls*cohort_size,qtls_low*cohort_size,qtls_high*cohort_size)
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
                matrixStats::rowQuantiles(t(pred$predictions_itv),
                             probs = c(0.5)))
            qtls_low <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions_low_itv),
                             probs = c(0.5)))
            qtls_high <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions_high_itv),
                             probs = c(0.5)))

            df<- dplyr::bind_cols(qtls,qtls_low,qtls_high)
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
                matrixStats::rowQuantiles(t(pred$casesaverted),
                             probs = c(0.5)))
            qtls_low <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$casesaverted_low),
                             probs = c(0.5)))
            qtls_high <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$casesaverted_high),
                             probs = c(0.5)))

            df<- dplyr::bind_cols(qtls,qtls_low,qtls_high)
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
                matrixStats::rowQuantiles(t(pred$predictions_cost),
                             probs = c(0.5)))
            qtls_low <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions_cost_low),
                             probs = c(0.5)))
            qtls_high <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions_cost_high),
                             probs = c(0.5)))

            df<- dplyr::bind_cols(qtls,qtls_low,qtls_high)
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
                matrixStats::rowQuantiles(t(pred$predictions_cost_itv),
                             probs = c(0.5)))
            qtls_low <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions_cost_low_itv),
                             probs = c(0.5)))
            qtls_high <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$predictions_cost_high_itv),
                             probs = c(0.5)))

            df<- dplyr::bind_cols(qtls,qtls_low,qtls_high)
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
                matrixStats::rowQuantiles(t(pred$costsaved),
                             probs = c(0.5)))
            qtls_low <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$costsaved_low),
                             probs = c(0.5)))
            qtls_high <- as.data.frame(
                matrixStats::rowQuantiles(t(pred$costsaved_high),
                             probs = c(0.5)))

            df<- dplyr::bind_cols(qtls,qtls_low,qtls_high)
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




    }


    shinyApp(ui, server)
}

