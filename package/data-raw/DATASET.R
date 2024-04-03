library(dplyr)
library(readxl)
library(socialmixr)

model <-
    load("data-raw/fit_final_github_2020-08-14") |>
    get()

country_tb_inc <-
    read.csv("data-raw/TB_burden_countries_2020-08-14.csv") |>
    select(country_of_birth = country, year_of_entry = year, e_inc_100k)

qaly_input<-list(
    q.male = as.data.frame(read_xlsx("data-raw/inputs.xlsx", 1)),
    q.female = as.data.frame(read_xlsx("data-raw/inputs.xlsx", 2)),
    qol = as.data.frame(read_xlsx("data-raw/inputs.xlsx", 3)),
    covid.age = as.data.frame(read_xlsx("data-raw/inputs.xlsx", 4)),
    age_bands= as.data.frame(read_xlsx("data-raw/inputs.xlsx", 5))
)
qaly_input$q.male <- qaly_input$q.male[1:2]
qaly_input$q.female <- qaly_input$q.female[1:2]
dat <- qaly_input$qol[1:4]
times <- with(dat, high - low + 1L)
index <- rep(1:nrow(dat), times)
dat <- dat[index,]
dat$Age <- 1:nrow(dat) - 1L
rownames(dat) <- NULL
qaly_input$qol <- dat
qaly_input$covid.age <- NULL

batch_temp <- read.csv("data-raw/template.csv")

dictionary <- read.csv("data-raw/dictionary.csv")

# normalised contact matrices for estimating secondary cases
contact = contact_matrix(
    polymod, countries = "United Kingdom",
    age.limits = c(0,15,35,45,65), # lower bounds
    symmetric = TRUE)
c_matrix<-contact$matrix[c(2:5),c(2:5)]/rowSums(contact$matrix[c(2:5),c(2:5)])

usethis::use_data(
    # objects
    model,
    country_tb_inc,
    qaly_input,
    batch_temp,
    dictionary,
    c_matrix,

    # other arguments
    internal = TRUE,
    overwrite = TRUE,
    version = 3
)
