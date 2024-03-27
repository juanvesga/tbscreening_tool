library(dplyr)
library(readxl)

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

batch_temp <- read.csv("data-raw/template.csv")

dictionary <- read.csv("data-raw/dictionary.csv")

usethis::use_data(
    # objects
    model,
    country_tb_inc,
    qaly_input,
    batch_temp,
    dictionary,

    # other arguments
    internal = TRUE,
    overwrite = TRUE,
    version = 3
)
