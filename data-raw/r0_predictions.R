## code to prepare `est_R0_final_model_comp` dataset goes here

args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(tidycovid19)
  library(ranger) # For making predictions 
})

usethis::ui_todo("Downloading last data...")
latest <- download_merged_data(silent = TRUE, cached = TRUE)

find_data <- function(latest_data = latest) {
  date_max <- Sys.Date()
  seq_dates <- seq.Date(date_max - 45, date_max,  by = 1)
  
  all_data <- function(date_maxx){
    latest_data %>% 
      dplyr::mutate(cum_cases = ecdc_cases,
                    cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
      dplyr::select(date, cases, country) %>% 
      dplyr::filter(date >= date_maxx - 21, date <= date_maxx) %>% 
      na.omit() %>% 
      dplyr::group_by(country) %>% 
      dplyr::mutate(
        n_ind = 1:n(), 
        R_name = paste0("R", n_ind)) %>% 
      dplyr::select(-date) %>% 
      dplyr::arrange(country) %>% 
      dplyr::ungroup() %>% 
      dplyr::as_tibble() %>% 
      tidyr::complete(R_name, fill = list(cases = NA)) %>% 
      dplyr::group_by(country) %>% 
      dplyr::arrange(country, n_ind) %>% 
      tidyr::fill(cases, .direction = "down") %>% 
      dplyr::select(-n_ind) %>% 
      tidyr::spread(R_name, cases) %>% 
      dplyr::ungroup() 
  }
  
  data_seq_dates <- purrr:::map(seq_dates, all_data) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate_if(is.numeric, scale) %>% 
    na.omit()
  data_seq_dates
  
}

countries <- latest %>% 
  split(.$country)

usethis::ui_todo("Finding data for each country...")
all_countries_data  <- countries %>% 
  purrr:::map(safely(find_data))

df <- all_countries_data %>% 
  purrr::map("result") %>% 
  dplyr::bind_rows() 


# This model uses the last 21 days of R 
usethis::ui_todo("Loading model...")
model <-  read_rds("data-raw/est_R0_final_model_comp.rds")
pred_country <- function(data, rf_model = model){
  pred.R <- predict(rf_model, data = data,
                    type = 'quantiles')
  df <- data.frame(
    low = pred.R$predictions[,1],
    upp = pred.R$predictions[,3],
    pred = pred.R$predictions[,2]
  ) %>% 
    dplyr::bind_cols(data)
  df
}

usethis::ui_todo("Making predictions...")
r0_predictions <- pred_country(df)

usethis::use_data(r0_predictions, overwrite = TRUE)


## Deploy app

# remotes::install_deps(dependencies = TRUE)
# 
# rsconnect::setAccountInfo(
#   name = 'apmuhamilton',
#   token = args[1],
#   secret= args[2]
# )
# 
# files <- list.files('.')
# files <- files[files != 'data-raw']
# 
# rsconnect::deployApp(
#   appFiles = files,
#   appName = 'hamiltonREstCountry',
#   forceUpdate = TRUE,
#   account = 'apmuhamilton'
# )
