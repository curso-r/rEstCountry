get_assuption_text <- function(){
  HTML(
    paste0("<div style = 'color: black;'><p> This visualisation plots the raw number of cases in a selected ",
    "country and calculates the R number for that period using the methods ",
    "described in <a href = 'https://bmcmedinformdecismak.biomedcentral.com/",
    "articles/10.1186/1472-6947-12-147'>Obadia et al, BMC Medical Informatics ",
    "and Decision Making, 2012</a>.<p> The method relies on an estimate of the ",
    "Generation Time of the disease; this is the time from becoming infected ",
    "with COVID-19 to the time of generating a secondary case. The estimated ",
    "generation time distribution and its parameters have been taken from <a ",
    "href = 'https://onlinelibrary.wiley.com/doi/full/10.1111/biom.13325'>Yuhao ",
    "et al Biometrics, 2020</a>. The values can be changed by clicking the ",
    "'show extra options' button.<p> The R0 package allows for different ",
    "methods to calculate the R value. We use the Sequential Bayes method which ",
    "also provides a 95% confidence interval. Other methods can be selected in ",
    "the extra options.<p> If there are large number of zero cases, or the date ",
    "range is too large/small, the estimate may fail and an R0 number will not ",
    "be shown.<p> Be aware that most of these methods have hidden assumptions ",
    "(e.g. that the date range shows a period of exponential growth). If you ",
    "are changing the method, we would recommend reading the above papers first ",
    "to avoid mistaken readings.</div>")
  
  )
}

find_data <- function(date_max, latest_data = latest, 
                      current_country = current_country){
  data_use <- latest_data %>% 
    #dplyr::filter(country == current_country) %>% 
    dplyr::mutate(cum_cases = ecdc_cases,
                  cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
    dplyr::select(date, cases, country) %>% 
    dplyr::filter(date >= date_max - 21, date <= date_max) %>% 
    stats::na.omit() %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(
      n_ind = 1:dplyr::n(), 
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
  
  # df_remove <-  latest_data %>% 
  #   dplyr::mutate(cum_cases = ecdc_cases,
  #                 cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
  #   dplyr::select(date, cases, country) %>% 
  #   dplyr::filter(date >= date_max - 22, date <= date_max) %>% 
  #   na.omit() %>% 
  #   dplyr::group_by(country) %>% 
  #   dplyr::summarise(s = sum(cases)) %>% 
  #   dplyr::filter(s == 0) %>% 
  #   dplyr::pull(country)
  # 
  # data_use %>% 
  #   dplyr::filter(!(country %in% df_remove))
  data_use
}

pred_country <- function(data, rf_model = model){
  #data[, -1] <- scale(data[, -1])
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
