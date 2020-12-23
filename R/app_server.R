#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  latest <- tidycovid19::download_merged_data(silent = TRUE, cached = TRUE) %>% 
    dplyr::filter(!is.na(ecdc_cases))
  
  callModule(mod_first_version_server, "first_version_ui_1", latest)
}
