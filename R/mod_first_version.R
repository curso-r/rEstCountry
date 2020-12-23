#' first_version UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_first_version_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::dashboardPage(
      sidebar_collapsed = TRUE,
      sidebar_mini = FALSE,
      body = bs4Dash::bs4DashBody(
        hamiltonThemes::use_bs4Dash_distill_theme(),
        br(),
        fluidRow(
          column(
            width = 3,
            # shinyjs::useShinyjs(),
            dateInput(ns("date_end"), "Date for estimated  R:",
                      value = max(latest$date),
                      max = Sys.Date(),
                      min = Sys.Date() - 45,
                      format = "dd/mm/yyyy"),
            
            shinyWidgets::pickerInput(ns("sel_cty"),
                        "Select country",
                        choices = sort(unique(latest$country)),
                        selected = c('Ireland'),
                        options = list(`actions-box` = TRUE,
                                       `live-search` = TRUE),
                        multiple = FALSE)
            # actionButton(inputId = ns("button"), label = "show extra options"),
            # 
            # shinyWidgets::pickerInput(ns("R_method"),
            #             "Method for computing R",
            #             choices = c("EG", "ML", "SB"),
            #             selected = c('SB'),
            #             multiple = FALSE),
            # 
            # shinyWidgets::pickerInput(ns("GD_dist"),
            #             "Generation time distribution", 
            #             choices = c("gamma", "weibull", "lognormal"),
            #             selected = c('gamma'),
            #             multiple = FALSE),
            # 
            # numericInput(inputId = ns("GT_mean"),
            #              label = "Generation time mean",
            #              value = 3.0),
            # 
            # numericInput(inputId = ns("GT_sd"),
            #              label = "Generation time standard deviation",
            #              value = 0.4),
            # 
            # numericInput(inputId = ns("num_sim"),
            #              label = "Number of simulations to run (higher = slower but more accurate)",
            #              value = 200)
          ),
          bs4Dash::bs4TabCard(
            width = 9,
            title = "",
            id = "tabcard",
            closable = FALSE,
            collapsible = FALSE,
            bs4Dash::bs4TabPanel(
              tabName = "Estimation",
              plotly::plotlyOutput(ns("R_estim")) %>% 
                hamiltonThemes::distill_load_spinner()
            ),
            bs4Dash::bs4TabPanel(
              tabName = "Assumptions",
              get_assuption_text()
            )
          )
        )
      ),
      footer = hamiltonThemes:::bs4dash_distill_footer()
    )
  )
}
    
#' first_version Server Function
#'
#' @noRd 
mod_first_version_server <- function(input, output, session){
  ns <- session$ns
  
  # observeEvent(input$button, {
  #   shinyjs::toggle("R_method")
  #   shinyjs::toggle("GD_dist")
  #   shinyjs::toggle("GT_mean")
  #   shinyjs::toggle("GT_sd")
  #   shinyjs::toggle("num_sim")
  # }, ignoreNULL = FALSE)
  
  output$R_estim <- plotly::renderPlotly({
     
    current_country <- input$sel_cty
    date_max <- input$date_end
    
    latest_filter <- latest %>% 
      dplyr::filter(country == current_country) %>%
      dplyr::mutate(cum_cases = ecdc_cases,
             cases = c(cum_cases[1], diff(ecdc_cases))) %>%
      dplyr::select(date, cases, population) %>%
      dplyr::filter(date >= date_max - 14, date <= date_max) %>%
      na.omit()
    
    estR0 = r0_predictions %>%
      dplyr::filter(country == current_country) 
    
    n_dates <- seq.Date(Sys.Date() - nrow(estR0) + 1, Sys.Date(),  by = 1)
    
    
    estR0 = estR0 %>% 
      dplyr::mutate(date = n_dates) %>% 
      dplyr::filter(date == date_max)
    
    p <- ggplot2::ggplot(
      data = latest_filter, 
      ggplot2::aes(x = date, y = cases)
    ) + 
      ggplot2::geom_point() + 
      ggplot2::labs(
        x = 'Date',
        y = 'Cases',
        title = paste('Cases in',input$sel_cty, 'from', 
                      format(input$date_end - 14, '%d-%b'), 'to',
                      format(input$date_end, '%d-%b'))) + 
      ggplot2::theme_bw() + 
      ggplot2::geom_smooth(se = FALSE, color = hamiltonThemes:::distill_status_to_colour("primary"))
    
    ggp <- ggplot2::ggplot_build(p)
    yrange = ggp$layout$panel_params[[1]]$y.range
    xrange = ggp$layout$panel_params[[1]]$x.range
    
    # Add the annotation
    a <- list(
      x = ggp$layout$panel_scales_x[[1]]$range$range[1],
      y = ggp$layout$panel_scales_y[[1]]$range$range[2],
      xref = "x",
      yref = "y",
      xanchor = 'left',
      showarrow = FALSE,
      font = list(size = 20)
    )
    
    if(nrow(estR0) == 0) {
      a$text = "R0 not estimated (bad case values or date range)"
      a$font = list(size = 14)
    } else {
      #if(input$R_method == "SB") {
      R_est = signif(estR0$pred, 3)
      R_low = signif(estR0$low, 3)
      R_high = signif(estR0$upp, 3)
      
      a$text = paste0("Estimated R = ", R_est,
                      ",  10-90 Quantile Interval: (", R_low,', ',
                      R_high, ')')
    }
    plotly::ggplotly(p) %>% 
      plotly::layout(annotations = a) 
  })
}
    
## To be copied in the UI
# mod_first_version_ui("first_version_ui_1")
    
## To be copied in the server
# callModule(mod_first_version_server, "first_version_ui_1")
 
