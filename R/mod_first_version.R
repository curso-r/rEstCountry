latest <- tidycovid19::download_merged_data(silent = TRUE, cached = TRUE)

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
            shinyjs::useShinyjs(),
            dateInput(ns("date_end"), "End of two week period to estimate R:",
                      value = max(latest$date),
                      format = "dd/mm/yyyy"),
            
            shinyWidgets::pickerInput(ns("sel_cty"),
                        "Select country",
                        choices = sort(unique(latest$country)),
                        selected = c('Ireland'),
                        options = list(`actions-box` = TRUE,
                                       `live-search` = TRUE),
                        multiple = FALSE),
            actionButton(inputId = ns("button"), label = "show extra options"),
            
            shinyWidgets::pickerInput(ns("R_method"),
                        "Method for computing R",
                        choices = c("EG", "ML", "SB"),
                        selected = c('SB'),
                        multiple = FALSE),
            
            shinyWidgets::pickerInput(ns("GD_dist"),
                        "Generation time distribution", 
                        choices = c("gamma", "weibull", "lognormal"),
                        selected = c('gamma'),
                        multiple = FALSE),
            
            numericInput(inputId = ns("GT_mean"),
                         label = "Generation time mean",
                         value = 3.0),
            
            numericInput(inputId = ns("GT_sd"),
                         label = "Generation time standard deviation",
                         value = 0.4),
            
            numericInput(inputId = ns("num_sim"),
                         label = "Number of simulations to run (higher = slower but more accurate)",
                         value = 200)
          ),
          bs4Dash::bs4TabCard(
            width = 9,
            title = "",
            id = "tabcard",
            closable = FALSE,
            collapsible = FALSE,
            bs4Dash::bs4TabPanel(
              tabName = "Estimation",
              plotly::plotlyOutput(ns("R_estim")) %>% hamiltonThemes::distill_load_spinner()
            ),
            bs4Dash::bs4TabPanel(
              tabName = "Assumptions",
              get_assuption_text()
            )
          ),
          hamiltonThemes:::bs4dash_distill_footer()
        )
      )
    )
 
  )
}
    
#' first_version Server Function
#'
#' @noRd 
mod_first_version_server <- function(input, output, session){
  ns <- session$ns
  
  observeEvent(input$button, {
    shinyjs::toggle("R_method")
    shinyjs::toggle("GD_dist")
    shinyjs::toggle("GT_mean")
    shinyjs::toggle("GT_sd")
    shinyjs::toggle("num_sim")
  }, ignoreNULL = FALSE)
  
  
  output$R_estim <- plotly::renderPlotly({
    data_use <- latest %>% 
      dplyr::filter(country == input$sel_cty) %>% 
      dplyr::mutate(
        cum_cases = ecdc_cases,
        cases = c(cum_cases[1], diff(ecdc_cases))
      ) %>% 
      dplyr::select(date, cases, population) %>% 
      dplyr::filter(date >= input$date_end - 14, date <= input$date_end) %>% 
      na.omit()
    
    # COVID generation time
    GT = R0::generation.time(input$GD_dist, c(input$GT_mean, input$GT_sd))
    
    estR0 = try(R0::estimate.R(
      epid = data_use$cases,
      t = data_use$date, 
      begin = as.integer(1),
      end = as.integer(length(data_use$cases)),
      GT = GT, 
      methods = input$R_method, 
      pop.size = data_use$population[1], 
      nsim = input$num_sim
    ), silent = TRUE)
    
    
    p = ggplot2::ggplot(data = data_use, ggplot2::aes(x = date, y = cases)) + 
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
    
    # shiny::validate(
    #   shiny::need(class(estR0) != "try-error", "Case values or date range not appropriate for R0 estimation using this method.")
    # )
    
    if(class(estR0) == "try-error" | any(data_use$cases < 10)) {
      a$text = "R0 not estimated (bad case values or date range)"
      a$font = list(size = 14)
    } else {
      if(input$R_method == "SB") {
        R_est = signif(tail(estR0$estimates[[input$R_method]]$R, 1), 3)
        R_low = signif(tail(estR0$estimates[[input$R_method]]$conf.int[1], 1), 3)
        R_high = signif(tail(estR0$estimates[[input$R_method]]$conf.int[2], 1), 3)
      } else {
        R_est = signif(estR0$estimates[[input$R_method]]$R, 3)
        R_low = signif(estR0$estimates[[input$R_method]]$conf.int[1], 3)
        R_high = signif(estR0$estimates[[input$R_method]]$conf.int[2], 3)
      }
      
      a$text = paste0("R = ", R_est,
                      " (95% CI: ", R_low,', ',
                      R_high, ')')
    }
    plotly::ggplotly(p) %>% plotly::layout(annotations = a) 
  })
}
    
## To be copied in the UI
# mod_first_version_ui("first_version_ui_1")
    
## To be copied in the server
# callModule(mod_first_version_server, "first_version_ui_1")
 
