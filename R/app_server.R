#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  # https://engineering-shiny.org/structuring-project.html#b.-the-strat%C3%A9gie-du-petit-r

  pairings = reactiveValues()
  
  mod_import_server("import_ui_1", pairings)
  mod_roulette_server("roulette_ui_1", pairings)
}
