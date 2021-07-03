#' import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("file1"), "Upload Past Pairings", 
              accept =".xlsx"),
    textOutput(ns("status"))
  )
}
    
#' import Server Functions
#'
#' @noRd 
mod_import_server <- function(id, upload){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    status = rv(message = "Ready to receive")
    
    #Uploaded a file
    observeEvent(input$file1, {
      path = input$file1$datapath
      
      #TODO: Want to validate path
      
      #Load each sheet within the workbook
      sheets = readxl::excel_sheets(path)
      tmp_pairings = purrr::map(sheets, function(sheet){
        readxl::read_excel(path, sheet = sheet)
      })
      tmp_pairings = setNames(tmp_pairings, sheets) #name the list
      
      #TODO: validate input
      
      # Pass it back to the reactive
      #tmp_pairings = roulette(tmp_pairings)
      upload$names = tmp_pairings$names
      upload$past = tmp_pairings$past
      
      status$message = "Ready to run"

  })
    
    
    output$status = renderText(
      status$message
    )
  })

}
    
