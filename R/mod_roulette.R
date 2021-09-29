#' roulette UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_roulette_ui <- function(id){
  ns <- NS(id)
  tagList(
   downloadButton(ns("download"), "Run")
   )
}
    
#' roulette Server Functions
#'
#' @noRd 
mod_roulette_server <- function(id, upload){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$download = downloadHandler(
      filename = "coffee.xlsx",
      content = function(file){
        this_match = roulette(upload$names, upload$past)
        writexl::write_xlsx(this_match, file)
        })
  })
}
    
roulette = function(names, past, m=2){
  # Turn past dates into dates
  past_partition = past %>% 
    mutate(date = paste0(date, "-01") %>% 
                    as.Date())
  
  next_date = max(past_partition$date) + 33 # Always guaranteed to be a day next month
  dates = unique(past_partition$date)
  
  past_partition = purrr::map(dates, function(d){
    past_partition %>% 
      filter(date == d) %>% 
      select(1:3) %>%
      mutate(group = dplyr::row_number()) %>% 
      tidyr::pivot_longer(2:3) %>% 
      select(-name) %>% 
      dplyr::rename(id = value) %>% 
      socialroulette::frame_to_partition() 
  }) %>% 
    purrr::set_names(dates)
  
  this_round = names %>% 
    mutate(date = next_date) %>%
    socialroulette::rsocialroulette(m = m, past = past_partition) %>% 
    list() %>% 
    setNames(next_date) %>% 
    socialroulette::partitions_to_pairs() %>% 
    #Join name
    dplyr::left_join(names, by=c("id1" ="id")) %>% 
    dplyr::rename(name_1 = Name) %>% 
    dplyr::left_join(names, by=c("id2" ="id")) %>% 
    dplyr::rename(name_2 = Name)  
  
  
  past = dplyr::bind_rows(past, 
                   select(this_round, 1:3))

  list(names = names,
       this_round = this_round,
       past = past)
  
  
}

ymd_to_ym = function(date){
  date %>% 
    as.character() %>% 
    stringr::str_remove("-[0-9]+$")
}
