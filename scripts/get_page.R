get_page <- function(webpage,element = '.toc-pagerange span+ span') {
  
  
    # Web-scrape page_nr from webpage.
    page_nr <- webpage %>% 
               html_nodes(element) %>% 
               html_text() %>%
               as_data_frame() %>%
               dplyr::rename(page_nr = value) 
    
    # Calculate page number based on start and end page (e.g. 17-25 is 9 pages)
    page_nr <- page_nr %>%
               mutate(start = as.numeric(gsub("-.*$","",page_nr)),
                      end   = as.numeric(gsub("^.*-","",page_nr))) %>%
               mutate(page_nr = end-start+1) %>%
               select(page_nr)
    
    return(page_nr)
  
}
