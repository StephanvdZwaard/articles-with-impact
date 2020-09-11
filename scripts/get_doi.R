get_doi <- function(webpage,element = '.epub-section__item a') {
  
  
  # Web-scrape titles from webpage.
  doi_data <- webpage %>% 
              html_nodes(element) %>% 
              html_text() %>%
              as_data_frame() %>%
              dplyr::rename(doi = value) 
  
  return(doi_data)
  
  
}