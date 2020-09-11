get_title <- function(webpage, element = '.issue-item__title') {
  
  
    # Web-scrape titles from webpage.
    title_data <- webpage %>% 
                  html_nodes(element) %>% 
                  html_text()    
    
    
    # If more than half of the words are in capitals -> title to lower cases.
    id_caps             <- which( str_count(title_data, "\\b[A-Z]{2,}\\b") / str_count(title_data, '\\w+') >= .5) # Look for number of capitalized words and total number of words within the title
    title_data[id_caps] <- title_data[id_caps] %>%
                           tolower() %>%
                           str_to_title() 
    
    
    # Create dataframe with title and number of words in the title
    titles <- as_data_frame(title_data) %>%
              dplyr::rename(title = contains('value')) %>%
              mutate(words_title = as.numeric(str_count(title_data,'\\w+'))) #count number of words in the title
    
    
    # Delay requests
    Sys.sleep(1)
    
    
    return(titles)
    
}