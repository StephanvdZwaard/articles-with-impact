get_authors <- function(webpage,element = '.loa') {
  
  
    # Web-scrape author from webpage.
    author_data <- webpage %>% 
                   html_nodes(element) %>% 
                   html_text()    
    
    
    # If more than half of the words are in capitals -> title to lower cases.
    id_caps              <- which( str_count(author_data, "\\b[A-Z]{2,}\\b") / str_count(author_data, '\\w+') >= .5)                              # Look for capitalized words and total number of words
    author_data[id_caps] <- author_data[id_caps] %>%
                            tolower() %>%
                            str_to_title() 
    
    
    # Create dataframe with authors and number of authors
    authors <- as_data_frame(author_data) %>%
               dplyr::rename(authors = value) %>%
               mutate(authors = gsub(", and"," and",authors)) %>%
               mutate(n_authors = ifelse(str_count(author_data,'\\,')>1, str_count(author_data,'\\,')-1, 0) + str_count(author_data,' and ') + 1) # Look for number of authors based on use of comma's and 'and' (which separate authors)

    
    # Delay requests
    Sys.sleep(1)
    
    
    return(authors) 
    
}
