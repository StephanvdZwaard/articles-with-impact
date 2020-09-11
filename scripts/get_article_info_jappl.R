# ------------------------------------------------------------------------------------------------------------------------ #
#                            Script for scraping article infomration from the journal website                              #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Scrape relevant information from the JAPPL journal website                                                 #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


get_article_info_jappl <- function(doi) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                       Webpage                                                            #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Retrieve website based on doi
  webpage <- try(read_html(as.character(doi),"rb"),silent=T)
  
  if (str_detect(webpage[1],"error") == F) {
    path <-  gsub("https://doi.org/","https://www.physiology.org/doi/abs/",doi)
    webpage <- try(read_html(path,"rb"),silent=T)
  } 
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                 Publication process                                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Get steps in publication including reviewing time (when submitted, accepted, in press and in print)
  time     <- anytime("01 January 2018")
  timeline <- webpage %>%
              html_nodes('.article__history div') %>%
              html_text() %>%
              as_data_frame() %>%
              mutate(phase = str_trim(sub("?(\\d+).*","",value)),
                     date  = str_trim(sub("^\\D*","",value)),
                     date  = ifelse(as.numeric(sub("\\s.*","",date))<10,paste0("0",date),date)) %>%
              mutate(date  = anytime(date)) %>%
              select(-value) %>%
              spread(phase, date)
  timeline <- timeline %>%
              mutate( Received             = ifelse('Received'           %in% names(.), Received,""),
                      Accepted             = ifelse('Accepted'           %in% names(.), Accepted,""),
                      `Published online`   = ifelse('Published online'   %in% names(.),`Published online`,""),
                      `Published in print` = ifelse('Published in print' %in% names(.),`Published in print`,""))
  attributes(timeline$Received)             <- attributes(time)
  attributes(timeline$Accepted)             <- attributes(time)
  attributes(timeline$`Published online`)   <- attributes(time)
  attributes(timeline$`Published in print`) <- attributes(time)
  timeline <- timeline %>%
              mutate(Under_review   = ifelse(nchar(Accepted)<=1 | nchar(Received)<=1, NA, round(difftime(Accepted,Received,"days"))),
                     Time_epub      = ifelse(nchar(`Published online`)<=1,   NA, round(difftime(Sys.Date(),`Published online`,"days"))/365.25),
                     Time_published = ifelse(nchar(`Published in print`)<=1, NA, round(difftime(Sys.Date(),`Published in print`,"days"))/365.25)) %>%
              select(c(Received, Accepted, `Published online`, `Published in print`, Under_review, Time_epub, Time_published))
  
  # Delay requests
  Sys.sleep(1)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                       Abstract                                                           #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Get full abstract
  abstract <- webpage %>%
              html_nodes(".abstractInFull") %>%
              html_text() %>%
              as_data_frame() %>%
              dplyr::rename(abstract = value)
  
  if (is_empty(abstract$abstract)) {
    
    rm(abstract)
    abstract <- data.frame(abstract= "no abstract",stringsAsFactors=FALSE)
    
  }
  abstract <- abstract %>% mutate(abstract_length = ifelse(abstract=="no abstract",0,lengths(strsplit(abstract, " "))))
  
  # Delay requests
  Sys.sleep(1)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                       Article type                                                       #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Get article type
  article_type <- webpage %>%
                  html_nodes(".citation__top") %>%
                  html_text() %>%
                  as_data_frame() %>%
                  dplyr::rename(article_type = value) %>%
                  mutate(article_type = case_when(str_detect(article_type,"Research Article") == T ~ "Research Article",
                                                  article_type == "Articles"                       ~ "Research Article",
                                                  str_detect(article_type,"Review")           == T ~ "Review",
                                                  str_detect(article_type,"Editorial")        == T ~ "Editorial",
                                                  TRUE ~ as.character(article_type)))
  
  # Delay requests
  Sys.sleep(1)
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                        Keywords                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # Get (first 6) keywords 
  keyword_data <- webpage %>%
                  html_nodes(".badge-type") %>%
                  html_text() %>%
                  as_data_frame() %>%
                  rownames_to_column("keywords") %>%
                  head(6) %>% 
                  mutate(keywords = paste("keyword",keywords),
                         value    = tolower(value)) %>%
                  spread(keywords, value) %>%
                  mutate(`keyword 1`  = ifelse('keyword 1' %in% names(.), `keyword 1`, ""),
                         `keyword 2`  = ifelse('keyword 2' %in% names(.), `keyword 2`, ""),
                         `keyword 3`  = ifelse('keyword 3' %in% names(.), `keyword 3`, ""),
                         `keyword 4`  = ifelse('keyword 4' %in% names(.), `keyword 4`, ""),
                         `keyword 5`  = ifelse('keyword 5' %in% names(.), `keyword 5`, ""),
                         `keyword 6`  = ifelse('keyword 6' %in% names(.), `keyword 6`, ""))
  if (is_empty(keyword_data$`keyword 1`)) {rm(keyword_data)
    keyword_data <- data.frame(`keyword 1`= "",`keyword 2`= "",`keyword 3`= "",`keyword 4`= "",`keyword 5`= "",`keyword 6`= "",stringsAsFactors=FALSE)}
  
  # Delay requests
  Sys.sleep(1)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                        Downloads                                                         #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  downloads <- webpage %>% 
               html_nodes(".article-downloads") %>%
               html_text() %>%
               as_data_frame() %>%
               mutate(downloads = as.numeric(gsub("[^0-9]","",value))) %>%
               select(downloads)
  
  # Delay requests
  Sys.sleep(1)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                        References                                                        #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Nr of references
  refs <- webpage %>%
          html_nodes(".ref__number") %>%
          html_text() %>%
          as_data_frame() %>%
          mutate(refs = as.numeric(value)) %>%
          select(refs) %>%
          summarise(references = max(refs))
  
  # Delay requests
  Sys.sleep(1)

    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Combine data and save                                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  #Combine all scraped information into one dataframe
  article_info <- bind_cols(doi=as.character(doi),article_type,timeline,downloads,refs,keyword_data,abstract) 
  
  
  # ------------------------------------------------------
  # Return scraped article info
  # ------------------------------------------------------   
  
      return(article_info)
  
}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################