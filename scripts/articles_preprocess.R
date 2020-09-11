# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for preprocessing article characteristics and impact metrics                            #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Preprocess article impact metrics and other article characteristics                                        #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

articles_preprocess <- function(data_pubmed) {

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Required libraries
  require(dplyr)
  require(stringr)
  require(readxl)
  require(countrycode)
  require(RecordLinkage)
  
  
  # Set time to English language
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                 Preprocess data                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  data_processed <- data_pubmed %>%

            # Group article types into review, viewpoint/perspective, research article and editorial/corrigendum
              mutate(article_type = case_when(str_detect(tolower(article_type),c("editor|corrigend|retraction"))~"editorial | corrigendum",
                                              str_detect(tolower(article_type),c("highlight|articles|research|innovative|lighted|cores|physiology"))~"research article",
                                              str_detect(tolower(article_type),c("point-counterpoint|point:|viewpoint|perspective"))~"viewpoint | perspective",
                                              str_detect(tolower(article_type),c("review|synthesis"))~"review",
                                              TRUE ~ tolower(article_type))) %>%
              mutate(article_type = case_when(article_type != "review" & str_detect(tolower(abstract),"review") & ! doi %in% c("https://doi.org/10.1152/japplphysiol.01258.2012","https://doi.org/10.1152/japplphysiol.00376.2013") ~ "review",
                                              TRUE ~ article_type)) %>%
    
    
            # Clean variables and make numeric
              mutate(volume                 = as.numeric(volume),
                     issue                  = as.numeric(issue),
                     references             = ifelse(is.finite(references)==F,0,references),
                     alt_rank_journal       = as.numeric(alt_rank_journal),
                     alt_rank_all_sim       = as.numeric(alt_rank_all_sim),
                     alt_rank_journal_sim   = as.numeric(alt_rank_journal_sim),
                     alt_pct_all_sim        = as.numeric(alt_pct_all_sim),
                     alt_pct_journal_sim    = as.numeric(alt_pct_journal_sim),
                     n_authors              = ifelse(is.na(n_authors),0,n_authors),
                     Under_review           = ifelse(is.na(Under_review),0,Under_review)) %>%

    
            # Add epub date
              mutate(citations  = as.character(citations),
                     In.Press   = as.POSIXct(gsub("/n","",gsub("^.*Epub ","",citations)),format = "%Y %b %d"),
                     alt_update = as.POSIXct(as.character(alt_update),format ="%Y-%m-%d %H:%M:%S")) %>%
              select(doi:Accepted,In.Press,everything())
  

            # Renaming variables
              data_processed <- data_processed %>% 
                                dplyr::rename(title_full          = title,
                                              title_n_words       = words_title,
                                              authors_full        = authors,
                                              authors_n           = n_authors,
                                              pub_n_pages         = page_nr,
                                              pub_volume          = volume,
                                              pub_issue           = issue,
                                              pub_article_type    = article_type,
                                              pub_date_submit     = Received,
                                              pub_date_accept     = Accepted,
                                              pub_date_inpress    = `In.Press`,
                                              pub_date_online     = `Published online`,
                                              pub_date_inprint    = `Published in print`,
                                              pub_review_n_days   = Under_review,
                                              abstr_full          = abstract,
                                              abstr_n_words       = abstract_length,
                                              pub_n_refs          = references,
                                              keywords_1          = `keyword.1`,
                                              keywords_2          = `keyword.2`,
                                              keywords_3          = `keyword.3`,
                                              keywords_4          = `keyword.4`,
                                              keywords_5          = `keyword.5`,
                                              keywords_6          = `keyword.6`,
                                              pub_metr_downloads  = downloads,
                                              pub_metr_altmetrics = altmetric_score,
                                              pub_metr_citations  = cited) 
                
              
            # Determine average review time (for imputation of missing values)
              delta_review   <- data_processed %>% summarise(mean = round(mean(pub_review_n_days,na.rm=T),0))
            
              data_processed <- data_processed %>%
              
                                # Processing and imputation of dates related to submission, acceptance and in press.
                                mutate(pub_date_submit      = as.POSIXct(as.character(pub_date_submit)),
                                       pub_date_accept      = as.POSIXct(as.character(pub_date_accept))) %>%

                                mutate(pub_date_inpress     = ifelse(is.na(pub_date_inpress), pub_date_accept + 5,  pub_date_inpress)) %>% #mean #days between accept and inpress = 4.4 days
                                mutate(pub_date_accept      = ifelse(is.na(pub_date_accept),  pub_date_inpress - 5, pub_date_accept)) %>%
                                mutate(pub_date_submit      = ifelse(is.na(pub_date_submit),  pub_date_accept - delta_review$mean, pub_date_submit)) %>%
                                mutate(pub_date_submit      = as.POSIXct(pub_date_submit,origin="1970-01-01"),
                                       pub_date_accept      = as.POSIXct(pub_date_accept,origin="1970-01-01"),
                                       pub_date_inpress     = as.POSIXct(pub_date_inpress,origin="1970-01-01")) %>%
                                mutate(pub_date_submit      = case_when(is.na(pub_date_submit) & pub_article_type == "viewpoint | perspective" ~ pub_date_inprint,
                                                                        is.na(pub_date_submit) & pub_article_type == "editorial | corrigendum" ~ pub_date_inprint,
                                                                        TRUE ~ pub_date_submit),
                                       pub_date_inpress     = case_when(is.na(pub_date_inpress) & pub_article_type == "viewpoint | perspective" ~ pub_date_inprint,
                                                                        is.na(pub_date_inpress) & pub_article_type == "editorial | corrigendum" ~ pub_date_inprint,
                                                                        TRUE ~ pub_date_inpress),
                                       pub_date_accept      = case_when(is.na(pub_date_accept) & pub_article_type == "viewpoint | perspective" ~ pub_date_inprint,
                                                                        is.na(pub_date_accept) & pub_article_type == "editorial | corrigendum" ~ pub_date_inprint,
                                                                        TRUE ~ pub_date_accept)) %>%
            
                                # Select and arrange columns within the data frame.
                                select(doi,starts_with("pub"),starts_with("authors"),starts_with("collab"),starts_with("title"),starts_with("keywords"),starts_with("abstract"),everything())
              
              
    # ------------------------------------------------------
    # Save preprocessed data
    # ------------------------------------------------------   
              
      #save(data_processed,file="data/data_processed.RData")
    
              
      # Reset datetime to Dutch.        
      Sys.setlocale("LC_TIME", "nl_NL.UTF-8")        
            
            
    return(data_processed)
  
}  


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################