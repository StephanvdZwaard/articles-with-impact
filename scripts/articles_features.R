# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for feature engineering based on preprocessed data                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Perform feature engineering to add predictors related to the article characteristics                       #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

articles_features <- function(data_processed) {

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Required libraries
  require(dplyr)
  require(stringr)
  require(lubridate)

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                 Perform feature engineering on preprocessed data                                         #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  data_features <- data_processed %>%
 
    
                # Add publication characteristics
                  mutate(pub_year_inprint     = year(pub_date_inprint),
                         pub_month_inprint    = month(pub_date_inprint),
                         pub_weekday_inprint  = wday(pub_date_inprint),
                         pub_season_inprint   = case_when(pub_month_inprint  >= 3   &  pub_month_inprint  < 6  ~ "spring",
                                                          pub_month_inprint  >= 6   &  pub_month_inprint  < 9  ~ "summer",
                                                          pub_month_inprint  >= 9   &  pub_month_inprint  < 12 ~ "autumn",
                                                          pub_month_inprint  >= 12  |  pub_month_inprint  <= 2 ~ "winter"
                                                  ), 
                         pub_month_inpress    = month(pub_date_inpress),  
                         pub_weekday_inpress  = wday(pub_date_inpress),
                         pub_season_inpress   = case_when(pub_month_inpress  >= 3   &  pub_month_inpress  < 6  ~ "spring",
                                                          pub_month_inpress  >= 6   &  pub_month_inpress  < 9  ~ "summer",
                                                          pub_month_inpress  >= 9   &  pub_month_inpress  < 12 ~ "autumn",
                                                          pub_month_inpress  >= 12  |  pub_month_inpress  <= 2 ~ "winter"
                                                    ),
                         pub_month_submit     = month(pub_date_submit),
                         pub_weekday_submit   = wday(pub_date_submit),
                         pub_season_submit    = case_when(pub_month_submit   >= 3   &  pub_month_submit   < 6  ~ "spring",
                                                          pub_month_submit   >= 6   &  pub_month_submit   < 9  ~ "summer",
                                                          pub_month_submit   >= 9   &  pub_month_submit   < 12 ~ "autumn",
                                                          pub_month_submit   >= 12  |  pub_month_submit   <= 2 ~ "winter"
                                                   )  
                  ) %>%
  
    
                 # Add title characteristics
                   mutate(title_n_char   = nchar(title_full),
                          title_qmark    = ifelse(str_detect(title_full,"\\?"),1,0),
                          title_brackets = ifelse(str_detect(title_full,"\\)"),1,0),
                          title_split    = ifelse(str_detect(title_full,":|;"),1,0)
                   ) %>%
                     
    
                 # Add abstract characteristics 
                   mutate(abstr_pvalue       = ifelse(str_detect(tolower(abstr_full),"p =|p <|p=|p<"),1,0),
                          abstr_n_pvalue     = str_count(tolower(abstr_full),"p =|p <|p=|p<"),
                          abstr_narr         = ifelse(str_detect(tolower(gsub("[[:punct:]]"," ",abstr_full))," we | i "),1,0),
                          abstr_n_narr       = str_count(tolower(gsub("[[:punct:]]"," ",abstr_full))," we | i "),
                          abstr_n_punct      = str_count(abstr_full,";|:|-"),
                          abstr_headers      = ifelse(str_detect(tolower(abstr_full),"results:|conclusion:|purpose:|aim:|methods:|background:"),1,0))
  
  
                # Features on author characteristics (including affilations and collaborations)
                  data_features <- get_affiliations_info(data_features)   
  
                  
                # Features on NLP title and abstract
                  data_features <- articles_nlp(data_features)  
                  
                  
                # Features on keywords
                  data_features <- articles_keywords(data_features)  
                  
                  
                # Some last cleaning
                  data_features <- data_features %>%
                                   mutate(title_noun_chunks                = ifelse(is.na(title_noun_chunks),0,title_noun_chunks),
                                          abstr_sentence_length_iqr        = ifelse(abstr_full == "no abstract",0,abstr_sentence_length_iqr),
                                          abstr_sentence_length_sd         = ifelse(abstr_full == "no abstract",0,abstr_sentence_length_sd),
                                          abstr_sentence_length_mean       = ifelse(abstr_full == "no abstract",0,abstr_sentence_length_mean),
                                          abstr_sentence_length_max        = ifelse(abstr_full == "no abstract",0,abstr_sentence_length_max),
                                          abstr_sentence_n                 = ifelse(abstr_full == "no abstract",0,abstr_sentence_n),
                                          abstr_noun                       = ifelse(abstr_full == "no abstract",0,abstr_noun),
                                          abstr_det                        = ifelse(abstr_full == "no abstract",0,abstr_det),
                                          abstr_perc_top25_verbs           = ifelse(abstr_full == "no abstract",0,abstr_perc_top25_verbs),
                                          abstr_perc_top25_nouns           = ifelse(abstr_full == "no abstract",0,abstr_perc_top25_nouns)) %>%
                    
                                mutate_at(vars(altmetrics_mentions_p_com:altmetrics_sources_p_wikipedia),function(x) ifelse(is.na(x),0,x)) %>%
                                mutate(   pub_article_type                 = case_when(pub_article_type == "viewpoint | perspective" ~ "view_persp",
                                                                                       pub_article_type == "editorial | corrigendum" ~ "edit_corr",
                                                                                       pub_article_type == "research article"        ~ "research",
                                                                                       TRUE ~ pub_article_type )) 
                  
     # ------------------------------------------------------
     # Save feature constructed data
     # ------------------------------------------------------   
                  
       #save(data_features, file = paste0(getwd(),"/data/data_features.RData"))
        
                            
       return(data_features)
  
}  


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################