get_effect_sizes <- function(data_features, table=F, filter=T) {
  
  
  # Remove variables that will not be incorporated in the model (mostly verbose variables, such as the abstract, authors
  # or full description of the keywords.
  data_model     <-  data_features %>%
                     select(-c(doi,citations,affiliations,
                               collab_countries,collab_continents,keywords,
                               journal:alt_img,
                               rcr, fcr, recent_cited, 
                               contains("pub_date"),     
                               ends_with("full"),        ends_with("cutoff"),
                               ends_with("top10"),       ends_with("t10"),
                              starts_with("keywords_"), starts_with("Time_"))) %>%
                      mutate(title_noun_chunks  = ifelse(is.infinite(title_noun_chunks),NA,title_noun_chunks))
  
  # Dummy coding of categorical variables
  category_data  <- data_features %>% 
                    select(pub_article_type,pub_season_inpress,pub_season_inprint,pub_season_submit) %>%
                    mutate(pub_article_type   = factor(pub_article_type,  levels=c("edit_corr","view_persp","research","review")),
                           pub_season_inpress = factor(pub_season_inpress,levels=c("summer","autumn","winter","spring")),
                           pub_season_inprint = factor(pub_season_inprint,levels=c("summer","autumn","winter","spring")),
                           pub_season_submit  = factor(pub_season_submit, levels=c("summer","autumn","winter","spring")))
  category_data  <- fastDummies::dummy_cols(category_data,  remove_first_dummy = F) %>% select(-c(1:4))
  data_model     <- data_model %>%
                    select(-c(pub_article_type,pub_season_inpress,pub_season_inprint,pub_season_submit)) %>%
                    cbind(category_data)
  
  # Get effect sizes
  parms <- data_model %>% select(-c(contains("pub_metr"), pub_weekday_inpress)) %>% colnames()
  results    <- c()
  
  for (p in parms) {
    res <-  get_mean_impact(data_model,p)
    if (p == parms[1]) {
      results <- res
    } else {
      results <- rbind(res,results)
    }
  }
  
  data_all <- results %>%
              mutate(subject = case_when(str_detect(feature,"title_") ~ "title",
                                         str_detect(feature,"collab_") ~ "collaboration",
                                         str_detect(feature,"authors_n") ~ "collaboration",
                                         str_detect(feature,"abstr_") ~ "abstract",
                                         str_detect(feature,"pub_") ~ "publication",
                                         str_detect(feature,"altmetrics_") ~ "engagement",
                                         str_detect(feature,"key_") ~ "topic",
                                         TRUE ~ "unknown")) %>%
              mutate(subject = ifelse(feature == "abstr_human_study" | feature == "abstr_animal_study","publication",subject)) 
  
  
  # 
  if (table == F & filter == F) {
    
    effect_sizes <- data_all %>%
                    arrange(rowname,subject,desc(abs(eff_size))) 
    
  } else if (table == F & filter == T) {
    
    effect_sizes <- data_all %>%
                    filter(n>25) %>%
                    group_by(rowname,subject) %>% 
                    arrange(rowname,subject,desc(abs(eff_size))) %>% 
                    filter(row_number()<=ifelse(subject=="topic",10,3)) %>% 
                    ungroup()
    
  } else {
    
    effect_sizes <- data_all %>%  
                    mutate_at(.vars = vars(contains("eff_ci"),contains("eff_size")),
                              .funs = list(~round(.,2))) %>%
                    dplyr::rename(impact_metric = rowname) %>%
                    mutate(eff_ci = paste0(eff_ci_low," – ",eff_ci_high),
                           eff_size = paste0(toupper(substr(eff_magn,1,1)), " (", ifelse(eff_size<0,"-","+"),")")) %>%
                    select(impact_metric, subject, feature, eff_size, eff_ci) 
    
  }

  return(effect_sizes)
  
}