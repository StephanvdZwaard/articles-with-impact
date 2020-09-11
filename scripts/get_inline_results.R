get_inline_results <- function(data_features) {

    # Get necessary effect sizes
    ES <- get_effect_sizes(data_features,filter=F) %>% mutate(eff_size = round(eff_size,2),
                                                              feature = gsub("^altmetrics_","engagement_",feature)) %>% 
                                                       dplyr::rename(impact_metric=rowname)

    # Altmetrics
    es_1 <- ES %>% filter(feature %in% c('collab_usa','abstr_mention_aim','abstr_mention_goal','abstr_mention_objctve','engagement_sources_p_tweeters') & impact_metric =="pub_metr_altmetrics") %>% 
                   select(impact_metric,eff_size,feature) 
    
    # Downloads
    es_2 <- ES %>% filter(feature %in% c("title_qmark","authors_n") & impact_metric == "pub_metr_downloads") %>%
                   select(impact_metric,eff_size,feature)  
    
    # Citations
    es_3 <- ES %>% filter(feature %in% c('pub_article_type_research','pub_article_type_review','engagement_sources_p_feeds','engagement_sources_p_tweeters','engagement_mentions_p_doc','engagement_mentions_p_com') & impact_metric =="pub_metr_citations") %>% 
                   select(impact_metric,eff_size,feature)  


    # Average ES for keywords related to scope of the journal.  
    es_4 <- ES %>% filter(feature %in% c('key_neuromuscular','key_cardiovascular','key_muscle',
                                 'key_respiratory','key_exercise', 'key_t50_aging',
                                 'key_t50_vo2max', 'key_training', 'key_t50_altitude', 'key_t50_aging', 
                                 'key_t50_hypertrophy', 'key_gravity', 'key_heat', 'key_disease', 
                                 'key_t50_mitochondria')) %>% 
                  arrange(feature,impact_metric) %>%
                  mutate(impact_metric = "all") %>%
                  group_by(impact_metric,feature) %>%
                  dplyr::summarise(eff_size = round(mean(eff_size),2)) %>% 
                  select(impact_metric,eff_size,feature) 
    
    # Combine results
    inline_results <- bind_rows(es_1,
                                es_2,
                                es_3,
                                es_4)
    # Return results    
    return(inline_results)
    
}