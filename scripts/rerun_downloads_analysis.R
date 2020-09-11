# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for rerunning the analysis with the correct no. of downloads                            #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Rerun analysis for impact of downloads with accurate values of HTML and PDF downloads                      #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         14-08-2020                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


# The Journal of Applied Physiology has adjusted their website in 2017. After submitting our manuscript we noticed that 
# some publications had fewer downloads now than on January first 2020. We asked the APS webmaster how it could be possible
# that the downloads actually decreased? We scraped the no. of downloads from the website, but of course wanted to make sure
# that these represent accurate download numbers. After contact with the webmaster it turned out that the downloads that are
# provided on the website are only the HTML downloads of the past year. This meas that it is actually possible that the no. of
# downloads decrease. We received the total accumulated number of downloads from the launch of the new website (including both
# HTML and PDF downloads) on 1 January 2020 and rerun our analysis in this script.
# The webmaster of the journal has indicated that they 


# Load libraries
  library(readxl); 
  library(tidyverse)


# Load dataframe of processed data
  load("data/data_features_0720.RData")

  
# Save data for webmaster to provide the accurate HTML + PDF downloads
  data_features %>% 
  select(doi,authors_full, title_full) %>% 
  write.csv(file="article-with-impact-doi.csv")
  
  
# Retrieved accurate HTML + PDF downloads from the webmaster on 13-08-2020. Load correct no. of downloads
  correct_downloads <- read_excel("data/JAPPL-downloads-130820.xlsx") 
  correct_downloads <- correct_downloads %>%
                       dplyr::rename(downloads = `Full-Text Accesses, 2017-2019`) %>%
                       select(doi,downloads)


# Join datasets
  correct_data      <- data_features %>%
                       left_join(correct_downloads, by=c("doi")) 
                     

# Plot differences
  ggplot(correct_data, aes(x=pub_metr_downloads, y=downloads)) + geom_point(alpha=.3) + geom_abline(intercept = 0, slope = 1)
  ggplot(correct_data, aes(x=percent_rank(pub_metr_downloads), y=percent_rank(downloads))) + geom_point(alpha=.3)
  
  
# Plot correlations (download scores are highly correlated; r = 0.98)
  cor(correct_data$downloads, correct_data$pub_metr_downloads, method="spearman")
  cor(percent_rank(correct_data$downloads), percent_rank(correct_data$pub_metr_downloads), method="spearman")
  
  
# Create linear model to determine slope between accurate and previous no. of downloads
# Downloads are 40% higher now, with an R2 of 0.96.
  summary(lm(downloads ~ pub_metr_downloads, data = correct_data))
     
  
# Replace downloads feature with the accurate numbers
  data_features <- correct_data %>% 
                   mutate(pub_metr_downloads = downloads) %>% 
                   select(-downloads)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Machine learning                                                     #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Prepare dataset for modeling (multi-collinearity, dummification, near-zero-variance).
  data_model <- prepare_data_modeling(data_features) 
  
  # Save relevant data
  save(data_features, data_model,
       file = paste0(getwd(),"/data/data_features_0820.RData"))

  ##########################################################
  #------- Uncomment to rerun the modeling ----------
  ##########################################################
  
  # Load models for attention scores and citations.
  load(paste0(getwd(),"/data/initial-models/RF-models_repro_0820.RData"))
  
  # Perform machine learning by modeling the random forest algorithm for the specified impact metric
  ranger_model_downloads  <- perform_RF_modeling(data_model,"pub_metr_downloads")

  # Save relevant modeling results
  save(ranger_model_downloads,
       file = paste0(getwd(),"/data/RF-models_",format(Sys.Date(),"%m%y"),".RData"))
  
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                      Results for Articles-with-impact manuscript                                         #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # Load modeling results for new downloads
  
    #New downloads
    load(paste0(getwd(),"/data/RF-models_0820.RData"))
    load(paste0(getwd(),"/data/data_features_0820.RData"))
  
    # Old downloads:
    # load(paste0(getwd(),"/data/initial-models/RF-models_repro_0820.RData"))
    # load(paste0(getwd(),"/data/data_features_0720.RData"))
    # data_model <- prepare_data_modeling(data_features) 
    
  # ------------------------------------------------------
  # In-text results
  # ------------------------------------------------------ 
  
      # Get modeling results for balanced accuracy & R2
      res <- evaluate_model_performance(ranger_model_downloads$`RF-model`,ranger_model_downloads$`test-set`,output="model_performance")
      print(paste0("R2 for predicted vs actual: ",res$lm$r.squared))
      print(paste0("Balanced acc for top-25% papers: ",res$CM_top25$byClass[11]))
      print(paste0("Balanced acc for top-10% papers: ",res$CM_top10$byClass[11]))
      
      
      # Average attention scores, downloads and citations
      data_model %>% 
        select(contains("pub_metr")) %>% 
        summarise_all(~list(round(mean(.,na.rm=T),1)))
      
      
      # Display correlations for impact measures
      data_model %>% 
        select(contains("pub_metr")) %>% 
        cor(use="pairwise.complete.obs",
            method='spearman')
      
      
      # Get effect sizes for results & discussion section
      ES <- get_effect_sizes(data_features,filter=F) %>% mutate(eff_size = round(eff_size,2),
                                                                feature = gsub("^altmetrics_","engagement_",feature)) %>% 
                                                         dplyr::rename(impact_metric=rowname)
      
      # Mention ES of variables that are not in top-3 anymore.
      ES %>% filter(feature %in% c("title_qmark","authors_n") & impact_metric == "pub_metr_downloads")
      ES %>% filter(feature %in% c("title_x","collab_brazil") & impact_metric == "pub_metr_downloads")
      
      # Average ES for keywords related to scope of the journal.  
      ES %>% filter(feature %in% c('key_neuromuscular','key_cardiovascular','key_muscle',
                                      'key_respiratory','key_exercise', 'key_t50_aging',
                                      'key_t50_vo2max', 'key_training', 'key_t50_altitude', 'key_t50_aging', 
                                      'key_t50_hypertrophy', 'key_gravity', 'key_heat', 'key_disease', 
                                      'key_t50_mitochondria')) %>% 
                arrange(feature,impact_metric) %>%
                select(eff_size,feature) %>% 
                group_by(feature) %>%
                dplyr::summarise(eff_size = round(mean(eff_size),2)) %>% print()
  
      
  # ------------------------------------------------------
  # Figures
  # ------------------------------------------------------ 
      
      
      # Figure 4 & 6
      ES <- get_effect_sizes(data_features,filter=T) 
      ES <- ES %>% filter(rowname == 'pub_metr_downloads') %>% select(feature, mean_feature_yes_or_high)
  
      
  # ------------------------------------------------------
  # Tables
  # ------------------------------------------------------ 
  
      
      # Table 2: Model performance
      table2 <- get_results_table2(ranger_model_altmetrics, 
                                   ranger_model_downloads, 
                                   ranger_model_citations)

      
      # Table 3: Important features
      table3 <- get_results_table3(data_features, 
                                   ranger_model_altmetrics, 
                                   ranger_model_downloads, 
                                   ranger_model_citations) %>% filter(impact_metric=="pub_metr_downloads")

      
      
  
##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################  