# ------------------------------------------------------------------------------------------------------------------------ #
#                        Script for retrieving impact and effect sizes of articles based on single predictors              #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Get ES and impact of articles based on single predictors (e.g. with and without the predictor for binary   # 
#               variables or with above and below average values for numeric variables)                                    #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

get_mean_impact <- function(data_model,param,subject="") {  
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # Load libraries
  require(effsize)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#           Calculate impact (in percentile scores) and effect sizes of articles with/without a specific feature           #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # Impact of predictors related to the abstract was determined only for articles with an abstract.
  if (str_detect(param,"abstr_"))  {data_model <- data_model %>% filter(abstr_n_words != 0)}
  
  # Since PubMed only registred 1 affiliation up to 2014, impact of predictors related to author affiliation are determined
  # for articles published from 2014.
  if (str_detect(param,"collab_")) {data_model <- data_model %>% filter(pub_year_inprint >2013)}

  
  # From the data, we selected the specified predictor in the input arguments, and the three impact metrics
  data <- data_model %>% select_("pub_metr_downloads","pub_metr_citations","pub_metr_altmetrics",param) %>% rowid_to_column()
  colnames(data) <- c("rowid","pub_metr_downloads","pub_metr_citations","pub_metr_altmetrics","predictor")
      
  
  # For binary predictors we distinguish articles with and those without the predictor (e.g. with the topic 'exercise' or not).
  # For numeric predictors, we distinguish articles into above or below average scores.
  data <- data %>% ungroup() %>% 
                  mutate(predictor = as.numeric(predictor)) %>%
                  mutate(predictor = case_when(length(unique(predictor)) > 2 & predictor >  mean(predictor,na.rm=T) ~ 1,
                                               length(unique(predictor)) > 2 & predictor <= mean(predictor,na.rm=T) ~ 0,
                                               TRUE ~ predictor))
  
  
  # Percent rank impact scores were calculated for the three impact metrics and are combined into 1 dataframe.
  data_alt  <- data %>% filter(!is.na(pub_metr_altmetrics)) %>% arrange(desc(pub_metr_altmetrics)) %>%
               mutate(pub_metr_altmetrics = percent_rank(pub_metr_altmetrics)) %>% select(rowid,pub_metr_altmetrics)
  data_down <- data %>% filter(!is.na(pub_metr_downloads))  %>% arrange(desc(pub_metr_downloads)) %>%
               mutate(pub_metr_downloads = percent_rank(pub_metr_downloads)) %>% select(rowid,pub_metr_downloads)
  data_cit  <- data %>% filter(!is.na(pub_metr_citations)) %>% arrange(desc(pub_metr_citations)) %>%
               mutate(pub_metr_citations = percent_rank(pub_metr_citations)) %>% select(rowid,pub_metr_citations)
  data      <- data %>% select(-contains("pub_metr")) %>%
               left_join(data_alt, by=c("rowid")) %>%
               left_join(data_down, by=c("rowid")) %>%
               left_join(data_cit, by=c("rowid"))
  
  
  # Effect sizes were calculated for articles with or without the predictor (for binary variables) or for articles with
  # above or below average scores (for numeric variables). ES were calculated for all three impact metrics.
  data_no  <- data %>% filter(!is.na(pub_metr_altmetrics)) %>% filter(predictor == 0) %>% select(pub_metr_altmetrics)
  data_yes <- data %>% filter(!is.na(pub_metr_altmetrics)) %>% filter(predictor == 1) %>% select(pub_metr_altmetrics)
  a = cohen.d(data_yes$pub_metr_altmetrics, data_no$pub_metr_altmetrics,paired = F)
  n_samples <- c(NA,dim(data_yes)[1])
  print(a$estimate)
  
  data_no  <- data %>% filter(!is.na(pub_metr_downloads)) %>% filter(predictor == 0) %>% select(pub_metr_downloads)
  data_yes <- data %>% filter(!is.na(pub_metr_downloads)) %>% filter(predictor == 1) %>% select(pub_metr_downloads)
  b = cohen.d(data_yes$pub_metr_downloads,data_no$pub_metr_downloads, paired = F)
  n_samples <- c(n_samples,dim(data_yes)[1])
  print(b$estimate)
  
  data_no  <- data %>% filter(!is.na(pub_metr_citations)) %>% filter(predictor == 0) %>% select(pub_metr_citations)
  data_yes <- data %>% filter(!is.na(pub_metr_citations)) %>% filter(predictor == 1) %>% select(pub_metr_citations)
  c = cohen.d(data_yes$pub_metr_citations, data_no$pub_metr_citations, paired = F)
  n_samples <- c(n_samples,dim(data_yes)[1])
  print(c$estimate)
  
  
  # Average impact, effect sizes and confidence intervals of the effect size, number of articles within each group
  # and subjects were combined in one data frame to return.
  data <- data %>% 
                  group_by(predictor) %>%
                  select(pub_metr_altmetrics,pub_metr_downloads,pub_metr_citations) %>%
                  summarise_all(list(~mean(.,na.rm=T))) %>% t() %>% data.frame() %>% tibble::rownames_to_column() %>% 
                  setNames(c("rowname","mean_feature_no_or_low","mean_feature_yes_or_high")) %>%
                  mutate(diff      = as.numeric(as.character(mean_feature_yes_or_high))-as.numeric(as.character(mean_feature_no_or_low))) %>%
                  mutate(perc_diff = abs(diff) / as.numeric(as.character(mean_feature_no_or_low)) * 100,
                         feature   = param,
                         subject   = subject,
                         n         = n_samples) %>% filter(row_number()!=1) %>%
                  cbind(data.frame(eff_size = c(a$estimate, b$estimate, c$estimate),
                                   eff_magn = c(as.character(a$magnitude),as.character(b$magnitude),as.character(c$magnitude)),
                                   eff_ci_low  = c(a$conf.int[1], b$conf.int[1], c$conf.int[1]),
                                   eff_ci_high = c(a$conf.int[2], b$conf.int[2], c$conf.int[2])))
  
  # ------------------------------------------------------
  # Return results 
  # ------------------------------------------------------   
  
  return(data)
  
}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################