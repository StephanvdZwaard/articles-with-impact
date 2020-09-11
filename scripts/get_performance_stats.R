# ------------------------------------------------------------------------------------------------------------------------ #
#                            Script for retrieving Table 3: information on most important features                         #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Get most important features from Random Forest models based on permutated feature importance               #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

get_performance_stats <- function(model_altmetrics, model_downloads, model_citations) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Load libraries
    require(dplyr)
    
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                    Get results for model performance for RF models                                       #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
    #Attention score
    model_perf_1 <- evaluate_model_performance(model_altmetrics$`RF-model`, 
                                               model_altmetrics$`test-set`, output="model_performance") 

    #Downloads
    model_perf_2 <- evaluate_model_performance(model_downloads$`RF-model`, 
                                               model_downloads$`test-set`, output="model_performance") 

    #Citations
    model_perf_3 <- evaluate_model_performance(model_citations$`RF-model`, 
                                               model_citations$`test-set`,  output="model_performance") 

    
    # Combine balanced accuracy in one data frame.
    bal_acc <- data.frame(metric = c("attention score","downloads","citations"),
                          top10  = c(model_perf_1$CM_top10$byClass[11],model_perf_2$CM_top10$byClass[11],model_perf_3$CM_top10$byClass[11]),
                          top25  = c(model_perf_1$CM_top25$byClass[11],model_perf_2$CM_top25$byClass[11],model_perf_3$CM_top25$byClass[11])) %>%
               mutate(top10 = round(top10,2),
                      top25 = round(top25,2)) %>% select(metric,top25,top10) 
    
    
    # Combine R squared of actual vs predicted values in one data frame.
    expl_var <- data.frame(metric = c("attention score","downloads","citations"),
                           R2     = c(model_perf_1$lm$r.squared,model_perf_2$lm$r.squared,model_perf_3$lm$r.squared)) %>%
                mutate(R2 = round(R2,2)) 

    
    # ------------------------------------------------------
    # Save results for table 3
    # ------------------------------------------------------   
    
      return(list("balanced_accuracy" = bal_acc,
                  "explained_variance"= expl_var))

    
}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################