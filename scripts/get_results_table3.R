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

get_results_table3 <- function(data_features, model_altmetrics, model_downloads, model_citations) {

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #


  # Load libraries
    require(effsize)
    require(stats)
    require(writexl)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                             Get results based on feature importance and add effect size of features                      #
# ------------------------------------------------------------------------------------------------------------------------ #
  

    # Get all effect sizes from data features
    ES <- get_effect_sizes(data_features, table=T)
    
    
    # Altmetric scores
    important_features_1 <- get_important_features(model_altmetrics$`RF-model`) %>%
                            mutate(impact_metric = "pub_metr_altmetrics") %>%
                            left_join(ES, by = c("Variable"="feature","impact_metric"))%>%
                            select(subject, impact_metric, everything())
    
    
    # Downloads
    important_features_2 <- get_important_features(model_downloads$`RF-model`) %>%
                            mutate(impact_metric = "pub_metr_downloads") %>%
                            left_join(ES, by = c("Variable"="feature","impact_metric"))%>%
                            select(subject, impact_metric, everything())

    # Citations
    important_features_3 <- get_important_features(model_citations$`RF-model`) %>%
                            mutate(impact_metric = "pub_metr_citations") %>%
                            left_join(ES, by = c("Variable"="feature","impact_metric")) %>%
                            select(subject, impact_metric, everything())
    
    
    # Combine results in one data.frame for Table 3.
    table3 <- rbind(important_features_1,
                    important_features_2,
                    important_features_3)
    
    
  # ------------------------------------------------------
  # Save results for table 3
  # ------------------------------------------------------   
    
    
    # Write to excel
    write_xlsx(table3, path = "./results/Table3.xlsx")
    
    
    # Return
    return(table3)
    
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################