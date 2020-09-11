# ------------------------------------------------------------------------------------------------------------------------ #
#                                      Script for retrieving Supplemental table 2                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve supplemental table 2 with effect sizes of the features provided in Fig 4-7                        #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

get_results_suppl_table2 <- function(data_features) {

  
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
    ES <- get_effect_sizes(data_features,filter=T) 
    ES <- ES %>% select(rowname, subject, feature, eff_size, eff_magn, eff_ci_low, eff_ci_high, n)
    
    
    # Combine results in one data.frame for Table 2.
    suppl_table2 <- ES %>% mutate(eff_ci = paste0("(",round(eff_ci_low,2)," - ",round(eff_ci_high,2),")")) %>%
                           select(-eff_ci_high,-eff_ci_low) %>% 
                           select(rowname,subject,feature,eff_size,eff_magn,eff_ci,n) %>%                          
                           dplyr::rename(impact_metric = rowname) %>%
                           mutate(subject = factor(subject, levels = c("topic","title","abstract","collaboration","publication","engagement")),
                                  impact_metric = factor(impact_metric, levels = c("pub_metr_altmetrics","pub_metr_downloads","pub_metr_citations"))) %>%
                           arrange(impact_metric,subject)
    
  # ------------------------------------------------------
  # Save results for supplemental table 2
  # ------------------------------------------------------   
    
    
    # Write to excel
    write_xlsx(suppl_table2, path = "./results/Suppl_table2.xlsx")
    
    
    # Return
    return(suppl_table2)
    
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################