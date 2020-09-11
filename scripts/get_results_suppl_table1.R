# ------------------------------------------------------------------------------------------------------------------------ #
#                                      Script for retrieving Supplemental table 1                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve supplemental table 1 with articles with highest impact for attention scores, downloads & citations#
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

get_results_suppl_table1 <- function(data_features) {

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #


  # Load libraries
    require(effsize)
    require(stats)
    require(writexl)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                      Get articles with highest impact for attention scores, downloads and citations                      #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    # Combine results in one data.frame for Table 1.
    t1a <- data_features %>%
           mutate(pub_metr_altmetrics = round(pub_metr_altmetrics),
                  top5 = "top5 attention scores") %>%
           select(top5,authors_full, pub_year_inprint, title_full, contains("pub_metr")) %>% 
           arrange(desc(pub_metr_altmetrics)) %>% head(5)
    
    t1b <- data_features %>%
           mutate(top5 = "top5 downloads") %>%
           select(top5,authors_full, pub_year_inprint, title_full, contains("pub_metr")) %>% 
           arrange(desc(pub_metr_downloads)) %>% head(5)
    
    t1c <- data_features %>%
           mutate(top5 = "top5 citations") %>%
           select(top5,authors_full, pub_year_inprint, title_full, contains("pub_metr")) %>% 
           arrange(desc(pub_metr_citations)) %>% head(5)
    
    suppl_table1 <- rbind(t1a, t1b, t1c)
    
  # ------------------------------------------------------
  # Save results for supplemental table 1
  # ------------------------------------------------------   
    
    
    # Write to excel
    write_xlsx(suppl_table1, path = "./results/Suppl_table1.xlsx")
    
    
    # Return
    return(suppl_table1)
    
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################