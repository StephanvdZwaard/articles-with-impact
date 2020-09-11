# ------------------------------------------------------------------------------------------------------------------------ #
#                                    Script for retrieving Table 2: model performance                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Get model performance from Naive Baseline and Random Forest models based on MAE                            #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


get_results_table2 <- function(model_altmetrics, model_downloads, model_citations) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Load libraries
  require(effsize)
  require(stats)
  require(writexl)
  

# ------------------------------------------------------------------------------------------------------------------------ #
#                             Get results for model performance for RF models and Naive baseline models                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  

  # Altmetric scores
  rf_data      <- evaluate_model_performance(model_altmetrics$`RF-model`, 
                                             model_altmetrics$`test-set`) 
  
  base_data    <- evaluate_model_performance("baseline", 
                                             model_altmetrics$`test-set`, 
                                             model_altmetrics$`train-set`) 
  
  model_perf_1 <- summarise_model_performance(base_data, rf_data)
  

  # Downloads
  rf_data      <- evaluate_model_performance(model_downloads$`RF-model`, 
                                             model_downloads$`test-set`) 
  
  base_data    <- evaluate_model_performance("baseline", 
                                             model_downloads$`test-set`, 
                                             model_downloads$`train-set`) 
  
  model_perf_2 <- summarise_model_performance(base_data, rf_data)
  
  
  # Citations
  rf_data      <- evaluate_model_performance(model_citations$`RF-model`, 
                                             model_citations$`test-set`) 
  
  base_data    <- evaluate_model_performance("baseline",
                                             model_citations$`test-set`, 
                                             model_citations$`train-set`) 
  
  model_perf_3 <- summarise_model_performance(base_data, rf_data)
  
  
  # Combine results in one data.frame for Table 3.
  table2 <- rbind(model_perf_1,
                  model_perf_2,
                  model_perf_3)
  
  
  # ------------------------------------------------------
  # Save results for table 3
  # ------------------------------------------------------   
  
  # Write to excel
  write_xlsx(table2, path = "./results/Table2.xlsx")
  

  return(table2)

  
}
##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################