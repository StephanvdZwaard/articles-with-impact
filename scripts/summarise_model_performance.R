# ------------------------------------------------------------------------------------------------------------------------ #
#                            Script for summarizing model performance of both RF-models and naive baseline models          #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Summarise mean absolute errors and effect sizes of random forest and naive basline models                  #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


summarise_model_performance <- function(base_results, rf_results) { 
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                    Summarise results of modelling for table 2                                            #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Print MAE and confidence interval (95%) based on absolute errors
  rf_ci <- rf_results %>% summarise(mae   = round(CI(abs_err, ci = 0.95), digits=2)) 

  
  # Print MAE and confidence interval (95%) based on absolute errors
  base_ci <- base_results %>% summarise(mae   = round(CI(abs_err, ci = 0.95), digits=2))

  
  # Determine effect size between RF model and baseline model.
  ES <- cohen.d(base_results$abs_err, 
                rf_results$abs_err, 
                paired = T)

  
  # Perform t-test
  t.test(base_results$abs_err, y =rf_results$abs_err,
         alternative = "two.sided",
         paired = TRUE,
         conf.level = 0.95)

  
  # Summarize
  summary <- data.frame(model    = c("RF","NB"),
                        MAE      = c(round(mean(rf_results$abs_err),2),       round(mean(base_results$abs_err),2)),
                        MAE_ci   = c(paste0("(",rf_ci$mae[3]," - ",rf_ci$mae[1],")"), paste0("(",base_ci$mae[3]," - ",base_ci$mae[1],")")),
                        diff     = c(paste0("-",round((mean(base_results$abs_err) - mean(rf_results$abs_err)) / mean(base_results$abs_err)*100,1),"%"),NA),
                        ES       = c(paste0(round(ES$estimate,2)," (",round(ES$conf.int[1],2)," - ",round(ES$conf.int[2],2),")"),NA),
                        ES_magn  = c(as.character(ES$magnitude),''))
  
  
  # ------------------------------------------------------
  # Return necessary results for table 2
  # ------------------------------------------------------   
  
      return(summary)
  
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################