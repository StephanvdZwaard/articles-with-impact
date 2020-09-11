# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for performing the Machine Learning with a Random Forest model                          #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Perform machine learning by modeling the random forest algorithm for the specified impact metric           #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


perform_RF_modeling <- function(data_model, impact_metric="pub_metr_downloads", test_set=data.frame(), train_set = data.frame()) {

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #


    # Load libraries
      require(rpart)
      require(caret)
      require(dplyr)
      require(Metrics)
      require(tidyverse)
      require(vip)

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                          Data partitioning into train and test set                                       #
# ------------------------------------------------------------------------------------------------------------------------ #

  
    # If prior test-train set split is available use this data
    if (nrow(test_set)>0 & nrow(train_set)>0) {

      if (any(colnames(test_set)=="response")) {
        
      # Rename old target name ('response') to target
      test_set  <- test_set %>% dplyr::rename(target = response)
      train_set <- train_set %>% dplyr::rename(target = response)
      
      }
      
    # Else create new data partitioning  
    } else {
  
    # Set seed to ensure reproducible random number generator (used for partitioning)
    set.seed(998)
    
    
    # Define target and remove other impact metrics. Make sure there are no NA values or zeros within the specified target.
    data        <- data_model %>% 
                   dplyr::rename_(.dots = setNames(impact_metric,"target")) %>%
                   mutate(target = as.numeric(target)) %>%
                   filter(!is.na(target)) %>%
                   select(-contains("pub_metr_")) %>%
                   select(-one_of("key_muscle","key_heat","key_disease","key_aging","key_training","key_performance")) # To comply with initial model of 16-04-20
    
    
    # Data partitioning into a train set (80% of records) and a test set (20% of records) 
    # The train set will be used to train the model and the test set to validate how well the model generalizes to unseen data.
    train_idx   <- createDataPartition(data$target, p = .8, list = FALSE)
    train_set   <- data[ train_idx,]
    test_set    <- data[-train_idx,]
      
    
    # Percentile rank scores were calculated from absolute scores of the target impact metric. This is done separately for the 
    # test and train set to avoid data leakage; making sure that data within test set is completely independent.
    train_set   <- train_set %>%
                   arrange(desc(target)) %>% 
                   mutate(target = percent_rank(target))
    test_set    <- test_set %>%
                   arrange(desc(target)) %>% 
                   mutate(target = percent_rank(target))
                  
    }
    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                           Machine learning: creating the RF model                                        #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    
    # Set seed to ensure reproducible random number generator (used for modeling)
    set.seed(825) # Be aware that set seed differs between 3.5.1 and later R versions (3.6 or higher) and therefore may result in slightly different models)

    
    # Specify 10-fold cross validation as the resampling method  
    fit_control <- trainControl(method = "cv",
                                number = 10)

    
    # TuneGrid (based on modelling on 16-4-2020) optimal mtry was 85 (attention score), 43 (downloads) or 105 (citations)
    grid_search <- expand.grid(mtry = c(2,5,10,22,43,64,85,105,126,147),
                               splitrule = 'variance',
                               min.node.size = 5)
    
    
    # Create the Random Forest regression model based on the train set (include feature importance based on permutation)
    ranger_model <- train(target ~ ., 
                          data       = train_set, 
                          method     = "ranger",
                          trControl  = fit_control,
                          tuneGrid   = grid_search,
                          importance = 'permutation')
    
    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                    Machine learning results: evaluating the RF model                                     #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    
    # Use RF model to predict target values within the independent test-set
    predicted_values <- predict(object  = ranger_model,
                                newdata = test_set)
    
    
    # Combine predicted and actual values in one data.frame
    model_results    <- data.frame(actual = test_set$target,
                                   predicted = predicted_values)
    
    
    # Preallocate and add label of modelled target
    ranger_results <- list()
    
    # Retrieve total error (Root Mean Square Error) 
    ranger_results$rmse <- rmse(actual    = model_results$actual,
                                predicted = model_results$predicted)
    
    
    # Retrieve the correlation between predicted and actual values
    ranger_results$cor <- cor(model_results)
    
    
    # Create a linear regression model based on the predicted and actual values
    ranger_results$lm <- lm(actual ~ predicted ,data = model_results) %>% summary()
    # par(mfrow = c(2, 2))
    # plot(lm_results)
    #summary(lm_results)
    
    
    # Visualize predicted versus actual target values.
    ggplot(model_results,aes(x=predicted,y=actual))+ geom_point()
    
    
    # Distinguish high-impact articles for the outstanding (top-10%) and excellent (top-25%) articles
    model_results <- model_results %>%
                     arrange(desc(actual)) %>%
                     mutate(actual_t25 = ifelse(percent_rank(actual) >= 0.75,1,0),
                            actual_t10 = ifelse(percent_rank(actual) >= 0.90,1,0)) %>%
                     arrange(desc(predicted)) %>%
                     mutate(predict_t25 = ifelse(percent_rank(predicted) >= 0.75,1,0),
                            predict_t10 = ifelse(percent_rank(predicted) >= 0.90,1,0))
         
    
    # Retrieve confusion matrix to obtain balanced accuracy of the model for detecting the 
    # top-10% and top-25% high-impact articles
    ranger_results$CM_top10 <- caret::confusionMatrix(as.factor(model_results$predict_t10),
                                                      as.factor(model_results$actual_t10))
    
    ranger_results$CM_top25 <- caret::confusionMatrix(as.factor(model_results$predict_t25),
                                                      as.factor(model_results$actual_t25))
    
    
    # Retrieve the most important featuers within the RF model based on feature importance
    ranger_results$vip <- vip::vip(ranger_model, num_features = 25, bar = T)
    
    
    # ------------------------------------------------------
    # Save results from machine learning
    # ------------------------------------------------------   
    
    
    # Save most important results
    name = case_when(impact_metric == "pub_metr_altmetrics" ~ "altmetrics",
                     impact_metric == "pub_metr_downloads"  ~ "downloads",
                     impact_metric == "pub_metr_citations"  ~ "citations")
    
    save(test_set, train_set, model_results, ranger_model, ranger_results, 
         file = paste0(getwd(),"/data/",name,"_m70_",format(Sys.Date(),"%m%y"),".RData"))
    
    
    # Return model
    return(list("target"     = impact_metric,
                "RF-model"   = ranger_model,
                "train-set"  = train_set,
                "test-set"   = test_set,
                "results"    = ranger_results))
    
    
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################