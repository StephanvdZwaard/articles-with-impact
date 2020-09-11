# ------------------------------------------------------------------------------------------------------------------------ #
#                                       Script for retrieving model performance                                            #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Get model performance on independent test set (both for naive baseline and RF models)                      #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


evaluate_model_performance <- function(ranger_model, test_set, train_set = data.frame(), output="model_results") {  
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # load libraries
  require(Rmisc)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                     Get actual and predicted values of test set                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # ---- Naive baseline model ----
  
  if (is.character(ranger_model)) {
    
    
    # If no model is provided as input argument, peform a naive baseline model.
    # Predict target values of independent test-set, based on naive baseline model (average of training set)
    model_results    <- data.frame(actual    = test_set$target,
                                   predicted = rep(mean(train_set$target),dim(test_set)[1]))
  
    
  # ---- Random Forest regression model ----
    
  } else {
    
    
    # Predict target values of independent test-set, based on RF model
    predicted_values <- predict(object  = ranger_model,
                                newdata = test_set)
    
    
    # get model results: predicted and actual values
    model_results    <- data.frame(actual    = test_set$target,
                                   predicted = predicted_values)
    
    
  } 

    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                  Calculate model performance                                             #
# ------------------------------------------------------------------------------------------------------------------------ #
  
    # Preallocate and add label of modelled target
    model_performance <- list()
  
    
    # Retrieve RMSE of actual vs predicted target values
    model_performance$rmse <- rmse(actual    = model_results$actual,
                                   predicted = model_results$predicted)
    
    # Create a linear regression model based on the predicted and actual values
    model_performance$lm <- summary(lm(actual ~ predicted, model_results))
  
    # Retrieve the correlation between predicted and actual values
    model_performance$cor <- cor(model_results)
    

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
    model_performance$CM_top10 <- caret::confusionMatrix(as.factor(model_results$predict_t10),
                                                         as.factor(model_results$actual_t10))

    model_performance$CM_top25 <- caret::confusionMatrix(as.factor(model_results$predict_t25),
                                                         as.factor(model_results$actual_t25))


    # Print MAE of actual vs predicted target values
    print(paste0("MAE: ",
          mae(actual     = model_results$actual,
               predicted = model_results$predicted)))


    # Calculate MAE for all records in test-set
    model_results <- model_results %>%
                     mutate(residuals = actual - predicted,
                            abs_err   = abs(actual - predicted))


    # Print MAE and confidence interval (95%) based on absolute errors
    model_results %>% summarise(mae   = round(CI(abs_err, ci = 0.95), digits=3)) %>% print()



  # ------------------------------------------------------
  # Return model performance parameters
  # ------------------------------------------------------

    if (output == "model_performance") {

      return(model_performance)

    } else {

      return(model_results)

    }

}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################