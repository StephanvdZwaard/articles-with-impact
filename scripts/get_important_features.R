get_important_features <- function(model) {
  
  # Load libraries
  require(vip)
  
  # Determine feature importance of model
  feature_importance <- vip::vip(model, num_features =170, bar = T) 
  
  
  # Get top 3 variables for each category based on feature importance
  features_title     <- feature_importance$data %>% filter(str_detect(Variable,"title_"))         %>% head(3)
  features_abstr     <- feature_importance$data %>% filter(str_detect(Variable,"abstr_") & 
                                                          !str_detect(Variable,"human|animal"))   %>% head(3)
  features_collab    <- feature_importance$data %>% filter(str_detect(Variable,"collab|authors")) %>% head(3)
  features_publish   <- feature_importance$data %>% filter(str_detect(Variable,"pub_") |
                                                           str_detect(Variable,"human|animal"))   %>% head(3)
  features_engage    <- feature_importance$data %>% filter(str_detect(Variable,"altmetric"))      %>% head(3)
  features_topic     <- feature_importance$data %>% filter(str_detect(Variable,"key"))            %>% head(4)
    
  
  # Combine into one data frame and return.
  features <- rbind(features_title, features_abstr, features_collab, features_publish, features_engage, features_topic) %>% 
              mutate(Importance = round(Importance, digits=1))
  
  return(features)
  
}