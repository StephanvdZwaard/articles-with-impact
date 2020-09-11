# ------------------------------------------------------------------------------------------------------------------------ #
#                                           Script for preparing data for modeling                                         #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Prepare data for modelling (multicollinearity, dummification, near-zero-variance)                          #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


prepare_data_modeling <- function(data_features) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
    # Load libraries
      require(fastDummies)
      require(stats)
      require(caret)
      require(tidyverse)
      
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                              Prepare data for modeling                                                   #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
      # Remove variables that will not be incorporated in the model (mostly verbose variables, such as the abstract, authors
      # or full description of the keywords.
        data_model     <-  data_features %>%
                           select(-c(doi,citations,affiliations,
                                     collab_countries,collab_continents,keywords,
                                     journal:alt_img,
                                     rcr, fcr, recent_cited, 
                                     contains("pub_date"),     contains("pub_metr"),
                                     ends_with("full"),        ends_with("cutoff"),
                                     ends_with("top10"),       ends_with("t10"),
                                     starts_with("keywords_"), starts_with("Time_")))
        targets        <- data_features %>% select(contains("pub_metr"))          
        
      # Dummy coding of categorical variables
        category_data  <- data_features %>% 
                          select(pub_article_type,pub_season_inpress,pub_season_inprint,pub_season_submit) %>%
                          mutate(pub_article_type   = factor(pub_article_type,  levels=c("edit_corr","view_persp","research","review")),
                                 pub_season_inpress = factor(pub_season_inpress,levels=c("summer","autumn","winter","spring")),
                                 pub_season_inprint = factor(pub_season_inprint,levels=c("summer","autumn","winter","spring")),
                                 pub_season_submit  = factor(pub_season_submit, levels=c("summer","autumn","winter","spring")))
        category_data  <- fastDummies::dummy_cols(category_data,  remove_first_dummy = T) %>%
                          select(-c(1:4))
        data_model     <- data_model %>%
                          select(-c(pub_article_type,pub_season_inpress,pub_season_inprint,pub_season_submit)) %>%
                          cbind(category_data)
  
        
      # Check for columns with zero variance and remove these  
        nzv            <- nearZeroVar(data_model, freqCut = 95/5, uniqueCut = 10, saveMetrics = T)
        data_model     <- data_model[,!nzv$zeroVar]
        dim(data_model)
        
        
      # Check for highly correlated variables and remove these: multicollinearity (r > 0.70)
        data_model_cor  <- cor(data_model) 
        highly_cor      <- findCorrelation(data_model_cor, cutoff = .70)
        colnames(data_model)[highly_cor]
        data_model_filt <- data_model[,-highly_cor]

        
      # Add target variables again
        data_model <- cbind(targets,data_model_filt)
        
        
   # ------------------------------------------------------
   # Return data
   # ------------------------------------------------------   
        
     return(data_model)

}  

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################