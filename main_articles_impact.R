# ------------------------------------------------------------------------------------------------------------------------ #
#                                       Script for the analysis of Articles with impact                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve article impact from citations, downloads and attention scores and                                 #
#               perform a machine-learning analysis to predict article impact based on article characteristics             #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02)                                                                                         #
#                                                                                                                          #
# Publication:  vd Zwaard et al. 2020. Articles with impact: insights into 10 years of research with machine learning      #
# DOI:          doi: 10.1152/japplphysiol.00489.2020                                                                       #
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
 


# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #


  # Set path to the analysis subfolder
    #setwd(paste0(getwd(),"/7. Analysis"))
  
    
  # ------------------------------------------------------
  # Import libraries
  # ------------------------------------------------------   
    
    # Documentation
      library(packrat);  library(writexl)
    
    # Data collection (incl. webscraping and connecting to API)
      library(httr);     library(jsonlite);    library(readxl)
      library(rvest);
    
    # Data preprocessing
      library(dplyr);    library(purrr);       library(data.table)
      library(tidyr);    library(stringr);     library(zoo)
      library(anytime);  library(lettercase);  library(lubridate)
      library(tibble);   library(countrycode); library(RecordLinkage)
      library(stringi);  library(tidyverse)
    
    # Text mining analysis
      library(udpipe);   library(lattice);    
    
    # Machine learning analysis
      library(caret);    library(Metrics);     library(stats);  
      library(vip);      library(fastDummies); library(ranger)
      library(rpart);
  
    # Statistics
      library(effsize);  library(Rmisc)
    
    # Data visualization
      library(ggplot2);  library(ggpubr);      library(wordcloud2)
      library(webshot);  library(htmlwidgets)

    
  # ------------------------------------------------------
  # Set options
  # ------------------------------------------------------   
    
    
    # set options
    options(stringsAsFactors = FALSE)
  
    
  # ------------------------------------------------------
  # Load helper scripts
  # ------------------------------------------------------   
    
    
    # Data collection (incl. webscraping and connecting to API)
    source("scripts/collect_article_data.R")
    source("scripts/collect_pubmed_data.R")
    source("scripts/get_title.R")
    source("scripts/get_authors.R")
    source("scripts/get_article_info_jappl.R")
    source("scripts/get_doi.R")
    source("scripts/get_page.R")
    source("scripts/get_altmetric_score.R")
    source("scripts/get_dimensions_score.R")
    source("scripts/insert_rows.R")
    
    # Data preprocessing
    source("scripts/articles_preprocess.R")
    
    # Feature engineering
    source("scripts/articles_features.R")
    source("scripts/articles_nlp.R")
    source("scripts/articles_keywords.R")
    source("scripts/get_affiliations_info.R")

    # Machine learning and models
    source("scripts/prepare_data_modeling.R")
    source("scripts/perform_RF_modeling.R")
    source("scripts/evaluate_model_performance.R")
    source("scripts/summarise_model_performance.R")
    source("scripts/get_performance_stats.R") 
    
    # Data visualization, results and stats
    source("scripts/multiplot.r")
    source("scripts/get_mean_impact.R")
    source("scripts/get_inline_results.R")
    source("scripts/get_important_features.R") 
    source("scripts/get_effect_sizes.R") 
    source("scripts/get_thermometer_plot.R") 
    source("scripts/fmt_dcimals.R")
    source("scripts/plot_fig1_impact.R")
    source("scripts/plot_fig2_impact.R")
    source("scripts/print_cutoffs.R")
    source("scripts/plot_fig3_impact.R")
    source("scripts/plot_fig4_impact.R")
    source("scripts/plot_fig5_impact.R")
    source("scripts/plot_fig6_impact.R")
    source("scripts/plot_fig7_impact.R")
    source("scripts/get_results_table2.R") 
    source("scripts/get_results_table3.R") 
    source("scripts/get_results_suppl_table1.R") 
    source("scripts/get_results_suppl_table2.R") 


    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Data collection                                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Obtain data of citations, downloads and attention scores as well as a broad collection of article characteristics.
  data_raw    <- collect_article_data()
  
  
  # Add information about author affiliation based on the PubMed database
  data_pubmed <- collect_pubmed_data(data_raw) 
   
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Data preprocessing                                                   #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Pre-process the collected data (including formatting, naming and categorization) 
  data_processed <- articles_preprocess(data_pubmed) 
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Feature-engineering                                                  #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Add features based on abstract, title, publication, natural language processing with keywords, title and abstract and 
  # author affiliations
  data_features <- articles_features(data_processed) 

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Machine learning                                                     #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Load required data
  load(paste0(getwd(),"/data/data_features_0820.RData"))
  
  
  # Prepare dataset for modeling (multi-collinearity, dummification, near-zero-variance).
  data_model <- prepare_data_modeling(data_features) 
  
  
  # Perform machine learning by modeling the random forest algorithm for the specified impact metric 
  load(paste0(getwd(),"/data/initial-models/altmetrics_m70_0420.RData"))
  ranger_model_altmetrics <- perform_RF_modeling(data_model,"pub_metr_altmetrics",test_set, train_set)
  
  ranger_model_downloads  <- perform_RF_modeling(data_model,"pub_metr_downloads")
  
  load(paste0(getwd(),"/data/initial-models/citations_m70_0420.RData"))
  ranger_model_citations  <- perform_RF_modeling(data_model,"pub_metr_citations",test_set, train_set)
  
  
  # Save modeling results
  save(ranger_model_altmetrics, ranger_model_downloads, ranger_model_citations,
       file = paste0(getwd(),"/data/RF-models_TEST_",format(Sys.Date(),"%m%y"),".RData"))
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                      Results for Articles-with-impact manuscript                                         #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
    # Rerun modeling with no of downloads from HTML & PDF instead of only HTML downloads from the previous year.
    # After contact with the journal's webmaster we received these number for all the DOI's included in this study.
    # Therefore, updated data_features, data_model and ranger_models should be used (as loaded below)
    
    # source("scripts/rerun_downloads_analysis.R") 
  
  
    # Load correct data
    load(paste0(getwd(),"/data/RF-models_0820.RData"))
    load(paste0(getwd(),"/data/data_features_0820.RData"))

  
  # ------------------------------------------------------
  # Figures
  # ------------------------------------------------------ 
  
  
        # Figure 1. A word cloud of the most frequently used keywords
        load("data/keyword_plot_0520.RData") 
        plot_fig1_impact(stats)
        
    
        # Figure 2. Density plots of the three impact metrics
        plot_fig2_impact(data_model)

        # Display cutoffs for impact properties of top-10% and top-25% articles
        print_cutoffs(data_model)
        

        # Figure 3: Plotting predicted versus actual impact values (in percentile rank scores) of the three impact metrics
        plot_fig3_impact(ranger_model_altmetrics, 
                         ranger_model_downloads, 
                         ranger_model_citations)
        
        
        # Figure 4: Create plot with topics with highest effect size for attention scores, downloads and citations
        ES <- get_effect_sizes(data_features,filter=T) 
        plot_fig4_impact(ES)
        
        
        # Figure 5: Create plot with predictors with highest effect size for attention scores 
        plot_fig5_impact(ES)
        
        
        # Figure 6: Create plot with predictors with highest effect size for downloads
        plot_fig6_impact(ES)
        
        
        # Figure 7: Create plot with predictors with highest effect size for citations 
        plot_fig7_impact(ES)
        
  
  # ------------------------------------------------------
  # Tables
  # ------------------------------------------------------ 
  
  
        # Table 2: Model performance
        table2 <- get_results_table2(ranger_model_altmetrics, 
                                     ranger_model_downloads, 
                                     ranger_model_citations)
        
        
        # Table 3: Important features
        table3 <- get_results_table3(data_features, 
                                     ranger_model_altmetrics, 
                                     ranger_model_downloads, 
                                     ranger_model_citations) 
    
        
        # Supplemental table 1: Top-5 high-impact articles
        suppl_table1 <- get_results_suppl_table1(data_features)
        
        
        # Supplemental table 2: ES and confidence intervals of features in Fig 4-7
        suppl_table2 <- get_results_suppl_table2(data_features)
        
        
  # ------------------------------------------------------
  # In-text results
  # ------------------------------------------------------ 
  
      
        # Proportion of article types
        data_features %>% 
          group_by(pub_article_type) %>% 
          dplyr::summarise(n = n()) %>%
          mutate(freq = round(n / sum(n) *100,1))
        
        
        # Average attention scores, downloads and citations
        data_model %>% 
          select(contains("pub_metr")) %>% 
          summarise_all(~list(round(mean(.,na.rm=T),1)))
        
  
        # Display correlations for impact measures
        data_model %>% 
          select(contains("pub_metr")) %>% 
          cor(use="pairwise.complete.obs",
              method='spearman')
    
        
        # Balanced accuracy & R2 for attention score, downloads and citations of top-25% and top-10% high-impact articles
        get_performance_stats(ranger_model_altmetrics, 
                              ranger_model_downloads, 
                              ranger_model_citations) 
        
        
        # Get effect sizes for results & discussion section
        get_inline_results(data_features)
            
                
          
##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################            