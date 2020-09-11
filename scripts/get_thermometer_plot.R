# ------------------------------------------------------------------------------------------------------------------------ #
#                                Script for creating a thermometer plot for impact (fig 4-7)                               #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Create thermometer plot required for figures 4-7                                                           #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


get_thermometer_plot <- function(data,impact_metric,subject_name="") {  
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # Load libraries
  require(stringr)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Set title and color code based on impact metric
    if (impact_metric == "pub_metr_altmetrics") {
      color_code = "#5bafce"
      plot_title <- "Attention scores"
      
    } else if (impact_metric == "pub_metr_downloads") {
      color_code = "#894c54"
      plot_title <- str_to_title(gsub("pub_metr_","",impact_metric))
      
    } else if (impact_metric == "pub_metr_citations") {
      color_code = "#538d4c"
      plot_title <- str_to_title(gsub("pub_metr_","",impact_metric))
      
    }
  
  # Set theme for plots
    theme_plot <- theme_bw() +
                  theme(legend.position = "none",
                        plot.title = element_text(color="black", size=14, face="bold",hjust=.12),
                        axis.line = element_line(colour = "black"),
                        #panel.background=element_rect(fill="white"),
                       # panel.border =element_rect(colour="black",size=.3),
                        panel.border = element_blank(),
                        panel.grid.major =  element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.title.y = element_blank(),
                        axis.title.x = element_text(size=9),
                        axis.text.x = element_text(size=9, angle=0,vjust=.5),
                        axis.text.y = element_text(size=9))

    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                 For figure 4, create plot with top-10 most important topics                              #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    if (subject_name == "topic") {
      
       # Some minor preprocessing (select top-10 based on topics with the highest effect size;
       # also remove punctuations and set transparency for attention score plot)
        data_part <- data %>% filter(rowname == impact_metric) %>% group_by(subject) %>% 
                     arrange(subject,desc(eff_size))  %>%
                     dplyr::mutate(feature = gsub("key_t50_","",feature),
                                   feature = gsub("key_","",feature)) %>% slice(1:10) %>% filter(subject==subject_name) %>%
                     mutate(alpha_impact = ifelse(str_detect(rowname,"altmetric"),1,.8)) %>%
                     dplyr::mutate(feature = ifelse(feature=="mri","MRI",feature),
                                   feature = ifelse(feature=="vo2_kinetics","vo2kinetics",feature),
                                   feature = gsub("_"," ",feature))
      
    p1 <- ggplot() +
      
      # First layer of thermometer plot (outer bar)
      geom_bar(width = .6,
               stat="identity",
               data = data_part,
               mapping = aes(x=reorder(feature,mean_feature_yes_or_high),
                             y=mean_feature_no_or_low),
               na.rm = TRUE,
               fill = "#e1e1e1") + #"#eeeeee"
      
      # Second layer of thermometer plot (inner bar)
      geom_bar(width = .4,
               stat="identity",
               data = data_part,
               mapping = aes(x=reorder(feature,mean_feature_yes_or_high),
                             y=mean_feature_yes_or_high,
                             alpha = alpha_impact),
               na.rm = TRUE,
               fill = color_code) +
      
      # Add annotation with % higher / lower impact
      geom_text(stat    = "identity",
                data    = data_part,
                mapping = aes(x = reorder(feature,mean_feature_no_or_low), 
                              y = mean_feature_yes_or_high + .0075, 
                              label=paste0(ifelse(diff<0,"-","+"),round(perc_diff,0),"%")),
                color= color_code, hjust = 0, size = 2.5) +
      
      # Add annotation with effect sizes
      geom_text(stat    = "identity",
                data    = data_part,
                mapping = aes(x = reorder(feature,mean_feature_yes_or_high), 
                              y = .04, 
                              label=paste0(ifelse(eff_magn == "negligible","",substr(eff_magn,1,1)))),
                color= "#f9f9f9", hjust = 0.5, vjust = 0.5, size = 2.5) +
      
      ggtitle(plot_title) +
      
      #flip the graph to horizontal
      coord_flip() +
      
      scale_y_continuous(limits = c(0,1.0), labels = fmt_dcimals(2)) +
      
      labs(y="percentile") +
      
      theme_plot
   
# ------------------------------------------------------------------------------------------------------------------------ #
#         For figure 5-7, create plot with top-3 most important predictors for the other article characteristics           #
# ------------------------------------------------------------------------------------------------------------------------ #
    
  } else { 
    
    # Some minor preprocessing (related to naming and punctuations; also set transparency for attention score plot)
    data_part <- data %>% filter(rowname == impact_metric) %>%
      arrange(subject,desc(mean_feature_yes_or_high))  %>%
      filter(subject!="topic") %>%
      dplyr::mutate(alpha_impact = ifelse(str_detect(rowname,"altmetric"),1,.8),
             feature = gsub("altmetrics_","",feature),
             subject = factor(subject,levels=c("title","abstract","collaboration","publication","engagement")),
             feature = gsub("title_|abstr_|pub_|sources_","",feature),
             feature = gsub("collab_","affiliation to ",feature),
             feature = gsub("sentence_n","number of sentences",feature),
             feature = gsub("authors_n","number of authors",feature),
             feature = gsub("inprint","in print",feature),
             feature = gsub("uk","the uk",feature),
             feature = gsub("qmark","question mark",feature),
             feature = gsub("edit_corr","editorial | corr",feature),
             feature = gsub("^n_","number of ",feature)) %>%
      dplyr::mutate(feature = ifelse(feature == "x","uncategorized UPOS (%)",feature),
             feature = ifelse(feature == "perc_top25_nouns","top25 nouns (%)",feature),
             feature = ifelse(feature == "propn","proper nouns (%)",feature),
             feature = gsub("_"," ",feature),
             feature = gsub("pvalue","P-values",feature),
             feature = ifelse(feature == "mention goal","mention 'goal'",feature),
             feature = ifelse(feature == "mention objctve","mention 'objective'",feature),
             feature = ifelse(feature == "p msm","via news outlets (%)",feature),
             feature = ifelse(feature == "p feeds","via blogs (%)",feature),
             feature = ifelse(feature == "p videos","via youtube (%)",feature),
             feature = ifelse(feature == "p rdts","via reddit (%)",feature),
             feature = ifelse(feature == "p patents","via patents (%)",feature),
             feature = ifelse(feature == "p policies","via policies (%)",feature),
             feature = gsub("article type","article type:",feature),
             feature = gsub("p sci","by scientists (%)",feature))
    
    p1 <- ggplot() +
      
      # First layer of thermometer plot (outer bar)
      geom_bar(width = .6,
               stat="identity",
               data = data_part,
               mapping = aes(x=reorder(feature,abs(eff_size)),
                             y=mean_feature_no_or_low),
               na.rm = TRUE,
               fill = "#e1e1e1") + #"#eeeeee"
      
      # Second layer of thermometer plot (inner bar)
      geom_bar(width = .4,
               stat="identity",
               data = data_part,
               mapping = aes(x=reorder(feature,abs(eff_size)),
                             y=mean_feature_yes_or_high,
                             alpha = alpha_impact),
               na.rm = TRUE,
               fill = color_code) +
      
      # Add annotation with % higher / lower impact
      geom_text(stat    = "identity",
                data    = data_part,
                mapping = aes(x = reorder(feature,abs(eff_size)), 
                              y = mean_feature_yes_or_high + .0075, 
                              label=paste0(ifelse(diff<0,"-","+"),round(perc_diff,0),"%")),
                color= color_code, hjust = 0, size = 2.5) +
      
      # Add annotation with effect sizes
      geom_text(stat    = "identity",
                data    = data_part,
                mapping = aes(x = reorder(feature,abs(eff_size)), 
                              y = .04, 
                              label=paste0(ifelse(eff_magn == "negligible","",substr(eff_magn,1,1)))),
                color= "#f9f9f9", hjust = 0.5, vjust = 0.5, size = 2.5) +
      
      ggtitle(plot_title) +
      
      #flip the graph to horizontal
      coord_flip() +
      
      scale_y_continuous(limits = c(0,1.0), labels = fmt_dcimals(2)) +
      
      labs(y="percentile") +
      
      facet_grid(subject ~ ., scales = "free", switch = "both") +
      
      theme_plot
    
  }
    
    # ------------------------------------------------------
    # Return ggplot object
    # ------------------------------------------------------   
    
    return(p1)

    
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################