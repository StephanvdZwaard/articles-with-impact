# ------------------------------------------------------------------------------------------------------------------------ #
#                                    Script for retrieving Figure 3: modeling results                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Create prediction vs actual plot for attention scores, downloads and citations (figure 3)                  #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


  plot_fig3_impact <- function(model_altmetrics, model_downloads, model_citations) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
    
  # Load required libraries and scripts and functions
  require(tidyverse)
  source('scripts/multiplot.R')
  fmt_dcimals <- function(decimals=1) {function(x) format(x,nsmall = decimals,scientific = FALSE)}

  # Set theme for plots
  theme_plot <- theme_bw() +
                theme(legend.position = "none",
                      plot.title = element_text(color="black", size=14, face="bold",hjust=.12),
                      axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.title.y = element_text(size=14),
                      axis.title.x = element_text(size=14),
                      axis.text.x = element_text(size=12, angle=0,vjust=.5),
                      axis.text.y = element_text(size=12))
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                         Create predicted vs actual impact score plot based on modeling results                           #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # Altmetric scores
  data <- evaluate_model_performance(model_altmetrics$`RF-model`, 
                                     model_altmetrics$`test-set`) 
  p1   <- ggplot(data,
                 aes(x=predicted, y=actual)) + 
                  geom_point(shape = 16, size = 1.5, alpha = .5, color = "black") +
                  geom_abline(aes(intercept=0, slope=1)) +
                  stat_smooth(geom = "smooth", color="#6fb9d4", fill ="#6fb9d4",  
                              method = "lm",   se = T, level = 0.99, linetype = 1, size = 1.2) +
                  annotate("text", x = 1.01, y = .1, color="#6fb9d4", hjust = 1, label = paste0("R^2 == ",round(ranger_model_altmetrics$results$lm$r.squared,2)), parse = T) +
                  scale_y_continuous(limits = c(0,1.02), labels = fmt_dcimals(2)) +
                  scale_x_continuous(limits = c(0,1.02), labels = fmt_dcimals(2)) +
                  scale_color_manual(values="#6fb9d4") + ##0084b4
                  ggtitle("Attention scores") +
                  labs(x="\nPredicted values", y="Actual values\n") +
                  theme_plot
  
  # Downloads
  data <- evaluate_model_performance(model_downloads$`RF-model`, 
                                     model_downloads$`test-set`) 
  p2   <- ggplot(data,
                 aes(x=predicted, y=actual)) + 
                geom_point(shape = 16, size = 1.5, alpha = .35, color = "black") +
                geom_abline(aes(intercept=0, slope=1)) +
                stat_smooth(geom = "smooth", color="#894c54", fill ="#894c54",  ##5894a9
                            method = "lm",   se = T, level = 0.99, linetype = 1, size = 1.2) +
                annotate("text", x = 1.01, y = .1, color="#894c54", hjust = 1, label = paste0("R^2 == ",round(ranger_model_downloads$results$lm$r.squared,2)), parse = T) +
                scale_y_continuous(limits = c(0,1.02), labels = fmt_dcimals(2)) +
                scale_x_continuous(limits = c(0,1.02), labels = fmt_dcimals(2)) +
                scale_color_manual(values="#894c54") + ##0084b4
                ggtitle("Downloads") +
                labs(x="\nPredicted values", y="") +
                theme_plot
  
  # Citaties
  data <- evaluate_model_performance(model_citations$`RF-model`, 
                                     model_citations$`test-set`) 
  p3   <- ggplot(data,
                 aes(x=predicted, y=actual)) + 
                # geom_rect(aes(xmin = .75, xmax = 1, ymin = .75, ymax = 1), fill="#e1e1e1", alpha = .25, linetype = 1) +
                # geom_rect(aes(xmin = .90, xmax = 1, ymin = .90, ymax = 1), fill="#b4b4b4", alpha = .25, linetype = 1) +
                geom_point(shape = 16, size = 1.5, alpha = .35, color = "black") +
                geom_abline(aes(intercept=0, slope=1)) +
                stat_smooth(geom = "smooth", color="#538d4c", fill ="#538d4c",  ##5894a9
                            method = "lm",   se = T, level = 0.99, linetype = 1, size = 1.2) +
                annotate("text", x = 1.01, y = .1, color="#538d4c", hjust = 1, label = paste0("R^2 == ",round(ranger_model_citations$results$lm$r.squared,2)), parse = T) +
                scale_y_continuous(limits = c(0,1.02), labels = fmt_dcimals(2)) +
                scale_x_continuous(limits = c(0,1.02), labels = fmt_dcimals(2)) +
                scale_color_manual(values="#538d4c") + ##0084b4
                ggtitle("Citations") +
                labs(x="\nPredicted values", y="") +
                theme_plot
  
  
  # ------------------------------------------------------
  # Return and save figure
  # ------------------------------------------------------   
  
  
      # Save image as png
      png(paste0("./results/Figure3_impact",format(Sys.Date(),"%d%m%y"),".png"), 
          bg = "transparent", width = 12, height = 4, unit = "in", pointsize = 12, res = 1200)
      
        multiplot(p1, p2, p3, cols=3) 
        
      dev.off()
      
      
      # Return plot object
      return(list("altmetrics" = p1,
                  "downloads"  = p2,
                  "citations"  = p3))
  
  }
  
  
  ##############################################################################################################################
  #                                                   End of syntax                                                            #
  ##############################################################################################################################