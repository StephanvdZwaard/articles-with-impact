# ------------------------------------------------------------------------------------------------------------------------ #
#                                    Script for retrieving Figure 4: important topics                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Create plot with topics with highest effect size for attention scores, downloads and citations (figure 4)  #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


plot_fig4_impact <- function(ES) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Load required libraries and scripts and functions
  source("scripts/multiplot.r")
  source("scripts/get_mean_impact.R")
  source("scripts/get_thermometer_plot.R") 
  source("scripts/get_effect_sizes.R")
  require(ggpubr)
  require(dplyr)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                             Get effect sizes from the data                                               #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
      #ES <- get_effect_sizes(data_features,filter=T) 

      
# ------------------------------------------------------------------------------------------------------------------------ #
#                                 Visualize most important predictors related to the topic                                 #
# ------------------------------------------------------------------------------------------------------------------------ #
      
  
      png(paste0(getwd(),"/results/Figure4_impact_",format(Sys.Date(),"%d%m%y"),".png"), 
          bg = "transparent", width = 12, height = 3.33, unit = "in", pointsize = 1/1200, res = 1200)
          
          print({p1 <- get_thermometer_plot(ES,"pub_metr_altmetrics","topic")
                 p2 <- get_thermometer_plot(ES,"pub_metr_downloads","topic")
                 p3 <- get_thermometer_plot(ES,"pub_metr_citations","topic")
            
                 ggarrange(p1, p2, p3,
                           ncol = 3, nrow = 1,
                           align = "v")})
          
      dev.off()
  
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################