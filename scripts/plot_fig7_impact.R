# ------------------------------------------------------------------------------------------------------------------------ #
#                               Script for retrieving Figure 7: important predictors for citations                         #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Create plot with predictors with highest effect size for citations (figure 7)                              #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


plot_fig7_impact <- function(ES) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #


      # Load required libraries and scripts and functions
      fmt_dcimals <- function(decimals=1) {function(x) format(x,nsmall = decimals,scientific = FALSE)}
      source("scripts/multiplot.r")
      source("scripts/get_mean_impact.R")
      source("scripts/get_thermometer_plot.R") 
      source("scripts/get_effect_sizes.R")
      require(ggpubr)


# ------------------------------------------------------------------------------------------------------------------------ #
#                                             Get effect sizes from the data                                               #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
      #ES <- get_effect_sizes(data_features,filter=T) 
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                 Visualize most important predictors related to citations                                 #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
      # -------  Citations  ------- 
      
      png(paste0("./results/Figure7_impact_",format(Sys.Date(),"%d%m%y"),".png"),  
          bg = "transparent", width = 5, height = 6, unit = "in", pointsize =  1/1200, res = 1200)
      
      print({get_thermometer_plot(ES,"pub_metr_citations")})

      dev.off()
      
  
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################