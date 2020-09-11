# ------------------------------------------------------------------------------------------------------------------------ #
#                            Script for retrieving Figure 1: wordcloud with most frequently used keywords                  #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Get most frequently used keywords and plot within a wordcloud                                              #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

plot_fig1_impact <- function(stats) {  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  # Load libraries
  require(wordcloud2)
  require(htmlwidgets)
  #install.packages("webshot")    # Required for wordcloud package to save image
  #webshot::install_phantomjs()
  require(webshot)
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                  Create wordcloud based on most frequently used keywords                                 #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Prepare data for plotting (incl. remove punctuation marks and add color)
  keywrd_stats  <- stats %>% 
                   arrange(desc(freq_pct)) %>%
                   mutate(color = ifelse(key %in% head(key,25),"#6fb9d4","black")) %>% #0.2261164
                   filter(freq > 3) %>%
                   select(key,freq_pct,color) %>%
                   mutate(key      = gsub("__","-", key),
                          key      = gsub("_"," ",key))
  
  
  # Create wordcloud from frequency table
  hw <- wordcloud2(keywrd_stats, 
                   size = 1.1, 
                   color = "#04539e", 
                   backgroundColor="#e5edf5", 
                   fontWeight = "normal",
                   fontFamily = "PT Sans",
                   shape = 'circle',
                   ellipticity = .9, 
                   gridSize = 1,
                   rotateRatio = 0, 
                   minRotation = -pi/8,
                   maxRotation = pi/8,
                   hoverFunction = NULL)
  
  
  # ------------------------------------------------------
  # Save figure
  # ------------------------------------------------------   
  
      # Save figure.
      saveWidget(hw,paste0(getwd(),"/results/1c.html"),selfcontained = F)
      webshot::webshot(paste0(getwd(),"/results/1c.html"),
                       paste0(getwd(),"/results/Figure1_impact_",format(Sys.Date(),"%d%m%y"),".png"),
                       vwidth = 1200, vheight = 1200, delay =45)
      
      return(hw)
      
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################