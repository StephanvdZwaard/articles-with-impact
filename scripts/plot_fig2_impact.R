# ------------------------------------------------------------------------------------------------------------------------ #
#                                    Script for retrieving Figure 2: density plots                                         #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Create density plot for attention scores, downloads and citations (figure 2)                               #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


plot_fig2_impact <- function(data) {

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
      # Load required libraries and scripts and functions
      require(tidyverse)
      source('scripts/multiplot.R')
      fmt_dcimals <- function(decimals=1) {function(x) format(x,nsmall = decimals,scientific = FALSE)}
    
      
      # Set theme for plots
      theme_plot <- theme_bw() +
                    theme(plot.title = element_text(color="black", size=18, face="bold",hjust=.12),
                          axis.line = element_line(colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          axis.title.y = element_blank(),
                          axis.title.x = element_text(size=12),
                          axis.text.x = element_text(size=12, angle=0,vjust=.5),
                          axis.text.y = element_text(size=12))
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                            Create density plot for attention scores, downloads and citations                             #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
      # Altmetric scores
      p1  <- ggplot(data %>% filter(pub_metr_altmetrics > 0),
                    aes(x=pub_metr_altmetrics)) + 
                    geom_density(bw=.15, kernel = "gaussian",fill="#6fb9d4")+
                    geom_vline(data = data %>% 
                                      filter(pub_metr_altmetrics > 0) %>%
                                      select(pub_metr_altmetrics) %>% 
                                      top_frac(.25) %>%
                                      summarise(top25 = min(pub_metr_altmetrics)),
                               aes(xintercept = top25),
                               colour="black",lty=1,size =.75) +
                    geom_text(aes(x = 7.5, y = .02, label = "top-25%"),
                              angle = 90, hjust = 0) + 
                    geom_vline(data = data %>% 
                                      filter(pub_metr_altmetrics > 0) %>%
                                      select(pub_metr_altmetrics) %>% 
                                      top_frac(.1) %>%
                                      summarise(top10 = min(pub_metr_altmetrics)),
                               aes(xintercept = top10),
                               colour="black",lty=2,size =.75) +
                    geom_text(aes(x = 23, y = .02, label = "top-10%"),
                              angle = 90, hjust = 0) + 
                    scale_y_continuous(limits = c(0,1.1), 
                                       breaks = seq(0,1,by=.25),
                                       labels = fmt_dcimals(2)) +
                    scale_x_continuous(trans = "log10",limits = c(1*10^-2, 1001),
                                       breaks = scales::trans_breaks("log10", function(x) 10^x, n=5),
                                       labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
                    annotation_logticks(sides = "b") +
                    scale_fill_manual(values=c("#6fb9d4"),labels=c("Altmetric top10")) + ##0084b4
                    labs(x="\nAltmetric Attention Score", y="Density") +
                    theme_plot
      
      
      # Downloads
      p2  <- ggplot(data,aes(x=pub_metr_downloads)) + 
                    geom_density(bw=.06, kernel = "gaussian",fill="#894c54") +
                    geom_vline(data = data %>% 
                                      select(pub_metr_downloads) %>% 
                                      top_frac(.25) %>%
                                      summarise(top25 = min(pub_metr_downloads)),
                               aes(xintercept = top25),
                               colour="black",lty=1,size =.75) +
                    geom_text(aes(x = 360, y = .02, label = "top-25%"),
                              angle = 90, hjust = 0, color = '#E5E5E5') + 
                    geom_vline(data = data %>% 
                                      select(pub_metr_downloads) %>% 
                                      top_frac(.1) %>%
                                      summarise(top10 = min(pub_metr_downloads)),
                               aes(xintercept = top10),
                               colour="black",lty=2,size =.75) +
                    geom_text(aes(x = 640, y = .02, label = "top-10%"),
                              angle = 90, hjust = 0, color = '#E5E5E5') + 
                    scale_x_continuous(trans = "log10",limits = c(1,20000),
                                       breaks = scales::trans_breaks("log10", function(x) 10^x, n=5),
                                       labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
                    scale_y_continuous(limits = c(0,1.15),
                                       breaks = seq(0,1,by=.25),
                                       labels = fmt_dcimals(2)) + 
                    annotation_logticks(sides = "b") +
                    labs(x="\nDownloads") + 
                    theme_plot
      
      
      # Citations
      p3  <- ggplot(data %>% filter(pub_metr_citations > 0),
                    aes(x=pub_metr_citations)) + 
                    geom_density(bw=.08, kernel = "gaussian",fill="#538d4c")+
                    geom_vline(data = data %>% 
                                      filter(pub_metr_citations > 0) %>%
                                      select(pub_metr_citations) %>% 
                                      top_frac(.25) %>%
                                      summarise(top25 = min(pub_metr_citations)),
                               aes(xintercept = top25),
                               colour="black",lty=1,size =.75) +
                    geom_text(aes(x = 23, y = .02, label = "top-25%"),
                              angle = 90, hjust = 0) + 
                    geom_vline(data = data %>% 
                                      filter(pub_metr_citations > 0) %>%
                                      select(pub_metr_citations) %>% 
                                      top_frac(.1) %>%
                                      summarise(top10 = min(pub_metr_citations)),
                               aes(xintercept = top10),
                               colour="black",lty=2,size =.75) +
                    geom_text(aes(x = 43, y = .02, label = "top-10%"),
                              angle = 90, hjust = 0) + 
                    scale_y_continuous(limits = c(0,1.1), 
                                       breaks = seq(0,1,by=.25),
                                       labels = fmt_dcimals(2)) +
                    scale_x_continuous(trans = "log10",limits = c(1*10^-2, 1001),
                                       breaks = scales::trans_breaks("log10", function(x) 10^x, n=5),
                                       labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
                    annotation_logticks(sides = "b") +
                    scale_fill_manual(values=c("#538d4c"),labels=c("Citations top10")) + ##0084b4
                    labs(x="\nCitations") +
                    theme_plot  
  
  
  # ------------------------------------------------------
  # Return and save figure
  # ------------------------------------------------------   
  
      # Save image as png
      png(paste0("./results/Figure2_impact_",format(Sys.Date(),"%d%m%y"),".png"), 
          bg = "transparent", width = 12, height = 4, unit = "in", pointsize = 12, res = 1200)
      
        multiplot(p1, p2, p3, cols = 3) 
      
      dev.off()
      
      # Return plot object
      return(list("altmetrics" = p1,
                  "downloads"  = p2,
                  "citations"  = p3))
  
}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################