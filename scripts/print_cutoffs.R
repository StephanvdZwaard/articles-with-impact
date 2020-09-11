print_cutoffs <- function(data_model) {
  
  require(dplyr)
  
  # --------------------------------------------------------
  # Thresholds for top-10% and top-25% high-impact articles 
  # --------------------------------------------------------   
  
  # Calculate the minimal number of attention scores, downloads and citations to be in the top-10%
  cutoff_citations_t10  <- data_model %>% filter(pub_metr_citations > 0) %>% select(pub_metr_citations)  %>% top_frac(.1) %>% summarise(top10_citations = min(pub_metr_citations))
  cutoff_downloads_t10  <- data_model %>% filter(pub_metr_downloads > 0) %>% select(pub_metr_downloads)  %>% top_frac(.1) %>% summarise(top10_downloads = min(pub_metr_downloads))
  cutoff_altmetrics_t10 <- data_model %>% filter(pub_metr_altmetrics> 0) %>% select(pub_metr_altmetrics) %>% top_frac(.1) %>% summarise(top10_altmetrics = min(pub_metr_altmetrics))

  # Calculate the minimal number of attention scores, downloads and citations to be in the top-10%
  cutoff_citations_t25  <- data_model %>% filter(pub_metr_citations > 0) %>% select(pub_metr_citations)  %>% top_frac(.25) %>% summarise(top25_citations = min(pub_metr_citations))
  cutoff_downloads_t25  <- data_model %>% filter(pub_metr_downloads > 0) %>% select(pub_metr_downloads)  %>% top_frac(.25) %>% summarise(top25_downloads = min(pub_metr_downloads))
  cutoff_altmetrics_t25 <- data_model %>% filter(pub_metr_altmetrics> 0) %>% select(pub_metr_altmetrics) %>% top_frac(.25) %>% summarise(top25_altmetrics = min(pub_metr_altmetrics))

  # ------------------------------------------------------
  # Return results 
  # ------------------------------------------------------   
  
    return(cbind(cutoff_citations_t10, cutoff_downloads_t10, cutoff_altmetrics_t10,cutoff_citations_t25, cutoff_downloads_t25, cutoff_altmetrics_t25))
  
}