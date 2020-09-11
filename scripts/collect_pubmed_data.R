# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for collecting author affiliations from PubMed                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve author affiliations from PubMed using webscraping                                                 #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


collect_pubmed_data <- function(data_raw) {

  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  require(rvest)
  require(httr)
  require(stringi)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                              Data collection using proxies                                               #
# ------------------------------------------------------------------------------------------------------------------------ #
  

  # ---------------------------------------------------------------------------------------------------------------------- #
  #                                                  Set-up proxies                                                        #
  # ---------------------------------------------------------------------------------------------------------------------- #
  
  
      # Get webpage for proxies list to approach the journal website from multiple IP addresses
      webpage <- try(read_html("https://free-proxy-list.net"),silent=T)
      
      
      # Retrieve proxy list from the webpage
      proxies <- webpage %>% 
        html_nodes('td') %>%
        html_text() %>%
        as_data_frame()
      
      
      # Restructure data frame and select elite proxy connections only
      proxies <- as_data_frame(matrix(unlist(t(proxies[1:1200,])), byrow=T, 150,8)) %>%
        purrr::set_names(c("IP","port","country_short","country_long","type","google","https","time")) %>%
        filter(str_detect(type, 'elite')) %>%
        mutate(port = as.numeric(port))


  # ---------------------------------------------------------------------------------------------------------------------- #
  #                         Collect author affiliation info from PubMed webpages using webscraping                         #
  # ---------------------------------------------------------------------------------------------------------------------- #
  
    # ------------------------------------------------------
    # Preallocation 
    # ------------------------------------------------------
  
        # Get PubMed pages based on DOI number
        doi <- data_raw %>% select(doi) 
    
        # Set up progress bar to track pogress of data collection
        pb  <- txtProgressBar(0, dim(doi)[1], style = 3)

      
    # ------------------------------------------------------
    # Get author affiliation info
    # ------------------------------------------------------
  
        for (i in 1:dim(doi)[1]) {

            # Set proxy for randomisation of IP
            if (i==1 | (i/100) == as.integer(i/100)) {
              p_id <- sample(dim(proxies)[1], 1)
              set_config(use_proxy(proxies$IP[p_id],proxies$port[p_id]))
            }
    
          
            # Retrieve article information (2 requests)
            site    <- gsub("https://doi.org/10.1152/","https://www.ncbi.nlm.nih.gov/pubmed/?term=10.1152%2F",doi$doi[i])
            webpage <- try(read_html(site),silent=T)
    
            # Check if webpage is found
            error   <- webpage %>%
                       html_nodes('.icon') %>%
                       html_text() %>%
                       as_data_frame()
    
            if (any(str_detect(error$value,"not found"))) {
    
                affiliations <- data.frame(affiliations = NA)
                citations    <- data.frame(citation = NA)
    
            } else {
    
              
              # Delay requests
                Sys.sleep(.2)
      
              
              # Retrieve author affiliations
                affiliations <- webpage %>%
                                #! Unfortunately, the website structure of PubMed has changed since the beginning of May.
                                html_nodes(ifelse(Sys.Date() < as.Date("2020-05-01"),'dd','.affiliations')) %>%  
                                html_text() %>%
                                as_data_frame() %>%
                                dplyr::rename(affiliation = value)
                
                
              # Delay requests
                Sys.sleep(.2)
      
                
              # Retrieve citation data to obtain date the article appeared 'in press'.
                citation     <- webpage %>%
                                html_nodes('.cit') %>%
                                html_text() %>%
                                as_data_frame() %>%
                                dplyr::rename(citation = value)

              }
      
            
              # Combine all collected data  
              if (i==1) {
                
                pubmed     <- data.frame(doi          = as.character(doi$doi[i]),
                                         affiliations = I(list(affiliations)),
                                         citations    = ifelse(dim(citation)[1]==0, NA, citation$citation))
                
              } else {
                
                pubmed_new <- data.frame(doi          = as.character(doi$doi[i]),
                                         affiliations = I(list(affiliations)),
                                         citations    = ifelse(dim(citation)[1]==0, NA, citation$citation))
                pubmed     <- rbind(pubmed,pubmed_new)
                
              }
      
              # Track progress
              setTxtProgressBar(pb, i)
        
        }
  
        
  # ------------------------------------------------------
  # Save PubMed data
  # ------------------------------------------------------   
     
        
    #Save raw PubMed data
    #save(pubmed,file = "pubmed.RData")

    # Merge with other article data and save
    data_pubmed <- left_join(data_raw, pubmed, by=c("doi"="doi"))
    #save(data_pubmed,file = "data_pubmed.RData")
  

    return(data_pubmed)
  
  
}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################
