# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for collecting article characteristics and impact metrics                               #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve article impact from citations, downloads and attention scores and other  article characteristics  #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

collect_article_data <- function() {

# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #

  
  # Required libraries
    require(rvest)
    require(purrr)
    require(tidyr)
    require(stringr)
    require(data.table)
    require(ggplot2)
    require(lettercase)
    require(dplyr)
    require(anytime)
    require(httr)
    require(jsonlite)
    require(RecordLinkage)


  # Set options
    options(stringsAsFactors = FALSE)
    options(warn=-1)

    
  # Source required functions
    source("scripts/get_title.R")
    source("scripts/get_authors.R")
    source("scripts/get_doi.R")
    source("scripts/get_page.R")
    source("scripts/insert_rows.R")
    source("scripts/get_altmetric_score.R")
    source("scripts/get_dimensions_score.R")
  
    
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
      proxies <- as_data_frame(matrix(unlist(t(proxies)), byrow=T, 250, 8)) %>%
                 purrr::set_names(c("IP","port","country_short","country_long","type","google","https","time")) %>%
                 filter(str_detect(type, 'elite')) %>%
                 mutate(port = as.numeric(port))

      
  # ---------------------------------------------------------------------------------------------------------------------- #
  #                  Collect downloads & article characteristics from journal webpages using webscraping                   #
  # ---------------------------------------------------------------------------------------------------------------------- #
      
    # ------------------------------------------------------
    # Preallocation 
    # ------------------------------------------------------
  
      
      # Preallocate data frame for meta-data
        meta_data <- data.frame()
        
        
      # Preallocate data frame for the list of issues for the years 2009-2018 within the Journal of Applied Physiology
        issues    <- data.frame()
        for (volume in 106:125) {
          
          # Most volumes had 6 issues, only the volumes 112-120 had 12 issues.
          if (volume %in% seq(112,120)) {
            issues_in_volume <- 12 
          } else {
            issues_in_volume <- 6
          }
          
          add_issues <- as_data_frame(paste0(rep(paste0("/",volume,"/"),issues_in_volume),seq(1,issues_in_volume,1)))
          issues     <- rbind(issues,
                              add_issues)
        }
    
        issues <- issues[1:2,]
        
        
      # Set up progress bar to track pogress of data collection
        pb <- txtProgressBar(0, dim(issues)[1], style = 3)
    
        
    # ------------------------------------------------------
    # Get article title, authors, doi and page numbers
    # ------------------------------------------------------
        
        
      # Iterate over all issues to obtain all titles, authors, pages and doi's
        for (n in 1:nrow(issues)) {
    
          
            # Change proxy setting with randomly selected proxy for every 70 issues. 
            if (n==1 | (n/70) == as.integer(n/70)) {
                p_id <- sample(dim(proxies)[1], 1)
                set_config(use_proxy(proxies$IP[p_id],
                                     proxies$port[p_id]))
            }
  
          
          # Read webpage
            webpage   <- read_html(paste0("https://www.physiology.org/toc/jappl",issues[n,]))
        
            
          # Retrieve titles
            titles    <- get_title(webpage) 
          
            
          # Retrieve authors
            authors   <- get_authors(webpage)
       
            
          # Correct for missing authors
            
              # Missing authors (automatic): for corrigenda, retractions, rebuttals and commentaries the authors names are almost always missing. 
              # For these article types, we fill a blank row for the authors names. 
                if (nrow(titles) != nrow(authors)) {
                  id_rows <- which(str_detect(tolower(titles$title),"corrigend") | str_detect(tolower(titles$title),"retract") | str_detect(tolower(titles$title),"rebuttal from")) 
                  if (length(id_rows) != 0L) {
                  for (i in 1:length(id_rows)) {
                      authors <- insert_rows(authors,id_rows[i]-1,c(NA,NA));
                  }}
                }
                if (nrow(titles) != nrow(authors)) {
                  id_rows <- which(str_detect(tolower(titles$title),"comments on point:counterpoint") | str_detect(tolower(titles$title),"commentaries on viewpoint")) 
                  if (length(id_rows) != 0L) {
                    for (i in 1:length(id_rows)) {
                        authors <- insert_rows(authors,id_rows[i]-1,c(NA,NA)); 
                  }}
                }
            
                # Missing authors (manual). There have been some cases where authors were actually present for the article types mentioned above.
                # In that case, we remove the automatically added blank rows again.
                if (issues$value[n] == "/107/6")  {authors = authors[-c(43:44),]}                                          # retraction: authors present
                if (issues$value[n] == "/119/4")  {authors = rbind(authors[c(1:11,14),],c(NA,NA),authors[c(15),])}         # commentaries: authors present
                if (issues$value[n] == "/119/12") {authors = authors[-c(22),]}                                             # commentaries: authors present
                if (issues$value[n] == "/123/3")  {authors = rbind(authors[c(1:21,24:25),],c(NA,NA),authors[c(26:29),])}   # rebuttal: authors present
          
            
            # Retrieve page nr's
            page_nr   <- get_page(webpage)
            
            
            # Retrieve doi's
            doi       <- get_doi(webpage)
        
            
            # Combine the collected data obtained by webscraping and add the corresponding volumes and issues. Save in the data.frame metadata
            comb_data <- bind_cols(doi, titles, authors, page_nr)
            comb_data <- data.frame(comb_data,
                                    volume = gsub('/(.*)/\\w+', '\\1', issues[n,]),
                                    issue  = gsub('.*/', '',issues[n,]))
            meta_data <- bind_rows(meta_data,comb_data)
        
            
            # Track progress
            setTxtProgressBar(pb, n)
            
        }
  
  # Change settings related to warning
    options(warn=0)
  
  
  # ------------------------------------------------------
  # Get downloads and other article characteristics
  # ------------------------------------------------------
  
    
      # Retrieve article characteristcs based on DOI numbers
      doi           <- data.frame(doi=meta_data$doi)
    
    
      # Preallocate data.frame based on data of the first article
      article_data  <- get_article_info_jappl(doi[1,])
      
      
      # Set up progress bar to track pogress of data collection
      pb            <- txtProgressBar(0, dim(doi)[1], style = 3)
      
      
      for (i in 2:nrow(doi)) {
        
        
          # Set proxy for randomisation of IP with every 100th article
          if (i==1 | (i/100) == as.integer(i/100)) {
            
            p_id <- sample(dim(proxies)[1], 1)
            
            set_config(use_proxy(proxies$IP[p_id],
                                 proxies$port[p_id]))
          }
        
        
           # Retrieve article characteristics based on the DOI number (5 requests)
           article_data[i,]  <- get_article_info_jappl(doi[i,])
           
           
           # Track progress
           setTxtProgressBar(pb, i)
           
      }
  
      
# ---------------------------------------------------------------------------------------------------------------------- #
#                 Collect attention scores and citations using API connections to Altmetrics & Dimensions                #
# ---------------------------------------------------------------------------------------------------------------------- #
      
      
  # ------------------------------------------------------
  # 1. Get raw Altmetric data 
  # ------------------------------------------------------
  
      # Reset proxy for API connection
      reset_config()
      
      
      # Preallocate data.frame 
      altmetric  <- list()
          
    
      # Set up progress bar to track pogress of data collection
      pb <- txtProgressBar(0, dim(doi)[1], style = 3)
      
      
      for (i in 1:nrow(doi)) {
        
        # Retrieve Altmetric information (1 request)
        alt_result <- get_altmetric_score(doi[i,])
        altmetric  <- do.call(c, list(altmetric, alt_result))
        
        
        # Track progress
        setTxtProgressBar(pb, i)
        
      }
    
      
      # ------------------------------------------------------
      # 2. Get attention scores from raw data 
      # ------------------------------------------------------

      for (i in 1:length(altmetric)) {
        
        
        # Specific check for certain articles.
        if (!(i %in% c(1698, 1803, 1839, 1991, 2108))) {
          
          
          # ---- Mentions by user groups: -----
          
            # Retrieve mentions by types of users ('cohorts') and calculate relative to total.
            if (length(altmetric[[i]][[2]]$cohorts) != 0) {
                cohort  <- data.frame(altmetric[[i]][[2]]$cohorts) 
                total   <- cohort %>% rowSums()
                cohort  <- cohort / total
            } else {
                cohort  <- data.frame(pub = 0) 
            }
            # Set missing categories to zero (no share of total)
            missing <- setdiff(c("pub",              # Members of the public  
                                 "doc",              # Practitioners (doctors, other healthcare professionals)
                                 "sci",              # Scientists
                                 "com"               # Science communicators (journalists, bloggers, editors)
                                ), names(cohort))    # Find names of missing columns
            cohort[missing]  <- 0                    # Add them, filled with '0's
            cohort           <- cohort %>% select(sort(names(.)))
            colnames(cohort) <- paste0("altmetrics_mentions_p_",colnames(cohort)) #rename column names
          
          
          # ----- Mentioned by types of sources -----
            
            # Retrieve mentions by types of sources and calculate relative to total.
            sources <- altmetric[[i]][[2]] %>% unlist() %>% as.data.frame() %>% t() %>% data.frame() %>% 
                       select(starts_with("cited")) %>% 
                       mutate_at(vars(starts_with("cited")), 
                                 funs(as.numeric(as.character(.))))
            posts   <- sources$cited_by_posts_count                       # Retrieve total posts
            accounts<- sources$cited_by_accounts_count                    # Retrieve total unique accounts
            sources <- sources %>% select(-c(cited_by_posts_count,cited_by_accounts_count))
            sources <- sources / accounts
          
            # Set missing categories to zero (no share of total)
            missing <- setdiff(c("cited_by_fbwalls_count",                # number of Facebook accounts
                                 "cited_by_feeds_count",                  # number of blogs
                                 "cited_by_gplus_count",                  # number of Google+ users
                                 "cited_by_msm_count",                    # number of news outlets
                                 "cited_by_patents_count",                # number of patents
                                 "cited_by_peer_review_sites_count",      # number of peer review sites
                                 "cited_by_policies_count",               # number of policy sources
                                 "cited_by_qna_count",                    # number of questions, answers or comments on Stack Exchange sites (inc. Biostar)
                                 "cited_by_rdts_count",                   # number of Reddit users
                                 "cited_by_rh_count",                     # number of research highlight platforms
                                 "cited_by_tweeters_count",               # number of Twitter users
                                 "cited_by_videos_count",                 # number of YouTube channels
                                 "cited_by_weibo_count",                  # number of Sina Weibo users
                                 "cited_by_wikipedia_count"               # number of pages on Wikipedia
                                 ), names(sources))                       # Find names of missing columns
            sources[missing] <- 0                                         # Add them, filled with '0's
            sources <- sources %>% select(sort(names(.)))
            colnames(sources) <- gsub("cited_by","altmetrics_sources_p",gsub("_count","",colnames(sources))) #rename column names
          
            
          # ----- Percentage of unique posts -----
          post <- data.frame(altmetrics_posts_p_unique = accounts / posts)
          
            
          # ------ Other stats -----
            
            # Get stats regarding ranking and altmetric score.
            stats <-  altmetric[[i]][[2]] %>% unlist() %>% as.data.frame() %>% t() %>% data.frame() %>% 
                      select(journal, altmetric_id, score, ends_with(".rank"), ends_with(".count"), ends_with(".pct"), last_updated, details_url, images.medium) %>% 
                      mutate_at(vars(ends_with(".rank"), ends_with(".count"), ends_with(".pct"), altmetric_id), 
                                funs(as.integer(.))) %>%
                      mutate(score    = as.numeric(score),
                             subjects = ifelse('subjects' %in% names(content),content$subjects,"")) %>%
                      select(journal, subjects, everything())
            colnames(stats) <- c("journal","subject", "altmetric_id", "altmetric_score", "alt_rank_all", "alt_rank_journal", "alt_rank_all_sim", "alt_rank_journal_sim", "alt_total",
                                 "alt_total_journal", "alt_total_all_sim", "alt_total_journal_sim", "alt_pct_all", "alt_pct_journal", "alt_pct_all_sim", "alt_pct_journal_sim" , "alt_update",           
                                 "alt_url", "alt_img")             
            
            # Change attribute to the correct time format.
            attributes(stats$alt_update) <- attributes(anytime("01 January 2019"))

            
            
          # Combine data
          d_altmetrics <- cbind(data.frame(doi = altmetric[[i]][[1]]), stats, cohort, post, sources)
          
          
          # Merge altmetrics data for all articles
          if (i == 1) {data_altmetrics <- d_altmetrics
          } else {     data_altmetrics <- rbind(data_altmetrics,d_altmetrics)
          }
          
        }
      }

  
  # ------------------------------------------------------
  # 1. Get raw Dimensions data
  # ------------------------------------------------------
       
      # Reset proxy for API connection
      reset_config()
      
      
      # Preallocate data.frame 
      dimensions  <- list()
      
      
      # Set up progress bar to track pogress of data collection
      pb <- txtProgressBar(0, dim(doi)[1], style = 3)
      

      
      for (i in 1:dim(doi)[1]) {
        
        
        # Retrieve Altmetric information (1 request)
        dim_result  <- get_dimensions_score(doi[i,])
        dimensions  <- do.call(c, list(dimensions,dim_result))
        
        # Track progress
        setTxtProgressBar(pb, i)
        
      }

      
  # ------------------------------------------------------
  # 2. Get Dimensions citations from raw data 
  # ------------------------------------------------------
      
      for (i in 1:length(dimensions)) {

          # Combine data
          d_dimensions <- cbind(data.frame(doi = dimensions[[i]][[2]]$doi,
                                           cited = dimensions[[i]][[2]]$times_cited,
                                           recent_cited = dimensions[[i]][[2]]$recent_citations,
                                           rcr   = ifelse(!is.null(dimensions[[i]][[2]]$relative_citation_ratio),
                                                          dimensions[[i]][[2]]$relative_citation_ratio,
                                                          NA),
                                           fcr   = ifelse(!is.null(dimensions[[i]][[2]]$field_citation_ratio),
                                                         dimensions[[i]][[2]]$field_citation_ratio,
                                                         NA)))

          # Merge altmetrics data for all articles
          if (i == 1) {data_dimensions <- d_dimensions
          } else {     data_dimensions <- rbind(data_dimensions,d_dimensions)
          }
        }

      data_dimensions <- data_dimensions %>% mutate(doi = paste0("https://doi.org/",doi))

      
  # ---------------------------------------------------------------------------------------------------------------------- #
  #                                       Combine and save all collected data                                              #
  # ---------------------------------------------------------------------------------------------------------------------- #
      
      
      # Merge data collected from journal website, Altmetric API and Dimensions API
      data_raw <-  meta_data %>%
                   left_join(article_data, by="doi") %>%
                   left_join(data_altmetrics ,by="doi") %>%
                   left_join(data_dimensions, by="doi")
      
      
      # Save data to RData file.    
      save(data_raw, altmetric, dimensions, article_data, data_altmetrics, data_dimensions, 
           file="jappl_2009_2018_retrieved_on_20200101.RData")
      
      
      # Return dataframe with raw data
      return(data_raw)
    
}  
      

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################