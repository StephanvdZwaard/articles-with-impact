get_altmetric_score <- function(doi) {
  
  
    # Define HTTP request based on the altmetric API and article doi.
    url  <- "https://api.altmetric.com/"
    path <-  gsub("https://doi.org/","v1/doi/",doi)
    
    # Altmetric score and stats from API.
    raw.result       <- GET(url = url, path = path)
    this.raw.content <- rawToChar(raw.result$content)
    content          <- try(fromJSON(this.raw.content), silent = TRUE)
    
    # Print results
    print(raw.result)
    
    # Delay requests
    Sys.sleep(1.1)
    
    # Only append if request was able to capture data from the API
    if (raw.result$status_code == 200) {
      alt_result <- list(list(doi,content))
      return(alt_result)
    }
  
}
