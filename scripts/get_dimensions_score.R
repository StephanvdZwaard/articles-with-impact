get_dimensions_score <- function(doi) {
  
  # Define HTTP request based on the Dimensions API and article doi.
  url                            <- "http://metrics-api.dimensions.ai/"
  path                           <-  gsub("https://doi.org/","doi/",doi)
  
  # Altmetric score and stats from API.
  raw.result                     <- GET(url = url, path = path)
  this.raw.content               <- rawToChar(raw.result$content)
  content                        <- try(fromJSON(this.raw.content), silent = TRUE)
  
  # Print results
  print(raw.result)
  
  # Delay requests
  Sys.sleep(1.1)
  
  # Only append if request was able to capture data from the API
  if (raw.result$status_code == 200) {
    dim_result <- list(list(doi,content))
    return(dim_result)
  }
  
}
