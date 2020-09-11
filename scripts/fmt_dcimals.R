fmt_dcimals <- function(decimals=1) {
  
  function(x) format(x,nsmall = decimals,scientific = FALSE)
  
}