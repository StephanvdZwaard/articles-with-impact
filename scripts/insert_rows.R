insert_rows <- function(X,index_after,vector_to_insert){
  
  # Insert rows is a function that is used to correct for missing authors (see script articles_collect.R)
  
  stopifnot(length(vector_to_insert) == ncol(X)); # to check valid row to be inserted
  
  if (index_after != 0) {
    
    if (dim(X)[1] > index_after) {
      X <- rbind(X[1:index_after,], vector_to_insert, X[(index_after+1):nrow(X),]);
    } else if (dim(X)[1] == index_after) {
      X <- rbind(X[1:index_after,], vector_to_insert);
    } else if (dim(X)[1] < index_after) {
      X <- rbind(X[1:dim(X)[1],], vector_to_insert);
    }
    
  } else {
    
    if (dim(X)[1] != index_after) {
      X <- rbind(vector_to_insert, X[(1):nrow(X),]);
    } else { 
      X <- rbind(vector_to_insert);
    }
    
  }
  
  row.names(X) <- 1:nrow(X);
  
  return (X);
}  
