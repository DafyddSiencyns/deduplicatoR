#' Deduplicates repeat specimens from a patient sent within specified time
#' @export
#' @param df dataframe
#' @param x numeric variable
dedup.function <- function(df,x){
  df <- arrange(df, HPRN, HCDATE)
  nrow.start=nrow(df)
  dup.df <- vector(mode = "numeric", length = nrow.start)
  nrow.end=1
  while(nrow.end<nrow.start){
    nrow.start=nrow(df)
    i <- 2
    while(i<1+nrow(df)){
      if(df$HPRN[[i]]!=df$HPRN[[i-1]]){
        dup.df[[i]]=1
      }
      else if((df$HPRN[[i]]==df$HPRN[[i-1]])&(as.numeric(df$HCDATE[[i]]-df$HCDATE[[i-1]])>x)){
        dup.df[[i]]=2
      }
      else{df <- df[-c(i),]
      break
      }
      i <- i+1
    }
    nrow.end=nrow(df)
  }
  return(df)
}

