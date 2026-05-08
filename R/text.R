#' @title Text Cleaner
#' @description Prepares text data for most analyses.
#' @param txt The string of text you would like cleaned.
#' @return A string that is the cleaned version of the one provided.
#' @export


clean_text <- function(txt) {

  if (!is.character(txt)) {
    stop("Head's up! Your input is not a string, so it is not considered text and cannot be used by this function. Check that your text has quotes around it, or that your value is of class character (<chr>)")
  }

  txt <- gsub("\\s+", " ", txt)
  txt <- trimws(txt)
  txt <- gsub("\n", " ", txt)
  txt <- gsub("[[:punct:]]", "", txt)
  txt <- gsub("http[s]?://\\S+", "", txt)
  txt <- tolower(txt)



  return(txt)
}


