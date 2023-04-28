#' Text Visualizer Function
#'
#' This function allows you to visualize the frequency of words within your input text.
#' @param mydata A string input or string vector that is used to find the most common words within.
#' @keywords visualization
#' @export
#' @examples
#' visual_function()

visual_function <- function(mydata){
  mydata <- tolower(mydata)
  mydata <- bracketX(mydata)
  mydata <- replace_abbreviation(mydata)
  mydata <- replace_contraction(mydata)
  mydata <- replace_symbol(mydata)
  mydata <- removePunctuation(mydata)
  mydata <- removeWords(mydata, stopwords("en"))
  mydata <- stripWhitespace(mydata)
  
  source <- VectorSource(mydata)
  dataCorp <- VCorpus(source)
  dataTDM <- TermDocumentMatrix(dataCorp)
  dataMatrix <- as.matrix(dataTDM)
  
  term_freq <- dataMatrix[order(dataMatrix[,1],decreasing = TRUE),]
  barplot(term_freq[1:10], col = "lavender", las = 2,main = "Most Common Words Found", ylab = "# of Times Found", names.arg = rownames(term_freq[1:10]))
}