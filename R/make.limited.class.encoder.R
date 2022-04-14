
choose.categories <- function(values, cutoff = 0.8, min.use = 1, maxitems=NA){
  #' Select a subset of a categorical variable cardinality.
  #'
  #' @param values Vector of values to reduce cardinality of
  #' @param cutoff Total percentage of variable to capture
  #' @param min.use Minimum use of each choice to be included
  #' @param maxitems [optional] Maximum cardinality to reach
  #'
  #' @export
  v <- values %>% table(useNA = "ifany")
  total <- sum(v)
  sorted <- v %>% sort()
  if (is.na(maxitems)){
    top.percentage <- sorted  %>% cumsum() %>% .[. >= (1-cutoff)*total] %>% names()
    non.unique <- v %>% .[. > min.use] %>% names()
    overlap <- intersect(top.percentage, non.unique)
    return(overlap)
  }

  rev(sorted)[1:maxitems] %>% names()
}

make.limited.class.encoder <- function(trainingData){
  #' Produce a function to reduce carnality of feature vectors in a data frame.
  #' Learns a pattern for each column by name.
  #'
  #' @param trainingData A data frame to learn class encoders from
  #'
  #' @export
  common.classes <- list()
  temp <- trainingData
  for (key in colnames(trainingData)){
    values <- trainingData[[key]]
    common.classes[[key]] <- choose.categories(values,
                                               cutoff = 0.8, min.use = 1)
  }

  f <- function(data) {
    for (key in names(data)) {
      if(key %in% names(common.classes)) {
        values <- data[[key]]
        classes <- common.classes[[key]]
        names(classes) <- classes
        data[[key]] <- classes[values] %>% replace_na("Other")
      }
    }
    data
  }

  return(f)
}
