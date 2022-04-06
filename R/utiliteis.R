blank_result <- function (data) {
  #' @export
  K <- dim(data)[2]
  result <- rep(NA, K)
  names(result) <- colnames(data)
  result
}


is.missing <- function(values, missing_types=MISSING_TYPES){
  #' Vector function - identify which values are 'missing'
  #'
  #' @param values Vector of values to check for missingness
  #' @param missing_types  Vector of values classified as missing.
  #'
  #' @return Vector of which values are 'missing'
  #'
  #' @export

  sapply(values, function(i) i %in% missing_types)
}

varImp <- function(model){
  #' @export
  result <- caret::varImp(model)
  colnames(result) <- "Score"
  result
}

varImp.lda <- function(model){
  #' @importFrom caret varImp
  #' @export

  coef(model)
}

