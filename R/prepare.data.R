prepare.data <- function(feature, target, extract_name=extract_name){
  #' This is a generic method that dispatches based on the first argument.
  #'
  #' @param feature Vector of data
  #' @param target Vector of data where some values are missing
  #' @param extract_name Function for naming discretized features
  #'
  UseMethod("prepare.data", feature)
}


prepare.data.character <- function(feature, target, extract_name=extract_name){
  #' Method for `prepare.data` if feature is categorical (character vector)
  #'
  #' @inheritParams prepare.data
  #'
  dp <- createDataPartition(target, p = .8,
                            list = FALSE,
                            times = 1)

  feature <- replace_na(feature, "Unknown")
  categories <- choose.categories(feature[dp], maxitems = 30)
  names(categories) <- categories
  feature <- categories[feature] %>% replace_na("Other")
  naming_vector <- unique(feature)
  names(naming_vector) <- naming_vector

  list(xTrain =feature[dp], yTrain = target[dp],
       xTest = feature[-dp], yTest = target[-dp],
       naming_vector = naming_vector)
}

prepare.data.POSIXct <- function(feature, target, extract_name=extract_name){

  #' Method for `prepare.data` if feature is a date/time (POSIXct vector)
  #'
  #' @inheritParams prepare.data
  #'
  feature.numeric <- feature %>% as.numeric()

  prepare.data(feature.numeric, target, extract_name.POSIXct)
}

year_month <- function(value){
  paste(month(value), year(value), sep="/")
}


extract_name <- function(data){
  #' Convert numeric variables aggregated by `Groups` into a string representation.
  #'
  #' @param data The data set to be converted.
  #' Should have columns `Value` and `Group`, where `Value` is numeric and `Group` is categorical.
  #'
  #'
  data %>% group_by(Group) %>% summarise(start = min(Value), end=max(Value)) %>%
    mutate(name = paste(start, "-", end))
}


extract_name.POSIXct <- function(data){
  #' Convert date time variables aggregated by `Groups` into a string representation.
  #'
  #' @param data The data set to be converted.
  #' Should have columns `Value` and `Group`, where `Value` is a date time and `Group` is categorical.
  #'
  #'
  data %>% group_by(Group) %>%
    summarise(
      start = as.POSIXct(min(Value), origin='1970-01-01'),
      end=as.POSIXct(max(Value), origin='1970-01-01')
    ) %>%
    mutate(name = paste(year_month(start), "-", year_month(end)))
}


prepare.data.default <- function(feature, target, extract_name=extract_name){
  #' Method for `prepare.data` for any other data types (eg numeric vector)
  #'
  #' @inheritParams prepare.data
  #'
  dp <- createDataPartition(target, p = .8,
                            list = FALSE,
                            times = 1)

  xTrain <- feature[dp]
  xTest <- feature[-dp]

  anyNA <- any(is.na(xTrain))

  disc <- recipes::discretize(xTrain, na.rm=T, cuts =20, keep_na=anyNA)

  ref <- data.frame(Group = predict(disc, xTrain), Value = xTrain)  %>%
    extract_name()

  naming_vector <- ref$name
  names(naming_vector) <- ref$Group

  list(xTrain = predict(disc, xTrain), yTrain = target[dp],
       xTest = predict(disc, xTest), yTest = target[-dp],
       naming_vector = naming_vector)

}


