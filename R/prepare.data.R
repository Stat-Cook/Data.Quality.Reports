prepare.data <- function(feature, target, extract_name=extract_name){
  UseMethod("prepare.data", feature)
}


prepare.data.character <- function(feature, target, extract_name=extract_name){
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
  feature.numeric <- feature %>% as.numeric()

  prepare.data(feature.numeric, target, extract_name.POSIXct)
}

extract_name <- function(data){
  data %>% group_by(Group) %>% summarise(start = min(Value), end=max(Value)) %>%
    mutate(name = paste(start, "-", end))
}

year_month <- function(value){
  paste(month(value), year(value), sep="/")
}

extract_name.POSIXct <- function(data){
  data %>% group_by(Group) %>%
    summarise(
      start = as.POSIXct(min(Value), origin='1970-01-01'),
      end=as.POSIXct(max(Value), origin='1970-01-01')
    ) %>%
    mutate(name = paste(year_month(start), "-", year_month(end)))
}


prepare.data.default <- function(feature, target, extract_name=extract_name){
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


