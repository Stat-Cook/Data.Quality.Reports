modal.size <- function(values){
  modal.size <- table(values) %>% sort() %>% tail(1)
  modal.size / length(values)
}


extract.modal <- function(values){
  modal <- values %>% table() %>% sort() %>% tail(1)
  return(modal)
  rate <- round(modal / length(values)*100, 1)
  name <- names(modal)
  c(name, rate)
}


missing.garbage <- function(data, dataset){

  #' @export

  missing <- c(NA, "", " ", "NA", "nan", "  /  /    ",
               "NULL", "null", "Null")
  missing.rates <- data %>% is.missing(missing) %>% apply(2, mean)
  modal.sizes <- data %>% apply(2, modal.size)


  miss.1 <- names(which(missing.rates == 1))
  miss.gr0.8 <- names(which((missing.rates >= 0.8) & (missing.rates < 1)))

  tibble(`Dateset` = dataset,
         Variable = c(miss.1, miss.gr0.8),
         `% Missing` = c(
           rep("100%", length(miss.1)),
           rep(">80%", length(miss.gr0.8))
           )
  )
}



singular.garbage <- function(data, dataset){

  #' @export

  missing <- c(NA, "", " ", "NA", "nan", "  /  /    ",
               "NULL", "null", "Null")
  missing.rates <- data %>% is.missing(missing) %>% apply(2, mean)
  modal.sizes <- data %>% apply(2, modal.size)

  low.missing.high.modal <- names(
    which((missing.rates < 0.8) & (modal.sizes > 0.8))
  )

  modal.groups <- data[,low.missing.high.modal] %>% lapply(extract.modal)
  group <- sapply(modal.groups, names)
  freq <- sapply(modal.groups, function(i)  i)
  modal.perc <- tryCatch(
    round(100*freq / nrow(data), 1),
    error = function(i) c()
  )
  tibble(
    `Dateset` = dataset,
    Variable = low.missing.high.modal,
    `Modal Group` = group,
    `Modal %` = modal.perc
  )
}




