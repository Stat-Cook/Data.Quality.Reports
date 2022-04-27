modal.report <- function(values){
  modal.size <- table(values, useNA = "ifany") %>% sort() %>% tail(1)
  c(Value = names(modal.size),
    Proportion = as.numeric(modal.size) / length(values))
}


DQ.Report <- function(con, pattern){
  #' @export
  missing <- c(NA, "", " ", "NA", "nan", "  /  /    ",
               "NULL", "null", "Null")

  tabs <- dbListTables(con)
  tabs <- tabs[grepl(pattern, tabs)]

  results <- list()

  for (i in tabs){
    data <- tbl(con, i) %>% collect()

    data.modal.size <- lapply(data, modal.report)
    data.modal.size <- t(do.call(data.frame, data.modal.size))
    data.modal.size <- as_tibble(data.modal.size, rownames="Variable") %>%
      mutate(Proportion = as.numeric(Proportion))

    data.modal.size <- data.modal.size %>% filter(Proportion > 0.8)
    data.missing <- data.modal.size  %>% filter(Value %in% missing)
    data.modal <- data.modal.size  %>% filter(!(Value %in% missing))

    last.col.string <- tail(colnames(data), 1)
    last.col <- data[[last.col.string]]
    comma.cnt <- str_count(last.col, ",")
    comma.tbl <- table(comma.cnt)

    results[[i]] <- list("Missing" = data.missing,
                         "Modal" = data.modal,
                         "Comma" = comma.tbl,
                         "Dimensions" = dim(data))
  }

  return(results)
}
