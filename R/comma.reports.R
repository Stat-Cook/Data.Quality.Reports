check.last <- function(con, sql.tbl, human.readable=NA){
  #' @import dplyr

  if (is.na(human.readable)){
    human.readable <- sql.tbl
  }

  cols <- tbl(con, sql.tbl) %>%
    colnames()

  last <- tail(cols, 1)

  sql.t <- tbl(con, sql.tbl)
  last.col <- collect(select(sql.t, last))

  last.col <- last.col[[last]]

  comma.cnt <- stringr::str_count(last.col, ",") %>% table()

  comma.cnt
}

report <- function(con, pattern){
  #' @export
  #'
  tab <- dbListTables(con)

  sel.tables <- tab[grepl(pattern, tab)]

  commas <- lapply(
    sel.tables,
    function(i) check.last(con, i)
  )

  names(commas) <- sel.tables
  commas
}

select.commas <- function(report, limit=0){
  #' @export
  comma.prop <- sapply(report, function(i) 1 - i[["0"]] / sum(i))

  which(comma.prop > 0) %>% names()
}

