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

  comma.cnt <- stringr::str_count(last.col, ",")
  comma.examples <- last.col[grepl(",", last.col)]

  result <- list(
    "Count" = table(comma.cnt),
    "Examples" =table(comma.examples)
  )
  class(result) <- "CommaSummary"
  result
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

zero.prop <- function(comma.summary){

  cnt <- comma.summary[["Count"]]
  if(sum(cnt) == 0){
    return(NA)
  }

  val <- tryCatch(
    cnt[["0"]],
    error = function(e) {
      0
    }
  )

  1 - val / sum(cnt)

}

select_commas <- function(report, limit=0){
  #' @export
  comma.prop <- sapply(report, zero.prop)

  which(comma.prop > 0) %>% names()
}

render.Comma <- function(data, human.readable){
  #' @export
  temp <- tempfile()
  saveRDS(data, temp)

  render.params <- list(
    data.location = temp,
    human.readable = human.readable
  )

  output.string <- file.path(
    getwd(),
    glue(glue("{human.readable} Comma DQ"))
  )

  rmarkdown::render(Comma.template,
                    params=render.params,
                    output_file = output.string)
}

# a <- sample(1:2, 100, T)
# b <- sample(1:2, 100, T)
#
# l <- list(A = result)
#
# render.Comma(l, "Test")
