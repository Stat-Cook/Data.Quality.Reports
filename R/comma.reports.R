check.last <- function(con, sql.tbl){
  #' Analyze the presences of commas (',') in the last column of a SQL database.
  #'
  #' @param con A `DBIConnection` object
  #' @param sql.tbl The reference string of the table to be querried.
  #'
  #' @import dplyr

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
  #' Check all available sql tables that match a string pattern.
  #'
  #' @inheritParams check.last
  #' @param pattern A string for matching to (grepl)
  #'
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
  #' Calculate what proportion of responses had zero commas.
  #'
  #' @param comma.summary A list produced by `check.last`
  #'
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
  #' Function to filter a list of `CommaSummary` objects for those where
  #' some commas are used.
  #'
  #' @param report list of `CommaSummary` objects
  #' @param limit [optional] Count of minimum number of commas allowed
  #'
  #' @export
  comma.prop <- sapply(report, zero.prop)

  which(comma.prop > limit) %>% names()
}

render.Comma <- function(data, human.readable){
  #' Render the RMD report of comma cases.
  #'
  #' @param data A list of `CommaSummary` objects
  #' @param human.readable A string to represent the data.
  #'
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
