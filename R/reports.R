DQ.template <- system.file("RMD Templates", "DQ_report.RMD", package="Data.Quality.Reports")
LD.template <- system.file("RMD Templates", "loc_Date_Report_DQ.RMD", package="Data.Quality.Reports")
Missing.template <- system.file("RMD Templates", "missing_Report_DQ.RMD", package="Data.Quality.Reports")
Comma.template <- system.file("RMD Templates", "Comma_report.RMD", package="Data.Quality.Reports")


report.DQ <- function(data, human.readable, output_pattern="DQ reports/DQ {human.readable}"){
  #' Generate a data quality report for a data set.
  #'
  #' @param data The data set to be reported ob
  #' @param human.readable A string representative of the data set to be used
  #' in the report.
  #' @param output_pattern A string to determine export location.  NB: string is parsed via
  #' `glue` hence code chunks can be included.
  #'
  #' @export
  temp <- tempfile()
  saveRDS(data, temp)

  render.params <- list(
    "human.readable" = human.readable,
    "data.location" = temp
  )
  output.string <- file.path(
    getwd(),
    glue(output_pattern)
  )

  rmarkdown::render(DQ.template, params=render.params,
                    output_file=output.string)
}


report.loc_date <- function(data, human.readable, location.var, date.var,
                            output_pattern="Location Date reports/LD {human.readable}"){
  #' Generate a missingness report reflecting how dependent missingess is on given
  #' location (categorical) and date time (numeric) variables.
  #'
  #' @inheritParams  report.DQ
  #' @param location.var A variable of `data` that represents a categorical location.
  #' @param date.var A variable of `data` that represents a numeric date time.
  #'
  #' @export

  temp <- tempfile()
  saveRDS(data, temp)

  render.params <- list(
    "location_variable" =  location.var,
    "date_variable" = date.var,
    path = temp
  )#

  output.string <- file.path(
    getwd(),
    glue(output_pattern)
  )

  set.seed(1337)
  rmarkdown::render(LD.template,
                    output_file = output.string,
                    params  = render.params)
}


report.missing <- function(data, human.readable,
                           output_pattern = "missing_Reports/Missing {human.readable}.html" ){
  #' Generate a general report of missingness for a data set.
  #'
  #' @inheritParams report.DQ
  #'
  #' @export
  temp <- tempfile()
  saveRDS(data, temp)

  output.string <- file.path(
    getwd(),
    glue(output_pattern)
  )

  render.params <- list(path = temp)

  rmarkdown::render(Missing.template,
                    params=render.params,
                    output_file = output.string)
}
