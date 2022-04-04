DQ.template <- system.file("RMD Templates", "DQ_report.RMD", package="Data.Quality.Reports")
LD.template <- system.file("RMD Templates", "loc_Date_Report_DQ.RMD", package="Data.Quality.Reports")
Missing.template <- system.file("RMD Templates", "missing_Report_DQ.RMD", package="Data.Quality.Reports")


report.DQ <- function(data, human.readable, output_pattern="DQ reports/DQ {human.readable}"){
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
