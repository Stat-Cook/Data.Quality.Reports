# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

DQ.template <- system.file("RMD Templates", "DQ_report.RMD", package="Data.Quality.Reports")
LD.template <- system.file("RMD Templates", "loc_Date_Report_DQ.RMD", package="Data.Quality.Reports")
Missing.template <- system.file("RMD Templates", "missing_Report_DQ.RMD", package="Data.Quality.Reports")

# data <- ChickWeight
# human.readable <- "Chicken Weight"
# output_pattern <- "DQ {human.readable}"

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

blank_result <- function (data) {
  #' @export
  K <- dim(data)[2]
  result <- rep(NA, K)
  names(result) <- colnames(data)
  result
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
