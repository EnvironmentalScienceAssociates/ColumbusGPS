#' Prep lat/lon data
#'
#' Convert string representation of decimal degrees to numeric.
#'
#' @md
#' @param x     String representing decimal degrees that end in N,S,E,W
#'
#' @export
#' @examples
#' prep_lat_lon(c("121.51W", "121.51E"))
#' prep_lat_lon(c("38.24N", "38.24S"))

prep_lat_lon <- function(x){
  as.numeric(gsub("[^0-9.-]+", "", x)) * ifelse(grepl("S|W", x), -1, 1)
}

#' Prep datetime
#'
#' Combine YYMMDD date and HHMMSS time into YYYY-MM-DD HH:MM:SS datetime.
#'
#' @md
#' @param date_str     Date string in YYMMDD format
#' @param time_str     Time string in HHMMSS format
#'
#' @export
#' @examples
#' prep_datetime("221212", "143000")

prep_datetime <- function(date_str, time_str){
  paste0("20", date_str, " ", time_str) |>
    as.POSIXct(format = "%Y%m%d %H%M%S") |>
    as.character()
}

#' Adjust datetime
#'
#' Adjust datetime string from UTC to pacific timezone or vice versa.
#'
#' @md
#' @param datetime_str     Datetime string in YYYY-MM-DD HH:MM:SS format
#' @param type             Direction for timezone adjustment: to_pacific or to_utc
#'
#' @export
#' @examples
#' # Daylight Savings Time started on 2022-11-06
#' adjust_datetime(c("2022-11-05 12:00:00", "2022-11-07 12:00:00"), "to_pacific")

adjust_datetime <- function(datetime_str, type = c("to_pacific", "to_utc")){
  # https://stackoverflow.com/a/32118896/2912447
  utc = as.POSIXct(datetime_str, tz = "UTC")
  pacific = as.POSIXct(datetime_str, tz = "America/Los_Angeles")
  off_set = as.numeric(difftime(pacific, utc, units = "hours"))
  if (type == "to_utc"){
    out = pacific + lubridate::hours(off_set)
  } else {
    out = utc - lubridate::hours(off_set)
  }
  as.character(out)
}

#' Get sample date
#'
#' Get the sample date from datetime strings based on threshold time (to accommodate sampling that extends beyond midnight).
#'
#' @md
#' @param datetime_str     Datetime string in the format YYYY-MM-DD HH:MM:SS
#' @param threshold_time   Time string in the format HH:MM:SS
#'
#' @export
#' @examples
#' get_sample_date(c("2023-01-05 23:00:00", "2023-01-06 03:00:00", "2023-01-06 06:00:00"))

get_sample_date <- function(datetime_str, threshold_time = "05:00:00"){
  date_str = substr(datetime_str, 1, 10)
  ifelse(datetime_str > paste(date_str, threshold_time),
         date_str,
         as.character(as.Date(date_str) - lubridate::days(1)))
}

#' Prep data file
#'
#' Prep Columbus GPS data file.
#'
#' @md
#' @param data_file    Path to Columbus GPS data file
#'
#' @export

prep_data_file <- function(data_file){
  raw = readr::read_csv(data_file, col_types = "ilccccnnn")
  dt = adjust_datetime(prep_datetime(raw$DATE, raw$TIME), "to_pacific")
  data.frame(sample_date = get_sample_date(dt),
             datetime = dt,
             lat = prep_lat_lon(raw[["LATITUDE N/S"]]),
             lon = prep_lat_lon(raw[["LONGITUDE E/W"]]))
}
