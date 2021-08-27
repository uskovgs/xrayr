#' Convert greg to JD astronomical
#'
#' @param year year
#' @param month
#' @param day
#' @param hour
#' @param min
#' @param sec
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' jd1 <- jd(2000, 1, 1, 12, 0, 0)
#' jd1
#' }
jd <- function(year,month,day,hour,min,sec){

  get_jdn <- function(year, month, day){
    a <- floor((14-month) / 12)
    y <- year + 4800 - a
    m <- month + 12*a - 3

    jdn <- day + floor((153*m+2)/5) + 365*y + floor(y/4) - floor(y/100) + floor(y/400) - 32045

    return(jdn)
  }

  jdn <- get_jdn(year, month, day)

  jd <- jdn + (hour-12)/24 + min/1440 + sec/86400

  return(jd)
}

#' Convert greg to JD astronomical. At first it converts to UTC.
#'
#' @param date
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' d0 <- ymd_hms('2000/01/01 15:00:00', tz='Europe/Moscow')
#' jd1 <- jd_fromdate(d0)
#' jd1
#' }
jd_fromdate <- function(date){

  date <- lubridate::with_tz(date, tzone = 'UTC')

  jd(year = lubridate::year(date),
     month = lubridate::month(date),
     day = lubridate::day(date),
     hour = lubridate::hour(date),
     min = lubridate::minute(date),
     sec = lubridate::second(date))
}

#' JD to greg.
#'
#' @param jd
#'
#' @return
#' @export
#'
#' @examples
jd2greg <- function(jd){

  ref_time <- lubridate::ymd_hms('1970-01-01 12:0:0', tz='UTC')
  ref_jd <- jd(1970, 1, 1, 12, 0, 0)

  jd_interval <- jd - ref_jd

  date <- ref_time + lubridate::duration(jd_interval, "days")
  lubridate::with_tz(date, 'UTC')
}


#' MJD to greg
#' MJD = JD − 2400000.5
#'
#' @param mjd
#'
#' @return
#' @export
#'
#' @examples
mjd2greg <- function(mjd){

  j <- mjd + 2400000.5
  return(jd2greg(j))
}
