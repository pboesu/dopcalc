#' Convert a ymdhms date to the date format used by the GPS almanac
#'
#' Inputs are currently assumed to be in UTC
#'
#' @param year integer year as YYYY
#' @param month integer month
#' @param mday integer day of month
#' @param hour integer hour 0-24
#' @param minute integer minute
#' @param second integer second
#'
#' @return a list with the elements gps_week, the GPS week number,
#' and sec_of_week, the GPS time of applicability.
#' @details The GPS week number or almanac reference number is the
#'  number of weeks since 1980-01-06 modulo 1024. The GPS time of
#'  applicability is the number of seconds since the beginning of
#'  the almanac reference week.  See ICD-GPS-240A for further details.
#' @author Benjamin W. Remondi (original Fortran code), Philipp Boersch-Supan (R translation)
#' @references Global Positioning Systems Directorate (2018). Systems Engineering and Integration Interface Specification IS-GPS-200J NAVSTAR GPS Space Segment/Navigation User Segment Interfaces. \href{https://www.gps.gov/technical/icwg/IS-GPS-200J.pdf}{https://www.gps.gov/technical/icwg/IS-GPS-200J.pdf}
#'
#' @export
#'
ymdhms2gps <- function(year,month,mday,hour,minute,second){
# TODO: THere should be a method for the standard posix classes date
# TODO: handle timezones.
#
# Function ymdhms2gps
#
# Coputes GPS week and seconds of week for a given date
#
# Sintaxe:
  # function GPSt=ymdhms2gps(year,month,mday,hour,minute,second)
  #
  # Reference: Source code from the Remondi Date/Time Algorithms
  #            http://www.ngs.noaa.gov/gps-toolbox/bwr-f.txt
  #
  #      integer*4 gps_week,year,month,mday,hour,minute
  #      real*8    sec_of_week, second
  #      integer*4 jan61980, jan11901
  #      real*8    sec_per_day
  #      integer*4 yday, mjd, leap_month_day, regu_month_day
  #      real*8    fmjd


  regu_month_day= c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334);
  leap_month_day= c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335);

  jan61980 = 44244;
  jan11901 = 15385;
  sec_per_day = 86400;

  if (year %% 4 ==0){
    yday = leap_month_day[month] + mday;
  } else {
    yday = regu_month_day[month] + mday;
  }

  mjd = trunc(((year - 1901)/4))*1461 + trunc(((year - 1901) %% 4))*365 + yday - 1 + jan11901;
  fmjd = ((second/60.0 + minute)/60.0 + hour)/24.0;

  gps_week = trunc((mjd - jan61980)/7);
  sec_of_week = ( (mjd - jan61980) - gps_week*7 + fmjd )*sec_per_day;

  GPSt=list(gps_week=gps_week, sec_of_week=sec_of_week);

  return(GPSt)
}
