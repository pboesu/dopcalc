# Programmatically Accessing
# NANUS, Almanacs and Ops Advisories
# It is now possible to programmatically access and download the contents of the various
# NANU, Almanacs (SEM AND YUMA) and Ops Advisory files without the need to
# spider the various listings.
# The base url for all of the programmatic accesses is : https://www.navcen.uscg.gov/
#   Items in brackets {} designate optional parameters.
# NANUs: To get the contents of a NANU file, use the base url followed by:
#   ?Do=getNanu&nanu=NNN{&year=YYYY}
# where NNN is the number of the NANU from 1 to 999 and an optional
# year starting from 1997 to the current year. (Note: 1997 starts with NANU
#                                               111.)
# If a year is not supplied, it will default to the current year.
# Almanacs: To get the contents of an Almanac file, use the base url followed by:
#   ?Do=getAlmanac&almanac=NNN{&year=YYYY}{&format=(yuma/sem)}
# where NNN is the number of the almanac from 1 to 999, an optional year
# starting from 1998 to the current year. (Note: 1998 starts with almanac 15.)
# and an optional format which would be either ‘sem’ or ‘yuma’.
# If a year is not supplied, it will default to the current year.
# If format is not supplied, it will default to the YUMA format.
# Ops Advisories: To get the contents of an Almanac file, use the base url followed by:
#   ?Do=getAdvisory&advisory=NNN{&year=YYYY}
# where NNN is the number of the Ops Advisory from 1 to 999, an optional
# year starting from 2000 to the current year. (Note: files from 1997 – 1999
#                                               are also available but they have gaps between the numbers. 2000-2002
#                                               starts at 003, 2003 starts at 002.
#                                               If a year is not supplied, it will default to the current year.
#                                               If the files that are requested exist, the contents of the file will be returned as non-html
#                                               text. No file names or dates for the file will be returned.
#                                               If a database error occurs, the following message will be returned:
#                                                 ERROR: A database error prevents the display of the page
#                                               If the file number is not found, the following message will be returned:
#                                                 ERROR: No such file
#                                               General Coding Logic:
#                                                 For getting almanacs
#                                               Set i equal to the starting point of the files you wish to download
#                                               Set year equal to the desired year; i.e. 2010
#                                               Set type equal to the format type desired; i.e. “sem”
#                                               for(i;i<1000;i++)
#                                               {
#                                                 url=https://www.navcen.uscg.gov/?Do=gpsAlmanac&almanac=i&year=year&for
#                                                 mat=type
#                                                 content = getContentFromUrl(url)
#                                                 if(!regx(“^ERROR:”,content))
#                                                   writeContentToFile(content)
#                                               }
#                                               The same general logic can be used for both NANUs and Ops Advisories by replacing
#                                               the gpsAlmanac with the proper command and leaving out the format parameter in the url.


#architecture idea:
# current approach uses scan anyways to access the almanc file so we can use a wrapper function to create the connection either from a file or from a url.
#also, currently there's no real parsing and/or input control, and the code traverses a largely unstructured vector of data. that should probably change.

paste_almanac_url <- function(yday, year){
  nnn <- sprintf('%03d', yday)
  url_string <- paste('https://www.navcen.uscg.gov/?Do=gpsAlmanac&almanac=',
             nnn,
             '&year=',
             year,
             '&format=sem',
             sep = '')
  return(url_string)
}

validate_almanac_url <- function(url_string){
  con <- url(url_string)
  #some error checking should occur here to chack that there is an internet connection in the first place, no 404 error etc
  header <- readLines(con, n = 1, warn = FALSE)
  if (grepl('ERROR', header)){
      url_valid <- FALSE
      reason <- header
    } else {
      url_valid <- TRUE
      reason <- NULL
    }
  close(con)
  return(list(url_valid = url_valid, reason = reason))
}

#' Attempt to find appropriate almanac file for a given date
#'
#' @param year numeric year
#' @param month numeric month
#' @param day numeric day of month
#' @param gps_week numeric gps_week
#'
#' @return a string
#'
#' @importFrom lubridate yday ymd
#'
#' @export
#'
get_almanac_url <- function(year, month, day, gps_week){
  yday <- lubridate::yday(lubridate::ymd(paste(year, month, day, sep = '-')))
  #first guess
  url_string <- paste_almanac_url(yday, year)
  #check if this is valid
  url_status <- validate_almanac_url(url_string)
  date_offset <- 1
  while (!url_status$url_valid && date_offset < 30){ #arbitrary cutoff value, probably should be user controlled
    url_string <- paste_almanac_url(yday + date_offset, year)
    url_status <- validate_almanac_url(url_string)
    if (!url_status$url_valid) {
      url_string <- paste_almanac_url(yday + date_offset, year)
      url_status <- validate_almanac_url(url_string)
    }
    date_offset <- date_offset + 1
  }
  if (!url_status$url_valid) {
    stop(paste('No valid almanac file found within 30 days of requested date'))
  } else {
  return(url_string)
  }
}
