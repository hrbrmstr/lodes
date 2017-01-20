lodes_base <- "https://lehd.ces.census.gov/data/lodes/LODES7"

typ_lkp <- setNames(c("od", "rac", "wac", "od", "rac", "wac"),
                    c("origin", "residence", "workplace", "od", "rac", "wac"))

#' Retrieve a LODES file/data
#'
#' Files are cached in \code{download_dir}
#'
#' @references See \href{https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.2.pdf}{the codebook}
#'             for valid parameters.
#' @param state 2-leter state code (auto-converted to lower case)
#' @param type one of "origin", "residence" or "workplace" or their short
#'        equivalents "od", "rac", "wac"
#' @param segment segment identifier
#' @param job_type job identifier
#' @param year year to retrieve
#' @param download_dir location to cache the file (defaults to current directory)
#' @return `tibble` of results if data file was found
#' @note the full path to the downloaded file is displayed in message
#' @export
#' @examples
#' read_lodes("de", "od", "aux", "JT00", "2006")
#' read_lodes("ak", "residence", "S000", "JT00", "2002")
#' read_lodes("id", "workplace", "SE02", "JT03", "2013")
read_lodes <- function(state, type=c("origin", "residence", "workplace", "od", "rac", "wac"),
                       segment, job_type, year, download_dir=getwd()) {

  state <- tolower(state)
  type <- match.arg(tolower(type), c("origin", "residence", "workplace", "od", "rac", "wac"))
  type <- typ_lkp[type]

  sprintf("%s/%s/%s/%s_%s_%s_%s_%s.csv.gz",
          lodes_base, state, type, state, type, segment, job_type, year) -> URL

  httr::stop_for_status(httr::HEAD(URL),
    "data for that combination of parameters was not found on LODES")

  fil <- file.path(path.expand(download_dir), basename(URL))
  if (file.exists(fil)) {
    message(sprintf("Cached version of file found in [%s].\nReading data from it...", fil))
  } else {
    message(sprintf("Downloading [%s] to [%s]...", URL, fil))
    res <- httr::GET(URL, write_disk(fil))
  }

  suppressMessages(read_csv(fil))

}

#' Retrieve geography crosswalk for a state
#'
#' Reads the crosswalk file and returns a \code{tibble}.
#' Files are cached in \code{download_dir}
#'
#' @param state state to read crosswalk for
#' @param download_dir location to cache the file (defaults to current directory)
#' @export
#' @examples
#' read_xwalk("id")
read_xwalk <- function(state, download_dir=getwd()) {

  state <- tolower(state)

  sprintf("https://lehd.ces.census.gov/data/lodes/LODES7/%s/%s_xwalk.csv.gz", state, state) -> URL

  httr::stop_for_status(httr::HEAD(URL),
    "crosswalk data for that state of parameters was not found on LODES")

  fil <- file.path(path.expand(download_dir), basename(URL))
  if (file.exists(fil)) {
    message(sprintf("Cached version of file found in [%s].\nReading data from it...", fil))
  } else {
    message(sprintf("Downloading [%s] to [%s]...", URL, fil))
    res <- httr::GET(URL, write_disk(fil))
  }

  suppressMessages(as_tibble(read.csv(gzfile(fil), stringsAsFactors=FALSE)))

}


