#' Internal function - generic downloader, used to serve the rds files from S3
#'
#' @param file file to be downloaded (or not...) from S3
#' @keywords internal

downloader <- function(file) {
  network <- as.logical(Sys.getenv("NETWORK_UP", unset = TRUE)) # dummy variable to allow testing of network
  aws <- as.logical(Sys.getenv("AWS_UP", unset = TRUE)) # dummy variable to allow testing of network

  remote_path <- "https://rczechia.jla-data.net/" # a remote archive
  local_dir <- tools::R_user_dir("RCzechia", which = "cache") # a local directory
  remote_file <- paste0(remote_path, file) # network path to file on AWS S3

  # is parent directory of local cache available?
  if(dir.exists(dirname(local_dir))) {

    # is the actual local cache dir available?
    if (!dir.exists(local_dir)) {

      dir.create(local_dir) # if missing, create!

    } # / create local cache dir

  } else {

    # nowhere to create local cache; use tempdir as fallback
    local_dir <- tempdir()

  } #/ directory check

  local_file <- file.path(local_dir, file) # local file

  if (file.exists(local_file) & network & aws) {
    message("RCzechia: using temporary local dataset.")
  } else {
    if (!curl::has_internet() | !network ) { # network is down
      message("No internet connection.")
      return(NULL)
    }

    if (httr::http_error(remote_file) | !aws) { # AWS bucket down
      message("Data source broken.")
      return(NULL)
    }

    # proceed to download via curl
    message("RCzechia: downloading remote dataset.")
    curl::curl_download(url = remote_file, destfile = local_file, quiet = T)
   } # /if - local file exists

  local_df <- readRDS(local_file)

  if (inherits(local_df, "sf")) {
    # to enforce consistent crs with PROJ both old & new
    sf::st_crs(local_df) <- 4326
  }

  local_df

} # /function
