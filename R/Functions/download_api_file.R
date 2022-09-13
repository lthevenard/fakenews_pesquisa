download_api_file <- function(year, theme, path, format = "csv") {
  url <- build_api_url(year, theme, format)
  destfile <- paste0(path, "/", theme, "-", year, ".", format)
  download.file(url, destfile)
}

safely_and_slowly_download_api_file <- download_api_file %>% 
  slowly(rate_delay(10)) %>% 
  safely()