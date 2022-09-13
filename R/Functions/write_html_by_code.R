write_html_page_by_code <- function(page, code, path) {
  file <- paste0(path, "/", code, ".html")
  xml2::write_html(page, file)
}