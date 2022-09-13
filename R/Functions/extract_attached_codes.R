extract_attr_by_xpath <- function(page, xpath, attr = "href") {
  page %>% 
    html_nodes(xpath = xpath) %>% 
    html_attr(attr) %>% 
    str_squish()
}

extract_attached_docs <- function (
    page, xpath = "//span[@class = 'mostrarToolTip']/a"
) {
  page %>% 
    extract_attr_by_xpath(xpath) %>% 
    str_extract("\\d+$")
}
