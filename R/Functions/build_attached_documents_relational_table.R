build_attached_documents_relational_table <- function(page, primary_code) {
  attached_codes <- extract_attached_docs(page)
  tibble(
    primary_code = primary_code,
    attached_code = as.numeric(attached_codes)
  )
}

safely_build_attached_documents_relational_table <- safely(
  build_attached_documents_relational_table
)
