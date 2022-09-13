count_codes <- function(codes) {
  output <- vector('list', length(codes))
  for (i in seq_along(codes)) {
    if (names(codes)[[i]] == "codes_tibble") {
      next()
    }
    output[[i]] <- tibble(
      category = names(codes)[[i]],
      count = length(codes[[i]])
    )
  }
  bind_rows(output)
}
