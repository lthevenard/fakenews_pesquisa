randomly_assign_researchers <- function(df, researchers, name, seed) {
  set.seed(seed)
  df <- df %>% slice_sample(prop = 1) %>% mutate(aaabbbccc = "")
  
  numrows <- nrow(df)
  step <- ceiling(numrows / length(researchers))
  
  for (i in seq_along(researchers)) {
    start <- ((i -1) * step) + 1
    end <- start + step - 1
    if (end > numrows) {
      end <- numrows
    }
    df$aaabbbccc[start:end] <- researchers[[i]]
  }
  return( df %>% rename(!!quo_name(name) := "aaabbbccc") )
}

randomly_assign_researchers_twice <- function(df, researchers, seed) {
  
  first_assignment <- randomly_assign_researchers(df, researchers, "primary_classification", seed)
  
  review_assignments <- vector('list', length(researchers))
  
  for (i in seq_along(researchers)) {
    researcher <- researchers[[i]]
    remaining_researchers <- researchers[researchers != researcher]
    
    review_assignments[[i]] <- first_assignment %>% 
      filter(primary_classification == researcher) %>% 
      randomly_assign_researchers(remaining_researchers, "reviewer" , seed+(i*5))
  }
  return(bind_rows(review_assignments))
}
