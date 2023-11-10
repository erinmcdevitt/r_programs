#Freder Loop Function 


fred_pull <- function(list_of_ids, freq, unit) {
  
  list <- list(
    series_id = c(list_of_ids),
    frequency = c(freq)
  )
  
  data <- pmap_dfr(
    .l = list,
    .f = ~ fredr(series_id = .x, frequency = .y), unit = unit) %>%
    select(c(date, series_id, value)) %>%
    arrange(series_id, date)

  return(data)
  
}

#example
#credit <- fred_pull(("SERIES_ID1", "SERIES_ID2"), "m", "ln")
