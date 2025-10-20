gc_percent <- function(tbl, keepSequence = TRUE, as_percent = TRUE) {

  tmp <- tbl |>
    dplyr::mutate(GC_percent = (stringr::str_count(string = sequence, pattern = "[CG]")) / stringr::str_length(sequence))

  if (keepSequence == FALSE) {
    tmp <- tmp |>
      dplyr::select(-sequence)
  }

  if (as_percent == TRUE) {
    tmp <- tmp |>
      dplyr::mutate(GC_percent = GC_percent * 100)
  }

  return(tmp)
}
