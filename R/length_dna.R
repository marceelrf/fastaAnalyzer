length_dna <- function(tbl, keepSequence = TRUE) {

  tmp <- tbl |>
    dplyr::mutate(BPs = stringr::str_length(sequence))

  if (keepSequence == TRUE) {
    return(tmp)
  } else {
    tmp <- tmp |>
      dplyr::select(-sequence)
  }

  return(tmp)
}
