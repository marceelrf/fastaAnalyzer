dna2rna <- function(tbl, keepSequence = TRUE) {

  tmp <- tbl |>
    dplyr::mutate(rna = stringr::str_replace_all(sequence, pattern = "T", replacement = "U"))

  if (keepSequence == FALSE) {
    tmp <- tmp |>
      dplyr::select(-sequence)
  }

  return(tmp)
}
