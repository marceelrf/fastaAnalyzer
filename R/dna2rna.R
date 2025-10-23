#' Convert DNA sequences to RNA sequences
#'
#' This function converts DNA sequences into RNA sequences by replacing all
#' occurrences of thymine (T) with uracil (U) in a column named `sequence`.
#' It returns a table containing a new column `rna` with the converted sequences.
#'
#' @param tbl A `data.frame` or `tibble` containing a column named `sequence`
#' with DNA sequences (character strings).
#' @param keepSequence Logical (default = `TRUE`). If `FALSE`, the original
#' `sequence` column will be removed from the output.
#'
#' @return A `tibble` containing the input data (optionally without the
#' `sequence` column) and a new column `rna` with the RNA version of each sequence.
#'
#' @details
#' The function performs a simple base replacement (`T` → `U`) to simulate
#' transcription from DNA to RNA. The input column `sequence` must contain
#' character strings composed of DNA bases (A, T, C, G).
#'
#' @examples
#' library(dplyr)
#'
#' dna_tbl <- tibble::tibble(
#'   id = 1:3,
#'   sequence = c("ATCG", "AATTCCGG", "GGGTTTAA")
#' )
#'
#' # Convert DNA to RNA and keep the original sequence column
#' dna2rna(dna_tbl)
#'
#' # Convert DNA to RNA and remove the original sequence column
#' dna2rna(dna_tbl, keepSequence = FALSE)
#'
#' @importFrom dplyr mutate select
#' @importFrom stringr str_replace_all
#'
#' @export
dna2rna <- function(tbl, keepSequence = TRUE) {

  tmp <- tbl |>
    dplyr::mutate(rna = stringr::str_replace_all(sequence, pattern = "T", replacement = "U"))

  if (keepSequence == FALSE) {
    tmp <- tmp |>
      dplyr::select(-sequence)
  }

  return(tmp)
}
