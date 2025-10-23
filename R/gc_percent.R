#' Calculate GC content in DNA sequences
#'
#' This function computes the GC content (the proportion or percentage of
#' guanine and cytosine bases) for each sequence in a column named `sequence`.
#' It adds a new column `GC_percent` containing either the fraction or
#' percentage of G and C bases relative to the total sequence length.
#'
#' @param tbl A `data.frame` or `tibble` containing a column named `sequence`
#' with DNA sequences (character strings).
#' @param keepSequence Logical (default = `TRUE`). If `FALSE`, the original
#' `sequence` column will be removed from the output.
#' @param as_percent Logical (default = `TRUE`). If `TRUE`, the GC content is
#' expressed as a percentage (0–100). If `FALSE`, values range from 0 to 1.
#'
#' @return A `tibble` containing the input data (optionally without the
#' `sequence` column) and a new column `GC_percent` representing the GC content
#' for each sequence.
#'
#' @details
#' The GC content is calculated as the number of G and C bases divided by the
#' total sequence length:
#' \deqn{GC = (G + C) / (A + T + G + C)}
#'
#' If `as_percent = TRUE`, the result is multiplied by 100.
#'
#' @examples
#' library(dplyr)
#'
#' dna_tbl <- tibble::tibble(
#'   id = 1:3,
#'   sequence = c("ATCG", "AATTCCGG", "GGGTTTAA")
#' )
#'
#' # GC content as percentage
#' gc_percent(dna_tbl)
#'
#' # GC content as proportion (0–1 scale)
#' gc_percent(dna_tbl, as_percent = FALSE)
#'
#' # Remove sequence column
#' gc_percent(dna_tbl, keepSequence = FALSE)
#'
#' @importFrom dplyr mutate select
#' @importFrom stringr str_count str_length
#'
#' @export
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
