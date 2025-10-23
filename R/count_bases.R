#' Count DNA bases (A, T, C, G) in a sequence column
#'
#' This function counts the occurrences of each nucleotide base (A, T, C, and G)
#' in a column named `sequence` within a given data frame or tibble.
#' It adds four new columns (`A_count`, `T_count`, `C_count`, `G_count`) to the
#' input table, representing the number of each base per sequence.
#'
#' @param tbl A `data.frame` or `tibble` containing at least one column named `sequence`.
#' @param keepSequence Logical (default = `TRUE`). If `FALSE`, the original `sequence`
#' column will be removed from the output.
#'
#' @return A `tibble` containing the original data (optionally without the `sequence`
#' column) and four additional columns:
#' \describe{
#'   \item{A_count}{Number of adenine (A) bases in each sequence.}
#'   \item{T_count}{Number of thymine (T) bases in each sequence.}
#'   \item{C_count}{Number of cytosine (C) bases in each sequence.}
#'   \item{G_count}{Number of guanine (G) bases in each sequence.}
#' }
#'
#' @details
#' The function requires the input table to have a column named `sequence`
#' containing character strings representing DNA sequences.
#'
#' If `tbl` is missing or not a valid data.frame/tibble, a warning is issued.
#' If the `sequence` column is missing, an error is raised.
#'
#' @examples
#' library(dplyr)
#'
#' dna_tbl <- tibble::tibble(
#'   id = 1:3,
#'   sequence = c("ATCG", "AATTCCGG", "GGGTTTAA")
#' )
#'
#' # Count bases and keep the sequence column
#' count_bases(dna_tbl)
#'
#' # Count bases and remove the sequence column
#' count_bases(dna_tbl, keepSequence = FALSE)
#'
#' @importFrom dplyr mutate select
#' @importFrom stringr str_count
#'
#' @export
count_bases <- function(tbl, keepSequence = TRUE) {

  if (missing(tbl)) {
    warning("Argument 'tbl' is missing. Please provide a data.frame or tibble.")
  }

  if (!inherits(tbl, c("data.frame", "tbl", "tbl_df"))) {
    warning("Argument '.data' must be a data.frame or tibble.")
  }


  if ( !("sequence" %in% colnames(tbl))) {
    stop("Your tbl must contain 'sequence' column.")
  }

  stopifnot((is.character(tbl$sequence)))

  tmp <- tbl |>
    dplyr::mutate(A_count = stringr::str_count(sequence, "A"),
                  T_count = stringr::str_count(sequence, "T"),
                  C_count = stringr::str_count(sequence, "C"),
                  G_count = stringr::str_count(sequence, "G"))

  if (!keepSequence) {
    tmp <- tmp |>
      dplyr::select(-sequence)
  }

  return(tmp)
}
