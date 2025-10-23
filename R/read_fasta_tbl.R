#' Read a FASTA file into a tibble
#'
#' This function reads a FASTA-formatted file and returns a `tibble` with two
#' columns: one for the sequence names (tags) and one for the corresponding
#' sequences. It is a simple reader designed for FASTA files where each sequence
#' is written on a single line.
#'
#' @param file Path to the FASTA file to be read.
#' @param keepTag Logical (default = `FALSE`). If `FALSE`, the leading `">"`
#' character in the sequence names is removed. If `TRUE`, the tags are kept as
#' they appear in the file.
#'
#' @return A `tibble` with the following columns:
#' \describe{
#'   \item{sequence_names}{Character vector containing sequence identifiers (tags).}
#'   \item{sequence}{Character vector containing the corresponding DNA or RNA sequences.}
#' }
#'
#' @details
#' The function assumes a **two-line FASTA format**, where each record contains:
#' \enumerate{
#'   \item A header line starting with `">"` (sequence identifier).
#'   \item A single line containing the nucleotide or amino acid sequence.
#' }
#'
#' It is not suitable for multi-line FASTA entries (where a single sequence spans
#' multiple lines). For such cases, consider using `Biostrings::readDNAStringSet()`
#' or `seqinr::read.fasta()`.
#'
#' @examples
#' library(readr)
#' library(tibble)
#'
#' # Example: reading a simple FASTA file
#' # Suppose "example.fasta" contains:
#' # >seq1
#' # ATCG
#' # >seq2
#' # GGCATTA
#'
#' fasta_tbl <- read_fasta_tbl("fake.fasta")
#' print(fasta_tbl)
#'
#' # Keep the original tags (with ">")
#' fasta_tbl <- read_fasta_tbl("fake.fasta", keepTag = TRUE)
#'
#' @importFrom readr read_lines
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#'
#' @export
read_fasta_tbl <- function(file, keepTag = FALSE) {

  lines <- readr::read_lines(file = file,
                             skip_empty_rows = T)

  sequence_names <- lines[seq(1,length(lines),2)]
  sequence <- lines[seq(1,length(lines),2)+1]

  if(keepTag != TRUE) {
    sequence_names <- stringr::str_remove_all(sequence_names,">")
  }

  #tibble == data.frame
  tbl <- tibble::tibble(sequence_names, sequence = sequence)

  return(tbl)

}
