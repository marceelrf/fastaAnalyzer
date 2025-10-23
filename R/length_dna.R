#' Calcule o tamanho da sequencia de DNA
#'
#' Esta função recebe uma tabela contendo a coluna 'sequence' e calcula o total de bases de DNA.
#'
#' @param tbl Uma tabela (data.frame ou tibble) que contenha a coluna 'sequence'
#' @param keepSequence Um valor logico para manter (default) a coluna que contem as sequencias
#'
#' @import dplyr
#' @importFrom stringr str_length
#'
#' @return Um `tbl` contendo a coluna BPs que informa a quantidade de
#' @export
#'
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
