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
