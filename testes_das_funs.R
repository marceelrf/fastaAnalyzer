fake_seqs <- read_fasta_tbl(file = "fake.fasta",keepTag = TRUE)


length_dna(fake_seqs,keepSequence = FALSE)

gc_percent(fake_seqs,
           keepSequence = FALSE,
           as_percent = FALSE)

dna2rna(fake_seqs,keepSequence = T) |> View()

dados_errados <- tibble::tibble(sequence_names = c("A","B"),
               sequence = 1:2)

count_bases(dados_errados)


 dna_tbl <- tibble::tibble(
   id = 1:3,
   sequence = c("ATCG", "AATTCCGG", "GGGTTTAA")
 )

 count_bases(dna_tbl)
