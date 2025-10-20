fake_seqs <- read_fasta_tbl(file = "fake.fasta",keepTag = TRUE)


length_dna(fake_seqs,keepSequence = FALSE)

gc_percent(fake_seqs,
           keepSequence = FALSE,
           as_percent = FALSE)

dna2rna(fake_seqs,keepSequence = T) |> View()
