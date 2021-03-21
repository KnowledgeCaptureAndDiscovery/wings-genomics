args <- commandArgs(TRUE)

library(customProDB)

## Run the function to generate the protein sequence fasta file
easyRun(bamFile = args[1],
        vcfFile = args[2],
        bedFile = args[3],
        annotation_path = args[4],
        outfile_path = dirname(args[5]),
        outfile_name = basename(args[5]),
        INDEL = args[6],        
        COSMIC = args[7],
        lablersid = args[8],
        rpkm_cutoff = as.numeric(as.character(args[9])),
        nov_junction = args[10])
