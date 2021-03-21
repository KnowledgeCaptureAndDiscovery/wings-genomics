# Retaining only required columns and non-synonymous mutations
formatFilterOnesnvFile <- function(snv.file,
								   sample.id,
								   sample.colname,
								   snv.geneid.colname,
								   geneid.colname,
								   vartype.colname,
								   vartype.value,
                                   retain.colnames) {
  input.snv.df <- read.delim(snv.file, header = T, stringsAsFactors = FALSE, check.names = FALSE)
  nonsyn.mut.df <- input.snv.df[input.snv.df[,vartype.colname] == vartype.value,]

  if(nrow(nonsyn.mut.df) > 0) {
    nonsyn.mut.df[,geneid.colname] <- nonsyn.mut.df[,snv.geneid.colname]
    nonsyn.mut.df[,sample.colname] <- sample.id
    nonsyn.mut.df <- nonsyn.mut.df[, colnames(nonsyn.mut.df) %in% retain.colnames]
    return(nonsyn.mut.df)
  }
}

# May need to provide few inputs as args later?
filtSNVTableFiles <- function(snv.files,
								 sample.ids,
                                 sample.colname,
                                 snv.geneid.colname,
                                 geneid.colname,
                                 vartype.colname,
                                 vartype.value,
                                 retain.colnames,
                                 filt.snv.outfile) {
  var.classif.colname <- "Variant_Classification"
  
  filt.snv.df <- data.frame()
  if(length(snv.files) == 1) {
    print("One input snv file found.")
    filt.snv.df <- formatFilterOnesnvFile(snv.file = snv.files,
								 sample.ids,
                                 sample.colname,
                                 snv.geneid.colname,
                                 geneid.colname,
                                 vartype.colname,
                                 vartype.value,
                                 retain.colnames)
  } else {
    filt.snv.df.list <- lapply(1:length(snv.files),
                               function(x)
                                 formatFilterOnesnvFile(
                                   snv.file = snv.files[x],
                                   sample.id = sample.ids[x],
                                   sample.colname,
                                   snv.geneid.colname,
                                   geneid.colname,
								   vartype.colname,
								   vartype.value,
			                       retain.colnames))
    filt.snv.df <- unique(do.call(rbind, filt.snv.df.list))
  }
 
  write.table(filt.snv.df, filt.snv.outfile, sep="\t", row.names = FALSE, quote = FALSE)
  return(filt.snv.df)
}

formatSNVTableFiles <- function() {
  args <- commandArgs(TRUE)
  snv.files.str <- args[1]
  sample.id.file <- args[2]
  sample.colname <- args[3]
  snv.geneid.colname <- args[4]
  geneid.colname <- args[5]
  vartype.colname <- args[6]
  vartype.value <- args[7]
  retain.colnames.str <- args[8]
  filt.snv.outfile <- args[9]
  
  sample.id.df <- read.delim(sample.id.file, header = F, stringsAsFactors = FALSE, check.names = FALSE)
  sample.ids <- sample.id.df[,1]
  snv.files <- unlist(strsplit(snv.files.str, ","))
  retain.colnames <- unlist(strsplit(retain.colnames.str, ","))

  # Format each input SNV table File and concat all-sample files into one.
  # The sampleID sequence should match the input file sequence (1:1)
  print("Formatting and filtering SNV files...")
  filt.snv.df <- filtSNVTableFiles(snv.files,
								 				 sample.ids,
				                                 sample.colname,
                				                 snv.geneid.colname,
				                                 geneid.colname,
				                                 vartype.colname,
				                                 vartype.value,
                				                 retain.colnames,
                                				 filt.snv.outfile)
  return(filt.snv.outfile)
  print("Done!")
}

formatSNVTableFiles()
