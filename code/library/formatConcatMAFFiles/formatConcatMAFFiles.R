# Add Mutation Annotation Format Annotations: combine by gene:
# This is mainly for TCGA-somatic mutation annotations:
formatFilterOneMAFFile <- function(maf.file,
								   sample.id,
								   sample.colname,
								   maf.geneid.colname,
								   geneid.colname,
                                   mut.status.colname,
                                   mut.status.value,
                                   val.status.colname,
                                   val.status.value,
                                   tcgasom.var.colname,
                                   retain.colnames) {
  # Retaining only required columns and somatic-validated mutations
  # Retain only missense mutations???
  input.maf.df <- read.delim(maf.file, header = T, stringsAsFactors = FALSE, check.names = FALSE)
  som.mut.df <- input.maf.df[input.maf.df[,mut.status.colname] == mut.status.value &
                             input.maf.df[, val.status.colname] == val.status.value,]

  if(nrow(som.mut.df) > 0) {
    som.mut.df[,tcgasom.var.colname] <- mut.status.value
    som.mut.df[,geneid.colname] <- som.mut.df[,maf.geneid.colname]
    som.mut.df[,sample.colname] <- sample.id
    som.mut.df <- som.mut.df[,colnames(som.mut.df) %in% retain.colnames]
    return(som.mut.df)
  }
  
}

# May need to provide few inputs as args later?
formatFilterMAFFiles <- function(maf.files,
								 sample.ids,
                                 sample.colname,
                                 maf.geneid.colname,
                                 geneid.colname,
                                 mut.status.colname,
                                 val.status.colname,
                                 mut.status.value,
                                 val.status.value,
                                 tcgasom.var.colname,
                                 retain.colnames,
                                 long.maf.filt.outfile) {
  var.classif.colname <- "Variant_Classification"#-->missense etc.
  
  long.maf.info.df <- data.frame()
  if(length(maf.files) == 1) {
    print("One input MAF file found.")
    long.maf.info.df <- formatFilterOneMAFFile(maf.file = maf.files,
                                               sample.ids,
                                               sample.colname,
                                               maf.geneid.colname,
											   geneid.colname,
                        			           mut.status.colname,
			                                   mut.status.value,
            			                       val.status.colname,
			                                   val.status.value,
			                                   tcgasom.var.colname,
            			                       retain.colnames)
  } else {
    maf.somatic.list <- lapply(1:length(maf.files),
                               function(x)
                                 formatFilterOneMAFFile(
                                   maf.file = maf.files[x],
                                   sample.id = sample.ids[x],
                                   sample.colname,
                                   maf.geneid.colname,
								   geneid.colname,
                                   mut.status.colname,
			                       mut.status.value,
			                       val.status.colname,
			                       val.status.value,
			                       tcgasom.var.colname,
			                       retain.colnames))
    long.maf.info.df <- unique(do.call(rbind, maf.somatic.list))
  }
 
  write.table(long.maf.info.df, long.maf.filt.outfile, sep="\t", row.names = FALSE, quote = FALSE)
  return(long.maf.filt.outfile)
}

formatConcatMAFFiles <- function() {
  args <- commandArgs(TRUE)
  maf.files.str <- args[1]
  sample.id.file <- args[2]
  sample.colname <- args[3]
  maf.geneid.colname <- args[4]
  geneid.colname <- args[5]
  mut.status.colname <- args[6]
  val.status.colname <- args[7]
  mut.status.value <- args[8]
  val.status.value <- args[9]
  tcgasom.var.colname <- args[10]
  retain.colnames.str <- args[11]
  long.maf.filt.outfile <- args[12]
  
  sample.id.df <- read.delim(sample.id.file, header = F, stringsAsFactors = FALSE, check.names = FALSE)
  sample.ids <- sample.id.df[,1]
  maf.files <- unlist(strsplit(maf.files.str, ","))
  retain.colnames <- unlist(strsplit(retain.colnames.str, ","))

  # Format each input MAF File and concat all-sample files into one.
  # The sampleID sequence should match the input file sequence (1:1)
  print("Formatting and filtering MAF files...")
  long.maf.info.filename <- formatFilterMAFFiles(maf.files,
								 				 sample.ids,
				                                 sample.colname,
                				                 maf.geneid.colname,
				                                 geneid.colname,
                				                 mut.status.colname,
				                                 val.status.colname,
                				                 mut.status.value,
				                                 val.status.value,
				                                 tcgasom.var.colname,
                				                 retain.colnames,
                                				 long.maf.filt.outfile)
  return(long.maf.info.filename)
  print("Done!")
}

formatConcatMAFFiles()
