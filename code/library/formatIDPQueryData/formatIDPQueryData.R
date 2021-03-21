
# Retain the required columns only
# Format the data to long format
# Add dataset name for further validation
# Write out both wide and long format files with required columns only 
# Ensure the separators are the same for proteins/genes in protein-groups/gene-groups column
# (Zhang used "," where as Wings used ";" to collapse protein/gene names into groups)
# FEW files have "/" before sample names while some DONOT.
readAndFormatData <- function(dataset.file,
								sample.ids,
								dataset.name,
								dataset.colname,
								sc.colname,
								geneid.colname="GeneId",
								protid.colname="Accession",
								isdecoy.colname="IsDecoy",
								sample.colname = "sample_id",
								dataset.outfile,
								long.dataset.outfile, ... ) {
	retain.colnames <- c(protid.colname, geneid.colname, sample.ids)
	dataset.prots.df <- read.delim(dataset.file, header = T, stringsAsFactors = F, check.names = F)
	colnames(dataset.prots.df) <- gsub("/", "", colnames(dataset.prots.df))
    if(isdecoy.colname %in% colnames(dataset.prots.df)) {
		dataset.prots.df <- dataset.prots.df[dataset.prots.df[,isdecoy.colname] != 1,
                                         colnames(dataset.prots.df) %in% retain.colnames]

    }
	# Add dataset name
	dataset.prots.df[,dataset.colname] <- dataset.name
	long.retain.colnames <- c(geneid.colname, dataset.colname, sample.ids)
	long.dataset.prots.df <- reshape2::melt(dataset.prots.df[,colnames(dataset.prots.df) %in%
                                                               long.retain.colnames],
											variable.name = sample.colname,
											value.name = sc.colname)
	write.table(dataset.prots.df, dataset.outfile, sep="\t", quote=F, row.names=F)
	write.table(long.dataset.prots.df, long.dataset.outfile, sep="\t", quote=F, row.names=F)
	return(list(dataset.prots.df, long.dataset.prots.df))
}

# Read and Parse command line arguments 
# Call the required function to format the data
formatIDPQueryData <- function() {
	args = commandArgs(trailingOnly=TRUE)
	dataset.file <- args[1]
	sampleids.file <- args[2]
	dataset.name <- args[3]
	dataset.colname <- args[4]
	sc.colname <- args[5]
	geneid.colname <- args[6]
	protid.colname <- args[7]
	isdecoy.colname <- args[8]
	sample.colname <- args[9]
	long.dataset.outfile <- args[10]
	dataset.outfile <- args[11]

	#dataset.outfile <- "wideFormattedProts.txt"
	#long.dataset.outfile <- "longFormattedProts.txt"
	
	sample.ids.df <- read.delim(sampleids.file, header = F, stringsAsFactors = F)
	sample.ids <- sample.ids.df[,1]

	wide.long.dataset.df.list <- readAndFormatData(dataset.file,
                                        			sample.ids,
                                        			dataset.name,
                                        			dataset.colname,
                                        			sc.colname,
                                        			geneid.colname= geneid.colname,
                                        			protid.colname= protid.colname,
                                        			#protlen.colname = protlen.colname,
                                        			isdecoy.colname = isdecoy.colname,
                                        			sample.colname = sample.colname,
                                        			dataset.outfile,
                                        			long.dataset.outfile)
	print("Formatting  idpQuery Data Done!")
	print(paste("Filenames:", paste(dataset.outfile,
									long.dataset.outfile, sep=",")))
  
}

formatIDPQueryData()
