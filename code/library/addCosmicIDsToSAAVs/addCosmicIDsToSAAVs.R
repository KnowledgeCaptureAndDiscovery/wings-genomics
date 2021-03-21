# Add COSMIC IDs to SAAV data
addCosmicInfo <- function(saav.df, snv.df,
						  var.colname,
						  aa.refposvar.cols,
						  snv.retain.cols,
						  merge.by.cols,
						  dbsnp.colname,
						  saav.withcosmic.outfile) {
	snv.df[,var.colname] <- do.call(paste, c(snv.df[aa.refposvar.cols], sep=""))
	saav.snv.df <- merge(unique(snv.df[,snv.retain.cols]),
                         	 saav.df,
	                         by = merge.by.cols, all.y =T)

	saav.snv.df[saav.snv.df[,dbsnp.colname] == "",
                	dbsnp.colname] <- NA

	write.table(saav.snv.df, saav.withcosmic.outfile, row.names = F, sep = "\t", quote = F)

	return(saav.snv.df)
}

addCosmicIDsToSAAVs <- function() {
	args = commandArgs(trailingOnly=TRUE)
	saav.file <- args[1]
	snv.file <- args[2]
	snv.retain.cols.str <- args[3]
	merge.by.cols.str <- args[4]
	aa.refposvar.cols.str <- args[5]
	var.colname <- args[6]
	dbsnp.colname <- args[7]
	saav.withcosmic.outfile <- args[9]

	aa.refposvar.cols <- unlist(strsplit(aa.refposvar.cols.str, ","))
	snv.retain.cols <- unlist(strsplit(snv.retain.cols.str, ","))
	merge.by.cols <- unlist(strsplit(merge.by.cols.str, ","))

	saav.df <- read.delim(saav.file, header = T, stringsAsFactors = F, check.names = F)
	snv.df <- read.delim(snv.file, header = T, stringsAsFactors = F,check.names = F)

	print("Adding COSMIC information to SAAV table...")
	var.pep.saav.df.list <- addCosmicInfo(saav.df, snv.df,
						  				  var.colname,
										  aa.refposvar.cols,
										  snv.retain.cols,
										  merge.by.cols,
										  dbsnp.colname,
										  saav.withcosmic.outfile)
	print("Done!")
	print(paste("Filename:", saav.withcosmic.outfile))
}

addCosmicIDsToSAAVs()
