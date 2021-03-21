
# Calculate means  per row of the dataset and shortlist rows above the given cut off
getIDsGEMeanCutoff <- function(wide.data.df, cutoff.value,
								wide.data.filt.outfile, filt.list.outfile) {
	data.rowmeans <- rowMeans(wide.data.df)
	data.rowmeans.gecutoff <- names(data.rowmeans[data.rowmeans >= cutoff.value])
	
	# Select data with values above cut-off
	wide.data.filt.df <- wide.data.df[data.rowmeans.gecutoff,]
	write.table(wide.data.filt.df, wide.data.filt.outfile, row.names = T, quote = F, sep = "\t")
	write.table(data.rowmeans.gecutoff, filt.list.outfile, row.names = F, quote = F, sep = "\t")
	return(list(wide.data.filt.df, data.rowmeans.gecutoff))
}

selectIDsGEAvgCutoff <- function() {
	args = commandArgs(trailingOnly=TRUE)
	wide.data.file <- args[1]
	cutoff.value <- as.numeric(as.character(args[2]))
	wide.data.filt.outfile <- args[3]
	filt.list.outfile <- args[4]

	wide.data.df <- read.delim(wide.data.file, header = T, stringsAsFactors = F, check.names = F)
	
	print("Get list of IDs with mean values >= the cut off value across rows...")
	data.rowmeans.gecutoff <- getIDsGEMeanCutoff(wide.data.df, cutoff.value,
													wide.data.filt.outfile, filt.list.outfile)

	print("Done")
	print(paste("Filenames:", filt.list.outfile, wide.data.filt.outfile))
}

selectIDsGEAvgCutoff()
