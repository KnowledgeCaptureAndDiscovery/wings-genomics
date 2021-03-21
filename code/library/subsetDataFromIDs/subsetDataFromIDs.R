
# Subset rows from a column in dataframe based on input IDs
dataFrameSubsetFromIDs <- function(wide.data.df,  filt.data.ids, filt.data.colname, filt.data.outfile) {
	wide.data.filt.df <- wide.data.df[wide.data.df[,filt.data.colname] %in% filt.data.ids,]
	write.table(wide.data.filt.df, filt.data.outfile, row.names = F, quote = F, sep = "\t")
	return(wide.data.filt.df)
}

subsetDataFromIDs <- function() {
	args = commandArgs(trailingOnly=TRUE)
	wide.data.file <- args[1]
	filt.data.ids.file <- args[2]
	filt.data.colname <- args[3]
	filt.data.outfile <- args[4]

	wide.data.df <- read.delim(wide.data.file, header = T, stringsAsFactors = F, check.names = F)
	filt.data.ids.df <- read.delim(filt.data.ids.file, header = T, stringsAsFactors = F)
	filt.data.ids <- filt.data.ids.df[,1]
	
	print("Subset Dataframe from given IDs...")	
	wide.data.filt.df <- dataFrameSubsetFromIDs(wide.data.df,  filt.data.ids, filt.data.colname, filt.data.outfile)
	print("Done!")
	print(paste("Filename:", filt.data.outfile))
}

subsetDataFromIDs()
