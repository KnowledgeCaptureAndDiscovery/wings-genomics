
# Subset columns in dataframe based on the list of input column names and one identifier
dataFrameSubsetCols <- function(wide.data.df,  filt.data.ids, id.colnames, filt.data.outfile) {
	retain.colnames <- c(id.colnames, filt.data.ids)
	wide.data.filt.df <- wide.data.df[,colnames(wide.data.df) %in% retain.colnames]
	write.table(wide.data.filt.df, filt.data.outfile, row.names = F, quote = F, sep = "\t")
	return(wide.data.filt.df)
}

subsetRequiredCols <- function() {
	args = commandArgs(trailingOnly=TRUE)
	wide.data.file <- args[1]
	filt.data.ids.file <- args[2]
	id.colnames.str <- args[3]
	filt.data.outfile <- args[4]

	id.colnames <- unlist(strsplit(id.colnames.str, split=","))
	wide.data.df <- read.delim(wide.data.file, header = T, stringsAsFactors = F, check.names = F)
	filt.data.ids.df <- read.delim(filt.data.ids.file, header = F, stringsAsFactors = F)
	filt.data.ids <- filt.data.ids.df[,1]
	
	print("Subset Dataframe to retain the input ID column and other input columns...")	
	wide.data.filt.df <- dataFrameSubsetCols(wide.data.df,  filt.data.ids, id.colnames, filt.data.outfile)
	print("Done!")
	print(paste("Filename:", filt.data.outfile))
}

subsetRequiredCols()
