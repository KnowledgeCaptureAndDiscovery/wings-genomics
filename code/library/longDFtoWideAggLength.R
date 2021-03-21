library(reshape2)

# Prior to this script, process files through formatIDPQueryData script to get the long format files
# Then process through tabulateIDsFromMultFiles to get a dataframe with rows as genes (no geneid column) and columns as datasets 

aggDataToWideByLength <- function(long.data.df, widecols.colname, id.colname, wide.data.outfile, ...) {
	longtowide.formula <- as.formula(paste(id.colname,"~",widecols.colname))
	retain.colnames <- c(id.colname, widecols.colname)
	wide.data.df <- dcast(unique(long.data.df[, colnames(long.data.df) %in% retain.colnames]),
							formula = longtowide.formula)

	write.table(wide.data.df, wide.data.outfile, row.names = F, quote = F, sep = "\t")
	return(wide.data.df)
}


longDFtoWideAggLength <- function() {
	args = commandArgs(trailingOnly=TRUE)
	long.data.file <- args[1]
	id.colname <- args[2]
	widecols.colname <- args[3]
	wide.data.outfile <- args[4]
	
	long.data.df <- read.delim(long.data.file, header = T, stringsAsFactors = F, check.names = F)
	print("Convert data from long to wide format and aggregate by length...")
	datasets.ids.df <- aggDataToWideByLength(long.data.df, widecols.colname, 
												id.colname, wide.data.outfile, 
												fun.aggregate = length)

	print("Done!")
	print(paste("Filename:", wide.data.outfile))

}

longDFtoWideAggLength()
