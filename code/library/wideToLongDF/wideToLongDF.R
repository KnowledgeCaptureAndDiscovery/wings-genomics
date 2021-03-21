library(reshape2)

# Prior to this script, process files through formatIDPQueryData script to get the long format files
# Then process through tabulateIDsFromMultFiles to get a dataframe with rows as genes (no geneid column) and columns as datasets 

formatWideDataToLong <- function(wide.data.df, long.data.outfile, ...) {
	long.data.df <- melt(wide.data.df, ...)
	write.table(long.data.df, long.data.outfile, row.names = F, quote = F, sep = "\t")
	return(long.data.outfile)
}


wideToLongDF <- function() {
	args = commandArgs(trailingOnly=TRUE)
	wide.data.file <- args[1]
	var.colname <- args[2]
	val.colname <- args[3]
	long.data.outfile <- args[4]
	
	wide.data.df <- read.delim(wide.data.file, header = T, stringsAsFactors = F, check.names = F)
	print("Convert data from wide to long format...")
	datasets.ids.df <- formatWideDataToLong(wide.data.df,
											 long.data.outfile,
											 variable.name = var.colname,
											 value.name = val.colname)

	print("Done!")
	print(paste("Filename:", long.data.outfile))

}

wideToLongDF()
