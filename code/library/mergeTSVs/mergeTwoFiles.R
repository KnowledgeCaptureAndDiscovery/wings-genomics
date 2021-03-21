

mergeTwoFiles <- function() {
	args = commandArgs(trailingOnly=TRUE)
	data1.file <- args[1]
	data2.file <- args[2]
	mergeby.colname <- args[3]
	merged.data1.2.outfile <- args[4]

	data1.df <- read.delim(data1.file, header = T, stringsAsFactors = F, check.names = F)
	data2.df <- read.delim(data2.file, header = T, stringsAsFactors = F, check.names = F)
	
	print(paste("Merging files", data1.file,"and", data2.file, "..."))
	merged.data1.2.df <- merge(data1.df[,1:2], data2.df[,1:2], by=mergeby.colname)

	write.table(merged.data1.2.df, merged.data1.2.outfile, row.names = F, quote = F, sep = "\t")

	print("Done!")
	print(paste("Filename:", merged.data1.2.outfile))
}

mergeTwoFiles()
