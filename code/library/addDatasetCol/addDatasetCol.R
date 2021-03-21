# Add Dataset Column name to input file

addDatasetCol <- function() {
	args = commandArgs(trailingOnly=TRUE)
	data.file <- args[1]
	dataset.colname <- args[2]
	dataset.value <- args[3]
	output.file <- args[4]

	data.df <- read.delim(data.file, header = T, stringsAsFactors = F, check.names = F)
	data.df[,dataset.colname] <- dataset.value

	write.table(data.df, output.file, row.names = F, sep = "\t", quote = F)

	print("Done!")
	print(paste("Filename:", output.file))
}

addDatasetCol()
