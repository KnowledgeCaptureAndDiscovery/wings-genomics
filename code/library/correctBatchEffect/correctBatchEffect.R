library(limma)

# wide.data.df is a matrix with IDs as row.names but NO identifier column.
# data.batches.df is a dataframe with first column as sample Id and second column as batch ID
correctBatchEffect <- function() {
	args = commandArgs(trailingOnly=TRUE)
	wide.data.file <- args[1]
	data.batches.file <- args[2]
	wide.data.nobatcheffect.outfile <- args[3]

	wide.data.df <- read.delim(wide.data.file, header = T, stringsAsFactors = F, check.names = F)
	data.batches.df <- read.delim(data.batches.file, header = T, stringsAsFactors = F, check.names = F)
    
    req.batch.ids <- colnames(wide.data.df) %in% data.batches.df[,1]
    wide.data.filt.df <- wide.data.df[,req.batch.ids]

	# Map column names from input fie to the batches ID order
	data.batches <- data.batches.df[match(colnames(wide.data.filt.df), data.batches.df[,1]),2]
	print("Removing batch effect from the input file based on the batches file...")

	wide.data.nobatcheffect.df <- removeBatchEffect(wide.data.filt.df, data.batches)
	wide.data.nobatcheffect.df <- setNames(data.frame(wide.data.df[,!req.batch.ids],
											 wide.data.nobatcheffect.df), colnames(wide.data.df))
	write.table(wide.data.nobatcheffect.df, wide.data.nobatcheffect.outfile,
				row.names = F, quote = F, sep = "\t")

	print("Done!")
	print(paste("Filename:", wide.data.nobatcheffect.outfile))
}

correctBatchEffect()
