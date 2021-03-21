
# Input data is a dataframe with first column as IDs (usually genes/proteins)
# Add IDs as row.names and remove the ID column
# Output data file is a matrix with rownames as IDs but no first ID column
convertDFtoMtx <- function(wide.data.df, col.ids, dataid.colname, filt.data.outfile) {
	row.names(wide.data.df) <- wide.data.df[,dataid.colname]
	wide.data.filt.df <- wide.data.df[,colnames(wide.data.df) %in% col.ids]
	write.table(wide.data.filt.df, filt.data.outfile, row.names = T, quote = F, sep = "\t")
	return(wide.data.filt.df)
}

dataframetoMatrix <- function() {
	args = commandArgs(trailingOnly=TRUE)
	wide.data.file <- args[1]
	col.ids.file <- args[2]
	dataid.colname <- args[3]
	filt.data.outfile <- args[4]

	wide.data.df <- read.delim(wide.data.file, header = T, stringsAsFactors = F, check.names = F)
	col.ids.df <- read.delim(col.ids.file, header = F, stringsAsFactors = F)
	col.ids <- col.ids.df[,1]

	print("Add as rownames and remove IDs column ...")
	wide.data.filt.df <- convertDFtoMtx(wide.data.df, col.ids, dataid.colname, filt.data.outfile)
	print("Done!")
	print(paste("Filenames", filt.data.outfile))
}

dataframetoMatrix()
