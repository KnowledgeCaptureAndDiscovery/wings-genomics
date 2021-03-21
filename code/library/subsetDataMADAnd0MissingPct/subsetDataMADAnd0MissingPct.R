# Identify rows with non-zero values in less than the missing percentage value
# Input data file is a matrix with rownames as IDs but no first ID column
filtDataByMADAndMissingPct <- function(wide.data.df, counttable.data.df, missing.pct,
										mad.cutoff, filt.data.outfile) {
	counttable.data.filt.df = counttable.data.df[rownames(wide.data.df),]
	count_0_vect = vector()
	for (j in 1:nrow(counttable.data.filt.df)) {	
		count_0 = sum(counttable.data.filt.df[j,]==0)
		count_0_vect = c(count_0_vect,count_0)
	}
	
	wide.data.scmads <- apply(wide.data.df, 1, mad)
	wide.data.filt.df <- wide.data.df[wide.data.scmads > mad.cutoff & count_0_vect < (ncol(wide.data.df)*missing.pct),]
	write.table(wide.data.filt.df, filt.data.outfile, row.names = T, quote = F, sep = "\t")
	return(wide.data.filt.df)
}

subsetDataBelowMissingPct <- function() {
	args = commandArgs(trailingOnly=TRUE)
	wide.data.file <- args[1]
	counttable.data.file <- args[2]
	missing.pct <- as.numeric(as.character(args[3]))
	mad.cutoff <- as.numeric(as.character(args[4]))
	filt.data.outfile <- args[5]

	wide.data.df <- read.delim(wide.data.file, header = T, stringsAsFactors = F, check.names = F)
	counttable.data.df <- read.delim(counttable.data.file, header = T, stringsAsFactors = F, check.names = F)

	print("Get rows with MAD above cutoff and non-zero values below the given missing percentage value...")
	wide.data.filt.df <- filtDataByMADAndMissingPct(wide.data.df, counttable.data.df,
													missing.pct, mad.cutoff, filt.data.outfile)
	print("Done!")
	print(paste("Filename:", filt.data.outfile))
}

subsetDataBelowMissingPct()
