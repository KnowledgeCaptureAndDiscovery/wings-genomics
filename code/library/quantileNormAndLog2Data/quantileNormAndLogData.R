library(limma)

# First column in the dataframe needs to be an identifier for protein/geneid
# Quantile normalize data
# Add 1 to the data to retain "zero values" and then log2 transform the data
# Output both wide and long format files

qlog2Transform <- function(input.df,
							sample.ids,
							geneid.colname,
                           	sample.colname,
                           	qlog2.colname,
                           	wide.qlog2.outfile,
                           	long.qlog2.outfile) {
	input.qnorm.df <- normalizeBetweenArrays(as.matrix(input.df[,colnames(input.df) %in% sample.ids]),
    	                                    method="quantile")
    	                                    
	wide.input.qlog2.df <- setNames(data.frame(input.df[,geneid.colname],
                                        log2(input.qnorm.df+1),
                                    stringsAsFactors = F),
                             c(geneid.colname, colnames(input.qnorm.df)))
	
	long.input.qlog2.df <- reshape2::melt(wide.input.qlog2.df,
										variable.name = sample.colname,
                                        value.name = qlog2.colname)

	write.table(wide.input.qlog2.df, wide.qlog2.outfile, row.names = F, quote = F, sep = "\t")
	write.table(long.input.qlog2.df, long.qlog2.outfile, row.names = F, quote = F, sep = "\t")
	
	return(list(wide.input.qlog2.df, long.input.qlog2.df))
}

quantileNormAndLogData <- function() {
	args = commandArgs(trailingOnly=TRUE)
	datasets.file <- args[1]
	sampleids.file <- args[2]
	geneid.colname <- args[3]
	sample.colname <- args[4]
	qlog2.colname <- args[5]
	wide.qlog2.outfile <- args[6]
	long.qlog2.outfile <- args[7]

	dataset.df <- read.delim(datasets.file, header = T, stringsAsFactors = F, check.names = F)
	sample.ids.df <- read.delim(sampleids.file, header = F, stringsAsFactors = F)
	sample.ids <- sample.ids.df[,1]
	
	wide.long.qlog2.df.list <- qlog2Transform(dataset.df,
														sample.ids, 
														geneid.colname,
														sample.colname,
														qlog2.colname,
														wide.qlog2.outfile,
														long.qlog2.outfile)
	 
	 print("Data quantile normalized and log2 transformed ---> Done")
	 print(paste("Filenames:", paste(wide.qlog2.outfile, long.qlog2.outfile, sep=",")))
}

quantileNormAndLogData()
