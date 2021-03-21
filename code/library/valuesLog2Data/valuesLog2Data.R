
# First column in the dataframe needs to be an identifier for protein/geneid
# Log 2 transform the wide format data
# Add 1 to the data to retain "zero values" and then log2 transform the data
# Output both wide and long format files

log2TransformData <- function(input.df,
							sample.ids,
							geneid.colname,
                           	sample.colname,
                           	log2.colname,
                           	wide.log2.outfile,
                           	long.log2.outfile) {
	input.mtx <- as.matrix(input.df[,colnames(input.df) %in% sample.ids])
    	                                    
	wide.input.log2.df <- setNames(data.frame(input.df[,geneid.colname],
                                        log2(input.mtx+1),
                                    stringsAsFactors = F),
                             c(geneid.colname, colnames(input.mtx)))
	
	long.input.log2.df <- reshape2::melt(wide.input.log2.df,
										variable.name = sample.colname,
                                        value.name = log2.colname)

	write.table(wide.input.log2.df, wide.log2.outfile, row.names = F, quote = F, sep = "\t")
	write.table(long.input.log2.df, long.log2.outfile, row.names = F, quote = F, sep = "\t")
	
	return(list(wide.input.log2.df, long.input.log2.df))
}

valuesLog2Data <- function() {
	args = commandArgs(trailingOnly=TRUE)
	datasets.file <- args[1]
	sampleids.file <- args[2]
	geneid.colname <- args[3]
	sample.colname <- args[4]
	log2.colname <- args[5]
	wide.log2.outfile <- args[6]
	long.log2.outfile <- args[7]

	dataset.df <- read.delim(datasets.file, header = T, stringsAsFactors = F, check.names = F)
	sample.ids.df <- read.delim(sampleids.file, header = F, stringsAsFactors = F)
	sample.ids <- sample.ids.df[,1]
	
	wide.long.log2.df.list <- log2TransformData(dataset.df,
														sample.ids, 
														geneid.colname,
														sample.colname,
														log2.colname,
														wide.log2.outfile,
														long.log2.outfile)
	 
	 print("Data log2 transformed ---> Done")
	 print(paste("Filenames:", paste(wide.log2.outfile, long.log2.outfile, sep=",")))
}

valuesLog2Data()
