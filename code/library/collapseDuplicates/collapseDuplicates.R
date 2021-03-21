# Input data file (wide.data.df) is a matrix with rownames as IDs but no first ID column
# counttable.data.df is a matrix with rownames as IDs, spectral counts as values but no first ID column
# sample.ann.df is a dataframe with first column as sample IDs and second column as replicate number, 0 if no replicates present.
retainRepMaxSampleCount <- function(wide.data.df, counttable.data.df, sample.ann.df, data.nodup.outfile) {
	sample_count = apply(counttable.data.df, 2, sum)
	rep = sample.ann.df[,2]
	rep_count = cbind(rep,sample_count)
	rownames(rep_count) = sample.ann.df[,1]

	########  For replicates, samples with larger count is kept
	rm_sample_vec = vector()
	for (i in 1:length(unique(rep[rep!=0]))) {
		rep_matrix = rep_count[rep_count[,1]==i,]	
		samples = rownames(rep_matrix)
		rm_sample = samples[rep_matrix[,2]==min(rep_matrix[,2])]
		rm_sample_vec = c(rm_sample_vec,rm_sample)
	}

	samples.nodup <- setdiff(colnames(wide.data.df),rm_sample_vec)
	wide.data.nodup.df <- wide.data.df[,colnames(wide.data.df) %in% samples.nodup]
	write.table(wide.data.nodup.df, data.nodup.outfile, row.names = T, quote = F, sep = "\t")
	return(list(wide.data.nodup.df, samples.nodup))
}

# Filter wide.data.file based on the counttable.data.file
collapseDuplicates <- function() {
	args = commandArgs(trailingOnly=TRUE)
	wide.data.file <- args[1]
	counttable.data.file <- args[2]
	sample.ann.file <- args[3]
	data.nodup.outfile <- args[4]

	wide.data.df <- read.delim(wide.data.file, header = T, stringsAsFactors = F, check.names = F)
	counttable.data.df <- read.delim(counttable.data.file, header = T, stringsAsFactors = F, check.names = F)
	sample.ann.df <- read.delim(sample.ann.file, header = T, stringsAsFactors = F, check.names = F)
	sample.ann.df <- sample.ann.df[sample.ann.df[,1] %in% colnames(counttable.data.df),]

	print("For replicates, retaining sample data with larger count...")
	wide.data.nodup.df.samples.nodup.list <- retainRepMaxSampleCount(wide.data.df,
																		counttable.data.df,
																		sample.ann.df,
																		data.nodup.outfile)
	print("Done!")
	print(paste("Filename:", data.nodup.outfile))
}

collapseDuplicates()
