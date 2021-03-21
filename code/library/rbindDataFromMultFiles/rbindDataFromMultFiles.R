# Dataset needs to be processed through formatIDPQueryData script prior to this to get the long formatted dataframe.
# long.dataset.df.list is a list of dataframes with same format -->
# where columns are sample IDs, gene/protein IDs, dataset name and expression values.
## Scripts multiple files (i.e. >=2 files)

rbindFromDatasetsList <- function(long.dataset.dfs.list,
									long.alldatasets.outfile) {
	long.alldatasets.df <- do.call(rbind, long.dataset.dfs.list)
	write.table(long.alldatasets.df, long.alldatasets.outfile, row.names = F, quote = F, sep = "\t")
	return(long.alldatasets.df)
}

rbindDataFromMultFiles <- function() {
	args = commandArgs(trailingOnly=TRUE)
	long.dataset.files.str <- args[1] # >=2 files
	long.alldatasets.outfile <- args[2]

	long.dataset.files.list <- unlist(strsplit(long.dataset.files.str, ','))
	long.dataset.dfs.list <- lapply(long.dataset.files.list,
										function(x)
											read.delim(x, header = T, stringsAsFactors = F, check.names = F))
	
	print("Generating table with ids from two datasets...")
	long.alldatasets.df <- rbindFromDatasetsList(long.dataset.dfs.list, long.alldatasets.outfile)
	print("Done!")
	print(paste("Filename:", long.alldatasets.outfile))
}

rbindDataFromMultFiles()
