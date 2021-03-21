# Dataset needs to be processed through formatIDPQueryData script prior to this to get the long formatted dataframe.
# long.dataset.df is a dataframe where columns are sample IDs, gene/protein IDs, dataset name and expression values.
## Scripts multiple files (i.e. >=2 files)

getIDsFromDatasetsList <- function(long.dataset.dfs.list, 
									id.colname,
									datasets.name.list,
									datasets.ids.outfile) {
	datasets.ids.list <- lapply(long.dataset.dfs.list, function(x) unique(x[,id.colname]))
	datasets.ids.df <- do.call(cbind, datasets.ids.list)
	colnames(datasets.ids.df) <- datasets.name.list
	write.table(datasets.ids.df, datasets.ids.outfile, row.names = F, quote = F, sep = "\t")
	return(datasets.ids.df)
}

tabulateIDsFromMultFiles <- function() {
	args = commandArgs(trailingOnly=TRUE)
	long.dataset.files.str <- args[1] # >=2 files
	id.colname <- args[2]
	datasets.name.str <- args[3]
	datasets.ids.outfile <- args[4]

	long.dataset.files.list <- unlist(strsplit(long.dataset.files.str, ','))
	long.dataset.dfs.list <- lapply(long.dataset.files.list,
										function(x)
											read.delim(x, header = T, stringsAsFactors = F, check.names = F))
	datasets.name.list <- unlist(strsplit(datasets.name.str, ','))
	
	print("Generating table with ids from two datasets...")
	datasets.ids.df <- getIDsFromDatasetsList(long.dataset.dfs.list,
												id.colname,
												datasets.name.list,
												datasets.ids.outfile)
	print("Done!")
	print(paste("Filename:", datasets.ids.outfile))
}

tabulateIDsFromMultFiles()
