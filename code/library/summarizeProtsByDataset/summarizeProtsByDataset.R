# Best suited for file with data from two datasets. >=2 dataset is processed but not detailed

countProtsPerDataset <- function(long.datasets.df,
									geneid.colname,
									sample.colname,
									dataset.colname,
									datasets.overlap.colname,
									sc.colname,
									genes.per.dataset.colname,
									groupby.cols.list,
									long.datasets.outfile,
									dataset.overlap.ids.outfile) {
	retain.colnames <- c(geneid.colname, sample.colname, dataset.colname)
	groupby.cols <- unlist(groupby.cols.list)
	groupby.cols.list.assymbols <- lapply(groupby.cols.list, function(x) as.symbol(x))
	dataset.names <- unique(long.datasets.df[,dataset.colname])
	long.datasets.df <- unique(long.datasets.df[long.datasets.df[,sc.colname] != 0,retain.colnames])
	
	dataset.genenames.list <- lapply(dataset.names, function(x)
										unique(long.datasets.df[long.datasets.df[,dataset.colname] 
																		 %in% x, geneid.colname]))
	dataset.overlap.genenames <- setNames(Reduce(intersect, dataset.genenames.list),geneid.colname)

	datasets.overlap.name <- paste(sort(dataset.names, decreasing = T), collapse="_")
	dataset.overlap.genenames.df <- setNames(data.frame(Reduce(intersect, dataset.genenames.list),
														datasets.overlap.name, stringsAsFactors = F),
											 c(geneid.colname, datasets.overlap.colname))

	long.datasets.overlapcoladded.df <- merge(long.datasets.df, dataset.overlap.genenames.df, all=T, incomparables = NA)
	long.datasets.overlapcoladded.df[,datasets.overlap.colname] <- ifelse(is.na(long.datasets.overlapcoladded.df[,datasets.overlap.colname]),
																			long.datasets.overlapcoladded.df[,dataset.colname],
																			long.datasets.overlapcoladded.df[,datasets.overlap.colname])

	unique.genes.count.formula <- as.formula(paste(geneid.colname, "~", paste(groupby.cols, collapse="+")))
	long.datasets.genescount.df <- setNames(aggregate(data=long.datasets.overlapcoladded.df,
												unique.genes.count.formula, function(x) length(unique(x))),
											c(groupby.cols, genes.per.dataset.colname))
	
	write.table(long.datasets.genescount.df, long.datasets.outfile, sep="\t", quote=F, row.names=F)
	write.table(dataset.overlap.genenames, dataset.overlap.ids.outfile, sep="\t", quote=F, row.names=F)
	return(list(long.datasets.genescount.df, dataset.overlap.genenames))
}

# Summarize proteins per dataset
summarizeProtsByDataset <- function() {
	args = commandArgs(trailingOnly=TRUE)
	long.datasets.file <- args[1]
	datasets.overlap.colname <- args[2]
	geneid.colname <- args[3]
	sample.colname <- args[4]
	dataset.colname <- args[5]
	sc.colname <- args[6]
	genes.per.dataset.colname <- args[7]
	groupby.cols.str <- args[8]
	long.datasets.outfile <- args[9]
	dataset.overlap.ids.outfile <- args[10]

	groupby.cols.list <- strsplit(groupby.cols.str, ',')
	long.datasets.df <- read.delim(long.datasets.file, header = T, stringsAsFactors = F, check.names = F)

	long.datasets.no0sc.df.overlap.genenames.list <- countProtsPerDataset(long.datasets.df,
													geneid.colname,
													sample.colname,
													dataset.colname,
													datasets.overlap.colname,
													sc.colname,
													genes.per.dataset.colname,
													groupby.cols.list,
													long.datasets.outfile,
													dataset.overlap.ids.outfile)
	 
	 print("Counting genes/proteins per dataset ---> Done.")
	 print(paste("Filename:", long.datasets.outfile))

  
}

summarizeProtsByDataset()
