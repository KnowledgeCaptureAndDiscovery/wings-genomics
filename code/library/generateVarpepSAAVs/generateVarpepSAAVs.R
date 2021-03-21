# Format and generate variant peptide and SAAV data
formatVarpepAndSAAVs <- function(var.pep.df, saav.colname,
						  pro.colname, var.colname,
						  dbsnp.colname, pep.format.colname,
						  pep.seq.colname, to.remove.colnames,
						  group.by.cols) {
	var.pep.df[,saav.colname] <- do.call(paste, c(var.pep.df[c(pro.colname, var.colname, dbsnp.colname)], sep=":"))
	var.pep.df <- var.pep.df[,c(saav.colname, 
								colnames(var.pep.df)[!colnames(var.pep.df)
													%in%
                                                  saav.colname])]
	colnames(var.pep.df)[colnames(var.pep.df) == pep.format.colname] <- pep.seq.colname
	write.table(var.pep.df, var.pep.outfile, row.names = F, sep = "\t", quote = F)

	## SAAV level table
	group.by.cols.df <- var.pep.df[group.by.cols]

	var.saav.df <- aggregate(x = var.pep.df[,!colnames(var.pep.df)
										%in% c(to.remove.colnames,
												group.by.cols)],
							 by = group.by.cols.df,
        	                 FUN = sum)
	write.table(var.saav.df, var.saav.outfile, row.names = F, sep = "\t", quote = F)
	return(list(var.pep.df, var.saav.df))
}

generateVarpepSAAVs <- function() {
	args = commandArgs(trailingOnly=TRUE)
	var.pep.file <- args[1]
	pep.format.colname <- args[2]
	pro.colname <- args[3]
	dbsnp.colname <- args[4]
	var.colname <- args[5]
	saav.colname <- args[6]
	pep.seq.colname <- args[7]
	var.pep.outfile <- args[8]
	var.saav.outfile <- args[9]

	var.pep.df <- read.delim(var.pep.file, header = T, stringsAsFactors = F, check.names = F)
	to.remove.colnames <- unlist(strsplit(to.remove.cols.str, ","))
	group.by.cols <- unlist(strsplit(group.by.cols.str, ","))

	print("Format the input variant peptide table to generate SAAV and variant peptide tables...")
	var.pep.saav.df.list <- formatVarpepAndSAAVs(var.pep.df, saav.colname,
												  pro.colname, var.colname,
												  dbsnp.colname, pep.format.colname,
												  pep.seq.colname, to.remove.colnames,
												  group.by.cols)
	print("Done!")
	print(paste("Filenames:", var.saav.outfile, "and", var.pep.outfile))
}

generateVarpepSAAVs()
