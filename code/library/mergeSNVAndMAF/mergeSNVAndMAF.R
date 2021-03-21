
joinSNVMAFByGeneAndSamples <- function(snv.df, maf.df,
									   sample.colname,
									   geneid.colname,
									   pro.colname,
									   aamod.colname,
									   aachange.colname,
									   tcga.som.colname,
									   snv.maf.outfile) {
	maf.df[,aamod.colname] <-  gsub("p\\.","", maf.df[,aachange.colname])
	maf.sample.ids <- unique(maf.df[,sample.colname])
	snv.df[,sample.colname] <- gsub(paste0("(.*)(",
                                           paste(maf.sample.ids, collapse = "|"),
                                           ")(.*)"),
                                    "\\2",
                                    snv.df[,sample.colname])

	long.snv.maf.df <- merge(unique(snv.df[,colnames(snv.df) 
    							                      %in%
							                c(geneid.colname,
							                  pro.colname,
							                  sample.colname,
							                  aamod.colname)]),
		                     unique(maf.df[,colnames(maf.df)
                                                  %in%
                                            c(geneid.colname,
        					                  sample.colname,
        					                  tcga.som.colname)]),
		                     by = c(geneid.colname, sample.colname),
		                     all.x = T)

	write.table(long.snv.maf.df, snv.maf.outfile,
				row.names = F, quote = F, sep = "\t")
	return(long.snv.maf.df)
}


mergeSNVAndMAF <- function() {
	args = commandArgs(trailingOnly=TRUE)
	snv.file <- args[1]
	maf.file <- args[2]
	sample.colname <- args[3]
	geneid.colname <- args[4]
	pro.colname <- args[5]
	aamod.colname <- args[6]
	aachange.colname <- args[7]
	tcga.som.colname <- args[8]
	snv.maf.outfile <- args[9]
	
	snv.df <- read.delim(snv.file, header = T,
						 stringsAsFactors = F, check.names = F)
	maf.df <- read.delim(maf.file, header = T,
						 stringsAsFactors = F, check.names = F)

	print("Join the MAF and SNV tables by gene ids and sample names...")	
	long.snv.maf.df <- joinSNVMAFByGeneAndSamples(snv.df, maf.df,
											      sample.colname,
											      geneid.colname,
											      pro.colname,
											      aamod.colname,
											      aachange.colname,
											      tcga.som.colname,
											      snv.maf.outfile)
	print("Done!")
	print(paste("Filename:", snv.maf.outfile))

}

mergeSNVAndMAF()
