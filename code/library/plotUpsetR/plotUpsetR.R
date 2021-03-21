library(UpSetR)
library(reshape2)
# Format the data for the UpSet plot:
formatDataForUpSet <- function(long.data.df,
								dataset.colname,
								geneid.colname,
								upset.data.colname,
								upset.format.outfile,
								upset.pdffile) {
	long.data.df[,upset.data.colname] <- 1
	upset.format.df <- dcast(unique(long.data.df[,c(geneid.colname,
                	                                                 dataset.colname,
            	                                                     upset.data.colname)]),
        	                      formula = as.formula(paste(geneid.colname, "~", dataset.colname)),
    	                          value.var = upset.data.colname, fill = 0)

	write.table(upset.format.df, upset.format.outfile, sep="\t", row.names = F, quote = F)
	
	pdf(upset.pdffile, width=10, height=5.2, onefile = FALSE)
	upset.plot <- upset(upset.format.df, order.by = "degree", keep.order = T,
    	  	text.scale = 1.6,
	      sets.x.label = "Proteins per group",
    	  mainbar.y.label = "Protein Intersections")
	print(upset.plot)
	dev.off()
	return(upset.format.df)
}

plotUpsetR <- function() {
	args = commandArgs(trailingOnly=TRUE)
	long.data.file <- args[1]
	dataset.colname <- args[2]
	geneid.colname <- args[3]
	upset.data.colname <- args[4]
	upset.format.outfile <- args[5]
	upset.pdffile <- args[6]

	long.data.df <- read.delim(long.data.file, header = T, stringsAsFactors = F, check.names = F)

	print("Formatting data and plotting UpSet...")	
	upset.format.df <- formatDataForUpSet(long.data.df,
										  dataset.colname,
										  geneid.colname,
										  upset.data.colname,
										  upset.format.outfile,
										  upset.pdffile)
	print("Done!")
	print(paste("Filenames:", upset.format.outfile, "and", upset.pdffile))
}

plotUpsetR()
