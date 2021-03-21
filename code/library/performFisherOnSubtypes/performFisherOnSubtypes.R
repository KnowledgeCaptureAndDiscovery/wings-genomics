

getTwoSubtypeSampleCount <- function(core.samples.ann.df,
									 subtype1.colname, 
									 subtype1.name,
									 subtype2.colname,
									 subtype2.name) {
	subtype12.sample.count <- nrow(core.samples.ann.df[which(core.samples.ann.df[,subtype1.colname] == subtype1.name &
                                   		core.samples.ann.df[,subtype2.colname] == subtype2.name),])
	return(subtype12.sample.count)
}

generate2by2ContingencyMtx <- function(all.samples.count,
									   subtype1.sample.count,
									   subtype12.sample.count,
									   subtype1.name, subtype2.name) {
	subtypes.contingency.mtx <- cbind(rbind(subtype1.sample.count,
											all.samples.count-subtype1.sample.count,
											deparse.level = 0),
            	                      rbind(subtype12.sample.count,
        	                                 subtype1.sample.count-subtype12.sample.count,
        	                                 deparse.level = 0),
        	                          deparse.level = 0)
	colnames(subtypes.contingency.mtx) <- c(subtype1.name, subtype2.name)
	return(subtypes.contingency.mtx)
}

performFisherExactTest <- function(contingency.mtx, ...) {
	fishertest.results.obj <- fisher.test(contingency.mtx, ...)
	return(fishertest.results.obj)
}

getPvalueFromFisherResults <- function(fishertest.results.obj) {
	fishertest.pvalue <- fishertest.results.obj["p.value"]
	return(fishertest.pvalue)
}

subtypeAssocFisherResults <- function(contingency.mtx, 
									  subtype1.colname,
									  subtype2.colname,
									  pvalue.colname, ...) {
	fishertest.results.obj <- performFisherExactTest(contingency.mtx, ...)
	fishertest.pvalue <- getPvalueFromFisherResults(fishertest.results.obj)
	fishertest.vars.mtx <- rbind(colnames(contingency.mtx))
	colnames(fishertest.vars.mtx) <- c(subtype1.colname, subtype2.colname)
	subtypes.fisherpvalue.df <- setNames(data.frame(fishertest.vars.mtx, 
											fishertest.pvalue),
									c(colnames(fishertest.vars.mtx), pvalue.colname))
	return(subtypes.fisherpvalue.df)
}

editDFColnames <- function(input.df,
						   subtype1.colname,
						   subtype2.colname,
						   subtype2.out.colname,
						   class.subtype2.out.colname,
						   pvalue.colname) {
	input.coledit.df <- setNames(cbind(subtype2.colname, input.df), 
				  				 c(subtype2.out.colname, subtype1.colname,
			                       class.subtype2.out.colname, pvalue.colname));
	input.coledit.df <- input.coledit.df[,c(subtype1.colname,
											subtype2.out.colname,
											class.subtype2.out.colname,
											pvalue.colname)]
	return(input.coledit.df)
}

performPairwiseFisherTest <- function(core.samples.ann.df,
										   subtype1.colname,
										   subtype2.colname,
										   subtype2.out.colname,
										   class.subtype2.out.colname,
										   pvalue.colname) {
	subtypes.contingency.mtx.list <- list()
	all.subtypes1 <- unique(core.samples.ann.df[,subtype1.colname])
	all.subtypes2 <- unique(core.samples.ann.df[!is.na(core.samples.ann.df[,subtype2.colname]),
												subtype2.colname])
	subtypes.contingency.mtx.list.index <- 1
	for(subtype1.index in 1:length(all.subtypes1)) {
		one.subtype1 <- all.subtypes1[subtype1.index]
		for(subtype2.index in 1:length(all.subtypes2)) {
			one.subtype2 <- all.subtypes2[subtype2.index]
			subtype1.sample.count <- nrow(core.samples.ann.df[which(core.samples.ann.df[,subtype1.colname] == one.subtype1),])
			all.samples.count <- nrow(core.samples.ann.df)
			subtype12.sample.count <- getTwoSubtypeSampleCount(core.samples.ann.df,
																subtype1.colname,
																one.subtype1,
																subtype2.colname,
																one.subtype2)
			subtypes.contingency.mtx <- generate2by2ContingencyMtx(all.samples.count,
									   								subtype1.sample.count,
																   subtype12.sample.count,
																   one.subtype1,
																   one.subtype2)
			subtypes.contingency.mtx.list[[subtypes.contingency.mtx.list.index]] <- subtypes.contingency.mtx
			subtypes.contingency.mtx.list.index <- subtypes.contingency.mtx.list.index+1
		}
	}
	subtypes.fisherpvalue.df.list <- lapply(subtypes.contingency.mtx.list,
											function(x)
												subtypeAssocFisherResults(x, subtype1.colname,
																		  subtype2.colname,
																		  pvalue.colname))
	subtypes.fisherpvalues.df <- do.call(rbind, subtypes.fisherpvalue.df.list)
	subtypes.fisherpvalues.coledit.df <- editDFColnames(subtypes.fisherpvalues.df,
														subtype1.colname,
														subtype2.colname,
														subtype2.out.colname, 
														class.subtype2.out.colname,
														pvalue.colname)
	return(subtypes.fisherpvalues.coledit.df)
}

performPairwiseFisherTestOnAllSubtypes <- function(core.samples.ann.df,
													subtype1.colname,
	                                                subtype2.out.colname,
    	                                            class.subtype2.out.colname,
													all.st.colnames,
													pvalue.colname,
													padj.method,
													padj.colname) {
	all.subtypes.fisherpvalues.df.list <- lapply(all.st.colnames, 
												 function(x) 
													performPairwiseFisherTest(core.samples.ann.df,
                                                         	              	  subtype1.colname, x,
	                                                                	      subtype2.out.colname,
    	                                                            	      class.subtype2.out.colname,
        	                                                        	      pvalue.colname))

	all.subtypes.fisherpvalues.df <- do.call(rbind, all.subtypes.fisherpvalues.df.list)
	all.subtypes.fisherpvalues.df[,padj.colname] <- p.adjust(all.subtypes.fisherpvalues.df[,pvalue.colname],
																	  method=padj.method)

	return(all.subtypes.fisherpvalues.df)
}

# The Fisher's Exact test is performed on all the subtypes against the subtype1
performFisherOnSubtypes <- function() {
	args = commandArgs(trailingOnly=TRUE)
	core.samples.ann.file <- args[1]
	subtype1.colname <- args[2]
	all.st.colnames.str <- args[3]
	subtype2.out.colname <-  args[4]
	class.subtype2.out.colname <-  args[5]
	padj.method <- args[6]
	pvalue.colname <- args[7]
	padj.colname <- args[8]
	subtypes.fisherpvalues.outfile <- args[9]

	core.samples.ann.df <- read.delim(core.samples.ann.file, header = T,
									  stringsAsFactors = F, check.names = F)
	
	all.st.colnames <- unlist(strsplit(all.st.colnames.str, ","))
	print("Performing Fisher's Exact test and extracting p-values...")

	all.subtypes.fisherpvalues.df <- performPairwiseFisherTestOnAllSubtypes(core.samples.ann.df,
													subtype1.colname,
	                                                subtype2.out.colname,
    	                                            class.subtype2.out.colname,
													all.st.colnames,
													pvalue.colname,
													padj.method,
													padj.colname)
	write.table(all.subtypes.fisherpvalues.df, subtypes.fisherpvalues.outfile,
				row.names = F, quote = F, sep = "\t")
	print("Done!")
	print(paste("Filename:", subtypes.fisherpvalues.outfile))
}

performFisherOnSubtypes()


