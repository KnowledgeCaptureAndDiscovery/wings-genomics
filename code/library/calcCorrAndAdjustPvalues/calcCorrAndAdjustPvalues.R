library(reshape2)
calcCorrByID <- function(dataset1.df,
                         dataset2.df,
                         dataset1.name,
                         dataset2.name,
                         calc.corrby.id,
                         corr.method.name,
                         padj.method.name,
                         corr.value.colname,
                         pvalue.colname,
                         padj.colname,
                         dataset1.2.corr.outfilename) {  
  dataset1.dataset2.df <- merge(dataset1.df, dataset2.df)
  dataset1.2.corr.pvalue.byid <- as.data.frame(do.call(rbind, lapply(unique(dataset1.dataset2.df[,calc.corrby.id]),
                                                                      function(x)
                                                                        c(as.character(x), unlist(cor.test(dataset1.dataset2.df[dataset1.dataset2.df[,calc.corrby.id]==x,dataset1.name],
                                                                                                                     dataset1.dataset2.df[dataset1.dataset2.df[,calc.corrby.id]==x,dataset2.name],
                                                                                                                     method = corr.method.name)[c("estimate", "p.value")])) )))
  colnames(dataset1.2.corr.pvalue.byid) <- c(calc.corrby.id, "estimate", "p.value")
  dataset1.2.corr.pvalue.byid[,"estimate"] <- as.numeric(as.character(dataset1.2.corr.pvalue.byid[,"estimate"]))
  dataset1.2.corr.pvalue.byid[,"p.value"] <- as.numeric(as.character(dataset1.2.corr.pvalue.byid[,"p.value"]))
  dataset1.2.corr.pvalue.byid[,"padj.value"] <- p.adjust(dataset1.2.corr.pvalue.byid[,"p.value"], method = padj.method.name)

  colnames(dataset1.2.corr.pvalue.byid) <- c(calc.corrby.id, corr.value.colname, pvalue.colname, padj.colname)

  write.table(dataset1.2.corr.pvalue.byid, dataset1.2.corr.outfilename, sep="\t", row.names = F, quote = F)
  return(dataset1.2.corr.pvalue.byid)
}

reformatWideToLongData <- function(wide.dataset.df,
                                   dataset.colname,
                                   sample.colname) {
  long.dataset.df <- melt(wide.dataset.df,
                          value.name = dataset.colname,
                          variable.name = sample.colname)
  return(long.dataset.df)
}

readAndFormatData <- function(dataset.file, dataset.colname, sample.colname) {
  dataset.df <- read.delim(dataset.file, header = T,
                          stringsAsFactors = FALSE,
                           check.names = FALSE)
  long.dataset.df <- reformatWideToLongData(dataset.df,
                                            dataset.colname,
                                            sample.colname)
  return(long.dataset.df)
}

# Dataset files 1 and 2 should have the following matrix format:
# gene/protein ids in first column and values per sample in columns
# headers: first column header should be the gene/protein ID followed by sampleIDs
calcCorrAndPvalues <- function() {
  args = commandArgs(trailingOnly=TRUE)
  dataset1.file <- args[1]
  dataset2.file <- args[2]
  dataset1.name <- args[3]
  dataset2.name <- args[4]
  sample.colname <- args[5]
  calc.corrby.id <- args[6]
  corr.method.name <- args[7]
  padj.method.name <- args[8]
  corr.value.colname <- args[9]
  pvalue.colname <- args[10]
  padj.colname <- args[11]
  dataset1.2.corr.outfilename <- args[12]
  
  long.dataset1.df <- readAndFormatData(dataset1.file,
                                        dataset1.name,
                                        sample.colname)
  long.dataset2.df <- readAndFormatData(dataset2.file,
                                        dataset2.name,
                                        sample.colname)
  print("Calculate correlations between two datasets ...")
  corr.pvalue.df <- calcCorrByID(long.dataset1.df,
                                 long.dataset2.df,
                                 dataset1.name,
                                 dataset2.name,
                                 calc.corrby.id,
                                 corr.method.name,
                                 padj.method.name,
                		         corr.value.colname,
        		                 pvalue.colname,
		                         padj.colname,
                                 dataset1.2.corr.outfilename)
  
  print("Done")
  print(paste("Filename:", dataset1.2.corr.outfilename))
}

calcCorrAndPvalues()



