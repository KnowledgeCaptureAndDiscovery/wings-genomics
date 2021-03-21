# Expects a dataframe with rows as genes/proteins and samples as columns
# Has a header with samplenames as colnames and first column has gene/protein ids
normRSEMMatrix <- function(rsem.matrix.df, id.colname) {
  rsem.matrix.noIDs.df <- rsem.matrix.df[,2:ncol(rsem.matrix.df)]
  matrix.percentile75.noIDs.df <- apply(rsem.matrix.noIDs.df, 2,
                                        quantile, probs = 0.75,  na.rm = TRUE)
  rsem.matrix.norm.noIDs.df <- 1000*(t(t(rsem.matrix.noIDs.df)/matrix.percentile75.noIDs.df))
  rsem.matrix.norm.df <- data.frame(rsem.matrix.df[,1], rsem.matrix.norm.noIDs.df)
  colnames(rsem.matrix.norm.df) <- colnames(rsem.matrix.df)
  colnames(rsem.matrix.norm.df)[1] <- id.colname
  return(rsem.matrix.norm.df)
}


perfromRSEMNorm <- function() {
  args <- commandArgs(TRUE)
  rsem.matrix.file <- args[1]
  rsem.matrix.norm.file <- args[2]
  id.colname <- args[3]
  
  rsem.matrix.df <- read.delim(rsem.matrix.file, header = T,
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
  rsem.matrix.norm.df <- normRSEMMatrix(rsem.matrix.df, id.colname)
  write.table(rsem.matrix.norm.df, rsem.matrix.norm.file,
              sep = "\t", quote = FALSE,
              row.names = FALSE)
  return(rsem.matrix.norm.file)
}


perfromRSEMNorm()
