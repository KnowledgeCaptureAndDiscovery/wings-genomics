library(stringr)
args = commandArgs(trailingOnly=TRUE)
toedit.file <- args[1]
infileids.file <- args[2]
sampleids.file <- args[3]

infileid.colname <- "input_file"
sampleid.colname <- "sample_id"
toedit.sampleid.colname <- "toedit_sample_id"

toedit.df <- read.delim(toedit.file, header = T,
                      stringsAsFactors = F, check.names = F)
infileids.df <- read.delim(infileids.file, header = F,
                     stringsAsFactors = F, check.names = F)
sample.df <- read.delim(sampleids.file, header = F,
                        stringsAsFactors = F, check.names = F)

infileid.sample.map.df <- setNames(data.frame(infileids.df, sample.df),
                              c(infileid.colname, sampleid.colname))
toedit.colnames <- colnames(toedit.df)
toedit.colnames.edited <- str_replace_all(toedit.colnames[2:length(toedit.colnames)],
                     paste0(".*",infileid.sample.map.df[,1]),
                infileid.sample.map.df[,2])
colnames(toedit.df)[2:length(colnames(toedit.df))] <- toedit.colnames.edited
write.table(toedit.df, "fileNameToSampleIDs.txt",
            sep="\t", row.names=F, quote = F)
