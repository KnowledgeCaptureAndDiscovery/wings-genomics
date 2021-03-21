####
#title: "Calculate and plot unique, mapped and unmapped reads"
#author: "Ravali Adusumilli"
#date: "May 14, 2016"
####

library(reshape2) #Using melt from reshape2 for value.name, variable.name
library(easyGgplot2)

#####Reads information analysis functions:
# Calculate mapped,unmapped reads from the input bam file
# Process the file generated from bam_stats.py from RSeQC package:
processBamStatsFiles <- function(bam.rc.file,
                                 unmapped.bam.rc.file="",
                                 sample.id=basename(bam.rc.file)) {
  
  print(paste("bam_stats.py generated read file:", bam.rc.file))
  if(file.exists(bam.rc.file)) {
    bam.rc.file.basename <- basename(bam.rc.file)
    if(file.exists(unmapped.bam.rc.file)) {
      unmapped.bam.rc.file.basename <- basename(unmapped.bam.rc.file)
      # bamstats.colnames <- c("metrics_name","reads_count")
      mapped.bamstats.df <- read.delim(bam.rc.file, sep=":", header=F, 
                                       comment.char = "#", stringsAsFactors = FALSE)
      umapped.bamstats.df <- read.delim(unmapped.bam.rc.file, sep=":", header=F, 
                                        comment.char = "#", stringsAsFactors = FALSE)
      unmapped.reads.mapped.bam <- mapped.bamstats.df[mapped.bamstats.df[,1] == "Unmapped reads",2] # should always be zero
      mapped.unique.reads.mapped.bam <- mapped.bamstats.df[mapped.bamstats.df[,1] == "mapq >= mapq_cut (unique)",2]
      mapped.nonunique.reads.mapped.bam <- mapped.bamstats.df[mapped.bamstats.df[,1] == "mapq < mapq_cut (non-unique)",2]
      
      unmapped.reads.unmapped.bam <- umapped.bamstats.df[umapped.bamstats.df[,1] == "Unmapped reads",2]
      mapped.unique.reads.unmapped.bam <- umapped.bamstats.df[umapped.bamstats.df[,1] == "mapq >= mapq_cut (unique)",2] # should always be zero
      mapped.nonunique.reads.unmapped.bam <- umapped.bamstats.df[umapped.bamstats.df[,1] == "mapq < mapq_cut (non-unique)",2] # should always be zero
      
      unique.mapped.reads <- mapped.unique.reads.mapped.bam+mapped.unique.reads.unmapped.bam
      mapped.reads <- unique.mapped.reads+mapped.nonunique.reads.mapped.bam+mapped.nonunique.reads.unmapped.bam
      unmapped.reads <- unmapped.reads.mapped.bam+unmapped.reads.unmapped.bam
      total.reads <- mapped.reads+unmapped.reads
      filename.str <- paste(bam.rc.file.basename, unmapped.bam.rc.file.basename, sep=",")
      mapped.unmapped.bamstats.df <- data.frame(sample_id=sample.id,
                                                total_reads=total.reads,
                                                unique_mapped_reads=unique.mapped.reads,
                                                mapped_reads=mapped.reads,
                                                unmapped_reads=unmapped.reads,
                                                input_filename=filename.str,
                                                stringsAsFactors = FALSE)
      return(mapped.unmapped.bamstats.df)
    } else {
      bamstats.df <- read.delim(bam.rc.file, sep=":", header=F, 
                                                comment.char = "#", stringsAsFactors = FALSE)
      
      unmapped.reads <- bamstats.df[bamstats.df[,1] == "Unmapped reads",2]
      unique.mapped.reads <- bamstats.df[bamstats.df[,1] == "mapq >= mapq_cut (unique)",2]
      nonunique.mapped.reads <- bamstats.df[bamstats.df[,1] == "mapq < mapq_cut (non-unique)",2]
      mapped.reads <- unique.mapped.reads+nonunique.mapped.reads
      total.reads <- mapped.reads+unmapped.reads
      
      mapped.unmapped.bamstats.df <- data.frame(sample_id=sample.id,
                                                total_reads=total.reads,
                                                unique_mapped_reads=unique.mapped.reads,
                                                mapped_reads=mapped.reads,
                                                unmapped_reads=unmapped.reads,
                                                input_filename=basename(bam.rc.file.basename),
                                                stringsAsFactors = FALSE)
      return(mapped.unmapped.bamstats.df)
    }
  } else {
    print("Missing input bam file")
  }
}

# Wrapper for analyzing multiple sam/bam rc.files
analyzeSequencingReads <- function(sample.ids,
                                   bam.rc.files,
                                   unmapped.bam.rc.files="") {
  print(paste("Calculating mapped+unmapped reads"))
  filename.prefix <- "readMappingAllSamples"
  output.filename <- paste0(filename.prefix, ".txt")
  output.bplotname <- paste0(filename.prefix, "bplot.pdf")
  
  sample.info.df <- data.frame(sample_id=sample.ids,
                               bam_rc_file=bam.rc.files,
                               unmapped_bam_rc_file=unmapped.bam.rc.files,
                               stringsAsFactors = FALSE)
  # print(sample.info.df)
  reads.bamstats.list  <- lapply(1:nrow(sample.info.df), function(one.sample)
                                    processBamStatsFiles(
                                      bam.rc.file=sample.info.df[one.sample,2],
                                      unmapped.bam.rc.file=sample.info.df[one.sample,3],
                                      sample.id=sample.info.df[one.sample,1]))
  reads.bamstats.allsamples <- do.call(rbind, reads.bamstats.list)
  write.table(reads.bamstats.allsamples, output.filename,
              sep="\t", row.names = FALSE, quote = FALSE)
  seqreads.bamstats.bplot <- formatAndPlotSequencingReads(reads.bamstats.allsamples,
                                                          output.bplotname)
  return(list(output.filename, seqreads.bamstats.bplot))
}

formatAndPlotSequencingReads <- function(reads.bamstats.df,
                                         output.bplotname) {
  print(paste("Plotting mapped, uniquely mapped, total reads"))
  sample.id.colname <- colnames(reads.bamstats.df)[1]
  metric.colname <- "metrics"
  metric.count.colname <- "metrics_count"
  metric.groups.colname <- "metrics_grouped"
  adj.total.reads.colname <- paste0("adj_", colnames(reads.bamstats.df[2]))
  adj.mapped.reads.colname <- paste0("adj_", colnames(reads.bamstats.df[4]))
  reads.bamstats.df[,adj.total.reads.colname] <- reads.bamstats.df[,2]-reads.bamstats.df[,4]
  reads.bamstats.df[,adj.mapped.reads.colname] <- reads.bamstats.df[,4]-reads.bamstats.df[,3]
  
  # Arrange the barplots in descending order by sample_name from highest mapped-unmapped reads to lowest
  # Ordering after converting to long format is not working due to adjusted read counts etc.
  reads.bamstats.df[,sample.id.colname] <- factor(reads.bamstats.df[,sample.id.colname],
                                                  levels=reads.bamstats.df[order(reads.bamstats.df[,2],
                                                                                 decreasing = TRUE),
                                                                           sample.id.colname])
  
  long.reads.bamstats.df <- melt(reads.bamstats.df[,c(1,3,ncol(reads.bamstats.df),ncol(reads.bamstats.df)-1)],
                                 id.vars=sample.id.colname,
                                 variable.name=metric.colname,
                                 value.name = metric.count.colname) #reshape2
  long.reads.bamstats.df[,metric.groups.colname] <- ifelse(long.reads.bamstats.df[,metric.colname] ==
                                                             adj.total.reads.colname,
                                                           "Total reads", ifelse(long.reads.bamstats.df[,metric.colname] ==
                                                                                   adj.mapped.reads.colname,
                                                                                 "Mapped reads", "Uniquely mapped reads"))
  
  pdf(output.bplotname)
  seqreads.bamstats.bplot <- ggplot2.barplot(data=long.reads.bamstats.df,
                                             xName = sample.id.colname,
                                             yName = metric.count.colname,
                                             xtitle = "Samples",
                                             ytitle = "Number of reads",
                                             mainTitle = "Summary of total RNA-Seq read counts and \n mapping results for individual samples",
                                             #xtickLabelRotation = 90,
                                             groupName = metric.groups.colname,
                                             color="black",
                                             legendPosition="top",
                                             xTickLabelFont=c(6,"plain", "black"),
                                             yTickLabelFont=c(8,"plain", "black"))
  seqreads.bamstats.bplot <- seqreads.bamstats.bplot+ theme(axis.text.x=element_text(angle=90,
		            						                                         vjust=0.5,
					         		        	                                     hjust=1))
  seqreads.bamstats.bplot <- seqreads.bamstats.bplot+scale_fill_manual(
                                               breaks=long.reads.bamstats.df[,metric.groups.colname],
                                               values=c("salmon", "skyblue2", "darkred"))
  
  print(seqreads.bamstats.bplot)
  dev.off()
  return(output.bplotname)
}

calcAndPlotReadsFromBam <- function(sample.ids,
                                    bam.rc.files,
                                    unmapped.bam.rc.files) {
  # Calculate and plot Mapped Unmapped Reads: 
  mapped.unmapped.reads.file.bplot.list <- analyzeSequencingReads(sample.ids = sample.ids,
    		                                                 	  bam.rc.files = bam.rc.files,
	                                                              unmapped.bam.rc.files = unmapped.bam.rc.files)
  return(mapped.unmapped.reads.file.bplot.list)
}


#####Function to process commandline arguments for reordering and plotting sequencing reads from bam file
orderAndplotBamReads <- function() {
  args <- commandArgs(TRUE)
  
  # comma-separated ids, params and filenames?
  sample.ids.str <- args[1]
  bam.rc.files.str <- args[2]
  unmapped.bam.rc.files.str <- args[3] 
  
  sample.ids <- strsplit(sample.ids.str, ",")
  bam.rc.files <- strsplit(bam.rc.files.str, ",")
  unmapped.bam.rc.files <- strsplit(unmapped.bam.rc.files.str, ",")
  
  mapped.unmapped.reads.file.bplotname.list <- calcAndPlotReadsFromBam(sample.ids,
                                                                   bam.rc.files,
                                                                   unmapped.bam.rc.files)
  return(mapped.unmapped.reads.file.bplotname.list)
}

orderAndplotBamReads()
