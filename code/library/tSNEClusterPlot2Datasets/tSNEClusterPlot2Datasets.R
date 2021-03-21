library(Rtsne)
library(ggplot2)
library(easyGgplot2)

performTSNEClustering <- function(data.file, sample2cluster.file,
								  mark.sample.ids,
								  dataset.colname,
								  sample.colname,
								  prot.st.colname,
								  prot.new.st.colname,
								  tsne.plot.title) {

	data.df <- read.delim(data.file, header = T, check.names = F,
						  stringsAsFactors = F, row.names = NULL)
	data.df <- data.df[,2:ncol(data.df)]
	sample2clu.df <- read.delim(sample2cluster.file, header = T,
								check.names = F, stringsAsFactors = F)

	t.data.mtx <- t(data.df)
	tsne_model_t.data.mtx = Rtsne(t.data.mtx, check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
	d_tsne_t.data.mtx = as.data.frame(tsne_model_t.data.mtx$Y)
  
	sample2clu.df[sample2clu.df[, sample.colname] %in% mark.sample.ids, prot.new.st.colname] <- "New"
	sample2clu.df[,prot.new.st.colname] <- ifelse(sample2clu.df[,prot.new.st.colname] == "New",
  												"New", 1)

	## Get the protein subtypes
	d_tsne_t.data.mtx[, prot.st.colname] = factor(sample2clu.df[, prot.st.colname])
	d_tsne_t.data.mtx[, prot.new.st.colname] = factor(sample2clu.df[, prot.new.st.colname])

	d_tsne_t.data.mtx[,dataset.colname] <- tsne.plot.title

	return(d_tsne_t.data.mtx)
}

# Datasets faceted plot colored by subtypes
plotTSNEData <- function(d_tsne1.2,
						 dataset.colname,
						 prot.st.colname,
						 prot.new.st.colname,
						 legend.title,
						 tSNECluster.pdffile) {
	d_tsne1.2[,dataset.colname] <- factor(d_tsne1.2[,dataset.colname],
                                          levels=sort(unique(d_tsne1.2[,dataset.colname]),
                                                      decreasing = T))

	panelborder.theme <- theme(panel.border = element_rect(linetype = "solid", colour = "black", fill="NA", size=3))
	panelbg.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

	pdf(tSNECluster.pdffile, width=10, height=4)
	tSNECluster.splot <- ggplot2.scatterplot(d_tsne1.2, 
                xName = "V1",
                yName = "V2",
                xtitle="",
                ytitle="",
                groupName = prot.st.colname,
                legendPosition="bottom",
                legendTitle = legend.title,
                legendTitleFont=c(15, "bold", "black"),
                legendTextFont=c(15, "bold", "black"),
                faceting = TRUE,
                facetingFont=c(20, 'bold', "black"),
                facetingDirection="horizontal",
                facetingRect=list(background="white"),
                facetingVarNames = dataset.colname)
	tSNECluster.splot <- tSNECluster.splot + geom_point(mapping=aes_string(shape = prot.new.st.colname),
	                                            size = 3)
	tSNECluster.splot <- tSNECluster.splot + guides(shape = FALSE)
	tSNECluster.splot <- tSNECluster.splot + panelborder.theme + panelbg.theme
	print(tSNECluster.splot)
	dev.off()
	return(d_tsne1.2)
}

performTSNEAndPlot <- function(data1.file, sample2cluster1.file,
							   data2.file, sample2cluster2.file,
							   mark.sample.ids,
							   dataset.colname,
							   sample.colname,
							   prot.st.colname,
							   prot.new.st.colname,
							   legend.title,
							   tSNECluster.pdffile,
							   tsne1.2.outfile) {

	tsne1.plot.title <- "Before Batch Effect Correction"
	tsne2.plot.title <- "After Batch Effect Correction"
	d_tsne1 <- performTSNEClustering(data1.file, sample2cluster1.file,
								  	 mark.sample.ids,
								  	 dataset.colname,
								  	 sample.colname,
								  	 prot.st.colname,
								  	 prot.new.st.colname,
								  	 tsne1.plot.title)

	d_tsne2 <- performTSNEClustering(data2.file, sample2cluster2.file,
								  	 mark.sample.ids,
								  	 dataset.colname,
								  	 sample.colname,
								  	 prot.st.colname,
								  	 prot.new.st.colname,
								  	 tsne2.plot.title)
	d_tsne1.2 <- rbind(d_tsne1, d_tsne2)
	
	write.table(d_tsne1.2, tsne1.2.outfile, row.names = F, quote = F, sep = "\t")
	
	d_tsne1.2 <- plotTSNEData(d_tsne1.2,
						 		  	  dataset.colname,
								  	  prot.st.colname,
								  	  prot.new.st.colname,
									  legend.title,
									  tSNECluster.pdffile)

	return(d_tsne1.2)
}

tSNEClusterPlot2Datasets <- function() {
	args = commandArgs(trailingOnly=TRUE)
	data1.file <- args[1]
	data2.file <- args[2]
	sample2cluster1.file <- args[3]
	sample2cluster2.file <- args[4]
	mark.sample.file <- args[5]
	dataset.colname <- args[6]
	sample.colname <- args[7]
	prot.st.colname <- args[8]
	prot.new.st.colname <- args[9]
	legend.title <- args[10]
	tsne1.2.outfile <- args[11]
	tSNECluster.pdffile <- args[12]

	mark.sample.df <- read.delim(mark.sample.file, header = F,
									 check.names = F, stringsAsFactors = F)
	mark.sample.ids <- mark.sample.df[,1]
	d_tsne1.2 <- performTSNEAndPlot(data1.file, sample2cluster1.file,
							   		data2.file, sample2cluster2.file,
							   		mark.sample.ids,
							   		dataset.colname, sample.colname, 
								   	prot.st.colname, prot.new.st.colname,
								   	legend.title, tSNECluster.pdffile,
								   	tsne1.2.outfile)

}

tSNEClusterPlot2Datasets()

