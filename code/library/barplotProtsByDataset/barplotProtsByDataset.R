library(ggplot2)
# long.datasets.df is a dataframe in long format 
plotBarPDF <- function(long.datasets.df,
						protPerDataset.bplot.pdffile,
						sample.colname,
						genes.per.dataset.colname,
						dataset.overlap.colname) {

	panelborder.theme <- theme(panel.border = element_rect(linetype = "solid", colour = "black", fill="NA", size=3))
	panelbg.theme <- theme(panel.grid.major = element_blank(),
							panel.grid.minor = element_blank(),
							panel.background = element_blank())
	plottitle.theme <- theme(plot.title = element_text(hjust = 0.5))
	
	pdf(protPerDataset.bplot.pdffile, width = 10, height = 6)
	protPerDataset.bplot <- ggplot(data = long.datasets.df,
                                   aes_string(x = sample.colname, y = genes.per.dataset.colname, fill=dataset.overlap.colname))
    protPerDataset.bplot <- protPerDataset.bplot+geom_bar(width=0.8, size=0.3, stat="identity", colour="black")
    protPerDataset.bplot <- protPerDataset.bplot+scale_fill_brewer(palette="Greys")
    protPerDataset.bplot <- protPerDataset.bplot+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2,
    																				size=7,face="bold", colour = "black"),
														axis.text.y=element_text(size=20,face="bold", colour = "black"),
														axis.title=element_text(size=20,face="bold", colour = "black"),
														panel.border = element_rect(linetype = "solid", colour = "black", fill="NA", size=1),
														plot.title = element_text(size=24, face="bold", colour = "black"),
														legend.text=element_text(size = 15, face="bold", color="black"), 
														legend.title=element_text(size = 20, face="bold", color="black"))
    protPerDataset.bplot <- protPerDataset.bplot+xlab("")+ylab("#Proteins")+ggtitle("#Proteins per dataset")+guides(fill=guide_legend(title="Dataset"))
	protPerDataset.bplot <- protPerDataset.bplot+panelborder.theme+panelbg.theme+plottitle.theme
	print(protPerDataset.bplot)
	dev.off()
	return(protPerDataset.bplot)
}

# Plot proteins per dataset (gradient barplot)
barplotProtsByDataset <- function() {
	args = commandArgs(trailingOnly=TRUE)
	long.datasets.df <- args[1]
	dataset.overlap.colname <- args[2]
	sample.colname <- args[3]
	genes.per.dataset.colname <- args[4]
	protPerDataset.bplot.pdffile <- args[5]

	long.datasets.df <- read.delim(long.datasets.df, header = T, stringsAsFactors = F, check.names = F)
	protPerDataset.bplot <- plotBarPDF(long.datasets.df,
										protPerDataset.bplot.pdffile,
										sample.colname,
										genes.per.dataset.colname,
										dataset.overlap.colname)
	 
	 print("Plotting genes/proteins per dataset ---> Done")
	 print(paste("Filename:", protPerDataset.bplot.pdffile))
}

barplotProtsByDataset()
