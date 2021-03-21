library(easyGgplot2)

plotProtsPerDataset <- function(prots.all.datasets.df, alldatasets.prots.pdffile, xvalue.colname, id.colname, ...) {
	prots.all.datasets.df[prots.all.datasets.df==0] <- NA
	panelborder.theme <- theme(panel.border = element_rect(linetype = "solid", colour = "black", fill="NA", size=3))
	panelbg.theme <- theme(panel.grid.major = element_blank(),
							panel.grid.minor = element_blank(),
							panel.background = element_blank())
	plottitle.theme <- theme(plot.title = element_text(hjust = 0.5))
	
	pdf(alldatasets.prots.pdffile)
	allsearches.prots.bplot <- ggplot2.barplot(unique(prots.all.datasets.df[,c(id.colname, xvalue.colname)]),
												xName = xvalue.colname,
								                xtitle = "",
								                ytitle = "#Proteins",
								                mainTitle = "Proteins per dataset",
								                mainTitleFont = c(19,"bold", "black"),
								                xTickLabelFont=c(20,"bold", "black"),
								                yTickLabelFont=c(20,"bold", "black"),
								                ytitleFont=c(20,"bold", "black"),
								                xtitleFont=c(20,"bold", "black"),
								                groupName = xvalue.colname,
								                removePanelGrid=TRUE,
								                backgroundColor="NA")
	allsearches.prots.bplot <- allsearches.prots.bplot+geom_bar(color="black")
	allsearches.prots.bplot <- allsearches.prots.bplot+panelborder.theme+panelbg.theme+plottitle.theme
    print(allsearches.prots.bplot)
	dev.off()
	return(allsearches.prots.bplot)
}

barplotProtsPerDataset <- function() {
	args = commandArgs(trailingOnly=TRUE)
	prots.all.datasets.file <- args[1]
	alldatasets.prots.pdffile <- args[2]
	xvalue.colname <- args[3]
    id.colname <- args[4]

	prots.all.datasets.df <- read.delim(prots.all.datasets.file, header = T, stringsAsFactors = F, check.names = F)
	allsearches.genes.bplot <- plotProtsPerDataset(prots.all.datasets.df, alldatasets.prots.pdffile, xvalue.colname, id.colname)
	 
	 print("Plotting genes/proteins per dataset ---> Done")
	 print(paste("Filename:", alldatasets.prots.pdffile))
}

barplotProtsPerDataset()
