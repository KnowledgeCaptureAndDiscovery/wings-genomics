library(easyGgplot2)
library(psych)

plotProtsCountSearchEng <- function(prots.all.searchengines.df, allsearches.prots.pdffile, ...) {
	prots.all.searchengines.df[prots.all.searchengines.df==0] <- NA
	panelborder.theme <- theme(panel.border = element_rect(linetype = "solid", colour = "black", fill="NA", size=3))
	panelbg.theme <- theme(panel.grid.major = element_blank(),
							panel.grid.minor = element_blank(),
							panel.background = element_blank())
	plottitle.theme <- theme(plot.title = element_text(hjust = 0.5))
	
	pdf(allsearches.prots.pdffile)
	allsearches.prots.bplot <- ggplot2.barplot(as.data.frame(describe(prots.all.searchengines.df)),
												xName = "n",
								                xtitle = "#Search Engines",
								                ytitle = "#Proteins",
								                mainTitle = "Proteins detected in number of Search Engines",
								                mainTitleFont = c(19,"bold", "black"),
								                xTickLabelFont=c(20,"bold", "black"),
								                yTickLabelFont=c(20,"bold", "black"),
								                ytitleFont=c(20,"bold", "black"),
								                xtitleFont=c(20,"bold", "black"),
								                removePanelGrid=TRUE,
								                backgroundColor="NA")
	allsearches.prots.bplot <- allsearches.prots.bplot+geom_bar(color="black")
	allsearches.prots.bplot <- allsearches.prots.bplot+panelborder.theme+panelbg.theme+plottitle.theme
    print(allsearches.prots.bplot)
	dev.off()
	return(allsearches.prots.bplot)
}

barplotProtsAndSearchEngCount <- function() {
	args = commandArgs(trailingOnly=TRUE)
	prots.all.searchengines.file <- args[1]
	allsearches.prots.pdffile <- args[2]

	prots.all.searchengines.df <- read.delim(prots.all.searchengines.file, header = T, stringsAsFactors = F, check.names = F)
	allsearches.genes.bplot <- plotProtsCountSearchEng(prots.all.searchengines.df, allsearches.prots.pdffile)
	 
	 print("Plotting genes/proteins per dataset ---> Done")
	 print(paste("Filename:", allsearches.prots.pdffile))
}

barplotProtsAndSearchEngCount()
