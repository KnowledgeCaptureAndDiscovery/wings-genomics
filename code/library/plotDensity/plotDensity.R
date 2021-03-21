library(ggplot2)
library(easyGgplot2)

# Plot the density for the given column from the input file
plotDensityPDF <- function(data.df,
                         value.colname,
                         xname.title,
                         yname.title,
                         dplot.title,
                         dplot.pdffile) {

  panelborder.theme <- theme(panel.border = element_rect(linetype = "solid", colour = "black", fill="NA", size=3))
  panelbg.theme <- theme(panel.grid.major = element_blank(),
							panel.grid.minor = element_blank(),
							panel.background = element_blank())
  plot.pos.theme <- theme(plot.title = element_text(hjust = 0.5))

  # Plot density from given input column
  pdf(dplot.pdffile)
  corr.dplot <- ggplot2.density(data = data.df,
                xName = value.colname,
                xtitle = xname.title,
                ytitle = yname.title,
                mainTitle = dplot.title,
                mainTitleFont = c(20,"bold", "black"),
                xTickLabelFont=c(20,"bold", "black"),
                yTickLabelFont=c(20,"bold", "black"),
                ytitleFont=c(20,"bold", "black"),
                xtitleFont=c(20,"bold", "black"),
                removePanelGrid=TRUE,
                backgroundColor="NA")
  corr.dplot <- corr.dplot+geom_density(size=1.5)
  corr.dplot <- corr.dplot+panelborder.theme+panelbg.theme+plot.pos.theme
  print(corr.dplot)
  dev.off()
  return(corr.dplot)
}

plotDensity <- function() {
	args = commandArgs(trailingOnly=TRUE)
	data.file <- args[1]
	value.colname <- args[2]
	xname.title <- args[3]
	yname.title <- args[4]
	dplot.title  <- args[5]
	dplot.pdffile <- args[6]

	data.df <- read.delim(data.file, header = T, stringsAsFactors = F, check.names = F)
	print("Plotting histogram with density curve for the given data ...")
	corr.lquant.hist <- plotDensityPDF(data.df,
                         value.colname,
                         xname.title,
                         yname.title,
                         dplot.title,
                         dplot.pdffile)

	 print("Done")
	 print(paste("Filename:", dplot.pdffile))
}

plotDensity()
