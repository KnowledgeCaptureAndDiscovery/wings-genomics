library(ggplot2)
library(easyGgplot2)
library(ggplot2)

# Plot the density for the given column from the input file
scatterPlotPDF <- function(data.df,
                         xvalue.colname,
                         yvalue.colname,
                         xname.title,
                         yname.title,
                         splot.title,
                         splot.pdffile) {
  data.values.corr <- cor(data.df[,xvalue.colname],
							data.df[,yvalue.colname], 
							use="complete.obs")
  panelborder.theme <- theme(panel.border = element_rect(linetype = "solid",
														 colour = "black",
														 fill="NA", size=3))
  panelbg.theme <- theme(panel.grid.major = element_blank(),
							panel.grid.minor = element_blank(),
							panel.background = element_blank())
  plot.pos.theme <- theme(plot.title = element_text(hjust = 0.5))

  # Plot density from given input column
  pdf(splot.pdffile)
  corr.splot <- ggplot2.scatterplot(data = data.df,
                xName = xvalue.colname,
                yName = yvalue.colname,
                xtitle = xname.title,
                ytitle = yname.title,
                mainTitle = splot.title,
                addRegLine = T,
                mainTitleFont = c(20,"bold", "black"),
                xTickLabelFont=c(20,"bold", "black"),
                yTickLabelFont=c(20,"bold", "black"),
                ytitleFont=c(20,"bold", "black"),
                xtitleFont=c(20,"bold", "black"),
                removePanelGrid=TRUE,
                backgroundColor="NA")
  corr.splot <- corr.splot+annotate("text", x=-Inf, y=Inf, hjust=-1.2, vjust=1.2, size =10,
									label = paste("R^2 == ", round(data.values.corr,3)),
									parse = TRUE, fontface="bold")
  corr.splot <- corr.splot+panelborder.theme+panelbg.theme+plot.pos.theme
  print(corr.splot)
  dev.off()
  return(corr.splot)
}

scatterPlotData <- function() {
	args = commandArgs(trailingOnly=TRUE)
	data.file <- args[1]
	xvalue.colname <- args[2]
	yvalue.colname <- args[3]
	xname.title <- args[4]
	yname.title <- args[5]
	splot.title  <- args[6]
	splot.pdffile <- args[7]

	data.df <- read.delim(data.file, header = T, stringsAsFactors = F, check.names = F)
	print("Plotting histogram with density curve for the given data ...")
	corr.lquant.hist <- scatterPlotPDF(data.df,
                         				xvalue.colname,
				                        yvalue.colname,
				                        xname.title,
				                        yname.title,
                				        splot.title,
				                        splot.pdffile)

	 print("Done")
	 print(paste("Filename:", splot.pdffile))
}

scatterPlotData()
