library(ggplot2)

# Plot the correlation histogram and annotate with mean value
plotHistWithDensityPDF <- function(data.df,
                         value.colname,
                         xname.title,
                         hist.title,
                         bar.fill.colorname="gray",
                         neg.bar.fill.colorname=bar.fill.colorname,
                         data.hist.pdffile = "corrHistDplot.pdf") {  
  # Calculate average correlation
  mean.data.df.value <- round(mean(data.df[,value.colname], na.rm=TRUE), 2)

  panelborder.theme <- theme(panel.border = element_rect(linetype = "solid", colour = "black", fill="NA", size=3))
  panelbg.theme <- theme(panel.grid.major = element_blank(),
							panel.grid.minor = element_blank(),
							panel.background = element_blank())
  plot.pos.theme <- theme(plot.title = element_text(hjust = 0.5))
  # Plot histograms for the calculated correlation between two variables
  stat.density.func <- "..density.."
  stat.x.func <- "..x.. < 0"
  pdf(data.hist.pdffile)
  corr.lquant.hist <- ggplot(data=data.df)
  corr.lquant.hist <- corr.lquant.hist+geom_histogram(aes_string(x= value.colname,
																 y = stat.density.func,
   																 fill = stat.x.func),
   																 color="black")
  corr.lquant.hist <- corr.lquant.hist+scale_fill_manual(values = c(bar.fill.colorname,
                                                                    neg.bar.fill.colorname),
                                                         guide = FALSE)
  corr.lquant.hist <- corr.lquant.hist+geom_density(aes_string(x= value.colname, y = stat.density.func), fill=NA)
  corr.lquant.hist <- corr.lquant.hist+labs(title=hist.title,
  										    x = xname.title)
  corr.lquant.hist <- corr.lquant.hist+scale_x_continuous(breaks = round(seq(min(data.df[,value.colname], na.rm=T),
																			 max(data.df[,value.colname], na.rm=T),
																			 by = 0.2),1))
  corr.lquant.hist <- corr.lquant.hist+geom_vline(aes(xintercept=mean.data.df.value),
									                                color="black",
									                                linetype="dashed", size=1)
  corr.lquant.hist <- corr.lquant.hist+theme(plot.title=element_text(colour="black",
                                                                     face="bold",
                                                                     size=14),
                                             axis.title=element_text(colour="black",
                                                                     face="bold",
                                                                     size=14),
                                             axis.text=element_text(colour="black",
                                                                     face="bold",
                                                                     size=12),
                                             axis.ticks = element_line(size = 0.5))
  corr.lquant.hist <- corr.lquant.hist+annotate("text", x=mean.data.df.value, y=Inf,
                                                 vjust=2, hjust=-0.1,
                                                 label=paste0("Mean = ",
                                                               mean.data.df.value),
                                                               color = "black")
  corr.lquant.hist <- corr.lquant.hist+panelborder.theme+panelbg.theme+plot.pos.theme
  print(corr.lquant.hist)
  dev.off()
  return(corr.lquant.hist)
}

plotHistWithDensity <- function() {
	args = commandArgs(trailingOnly=TRUE)
	data.file <- args[1]
	value.colname <- args[2]
	xname.title <- args[3]
	hist.title <- args[4]
	bar.col1  <- args[5]
	bar.col2  <- args[6]
	data.hist.pdffile <- args[7]

	data.df <- read.delim(data.file, header = T, stringsAsFactors = F, check.names = F)
	print("Plotting histogram with density curve for the given data ...")
	corr.lquant.hist <- plotHistWithDensityPDF(data.df,
                         value.colname,
                         xname.title,
                         hist.title,
                         bar.fill.colorname=bar.col1,
                         neg.bar.fill.colorname=bar.col2,
                         data.hist.pdffile)

	 print("Done")
	 print(paste("Filename:", data.hist.pdffile))
}

plotHistWithDensity()
