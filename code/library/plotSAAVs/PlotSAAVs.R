args <- commandArgs(TRUE)

library(easyGgplot2)
library(ggplot2)
library(Vennerable)
library(GGally)

# Barplot of SAAV count per sample
countSAAVsPerSample <- function(long.saav.df,
								sample.colname,
								var.db.colname,
								saav.colname,
								saav.sample.count.colname,
								var.db.ordered,
								saav.persample.outfile) {
	group.by.sample.vardb.symblist <- lapply(c(sample.colname, var.db.colname),
												function(x) as.symbol(x))
	saav.count.formula <- as.formula(paste("~", "length","(","unique","(",saav.colname,")",")"))
	
	long.saav.count.df <- as.data.frame(long.saav.df
                                                    %>%
                                   group_by_(.dots=group.by.sample.vardb.symblist)
                                   %>% summarise_(.dots=setNames(list(saav.count.formula),
                                                              saav.sample.count.colname)))

	long.saav.count.df[,sample.colname] <- factor(long.saav.count.df[,sample.colname],
											levels = unique(var.db.colname[order(long.saav.df[,var.db.colname],
																				 long.saav.df[,saav.sample.count.colname],
																				 decreasing = T), sample.colname]))

	long.saav.count.df[,var.db.colname] <- factor(long.saav.count.df[,var.db.colname], 
											levels = var.db.ordered)
	write.table(long.saav.count.df, saav.persample.outfile,
				row.names = F, quote = F, sep = "\t")
	return(long.saav.count.df)
}

formatAndBarplotSAAVs <- function(long.saav.df,
						 		  sample.colname,
						 		  var.db.colname,
						 		  saav.colname,
						 		  saav.sample.count.colname,
						 		  xname.title,
						 		  yname.title,
						 		  var.db.ordered,
						 		  saav.persample.outfile,
						 		  saav.persample.barplot.pdffile) {
	long.saav.count.df <- countSAAVsPerSample(long.saav.df,
											  sample.colname,
											  var.db.colname,
											  saav.colname,
											  saav.sample.count.colname,
											  var.db.ordered,
											  saav.persample.outfile)

	panelbg.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

	pdf(saav.persample.barplot.pdffile, width=8, height=8)
	saav.bplot <- ggplot2.barplot(data = long.saav.count.df,
                				  xName=sample.colname,
				                  yName=saav.sample.count.colname,
				                  faceting=TRUE,
				                  facetingVarNames=var.db.colname,
				                  facetingScales="free",
				                  groupName =var.db.colname,
				                  showLegend = F,
				                  ytitle = yname.title,
				                  xtitle = xname.title)
	saav.bplot <- saav.bplot+theme(axis.text.x=element_blank(),
                               axis.ticks.x = element_blank(),
                               axis.line.y = element_line(size=0.5,
                                                          colour = "black"),
                               axis.line.x = element_line(size=0.2,
                                                          colour = "black"))
	saav.bplot <- saav.bplot+annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=1)
	saav.bplot <- saav.bplot+panelbg.theme+scale_y_continuous(expand = c(0, 0))
	print(saav.bplot)
	dev.off()
	return(long.saav.count.df)
}

# Pie chart of distribution frequency of SAAVs
distFreqSAAVsPerSample <- function(long.saav.df,
								   samplecount.colname,
								   samplebin.colname,
								   saav.count.by.samplebin.vdb.colname,
								   var.db.colname,
								   saav.distfreq.outfile) {

	saav.1.samplebin.name <- "1 sample"
	saav.2to9.samplebin.name <- "2-9 samples"
	saav.10.samplebin.name <- ">= 10 samples"
	nosaav.samplebin.colname <- "0 samples"

	sample.count.formula <- as.formula(paste("~", "length","(","unique","(",sample.colname,")",")"))
	long.saav.pie.df <- as.data.frame(long.saav.df %>%
                                      group_by_(.dots=list(as.symbol(saav.colname),
                                                           as.symbol(var.db.colname)))
                                      %>% summarise_(.dots=setNames(list(sample.count.formula),
                                                                 samplecount.colname)))

	long.saav.pie.df[,samplebin.colname] <- as.character(ifelse(
	  long.saav.pie.df[,samplecount.colname] == 1, saav.1.samplebin.name,
    	   ifelse(long.saav.pie.df[,samplecount.colname] > 1 &
        	   long.saav.pie.df[,samplecount.colname] < 10, saav.2to9.samplebin.name,
            	ifelse(long.saav.pie.df[,samplecount.colname] >=10,
                	   saav.10.samplebin.name, nosaav.samplebin.colname))))

	saavcount.per.formula <- as.formula(paste("~", "length","(","unique","(",saav.colname,")",")"))

	# Calculate protein count in each variant and sampe bin for Venn diagram 1b
	long.saav.pie.df <- as.data.frame(long.saav.pie.df %>%
                                      group_by_(.dots=list(as.symbol(var.db.colname),
                                                           as.symbol(samplebin.colname)))
                                      %>% summarise_(.dots=setNames(list(saavcount.per.formula),
                                                                 saav.count.by.samplebin.vdb.colname)))

	long.saav.pie.df[,var.db.colname] <- factor(long.saav.pie.df[,var.db.colname],
												levels = var.db.ordered)

	samplebin.ordered <- c(saav.1.samplebin.name, saav.2to9.samplebin.name,
							saav.10.samplebin.name, nosaav.samplebin.colname)
	long.saav.pie.df[,samplebin.colname] <- factor(long.saav.pie.df[,samplebin.colname],
    											    levels=samplebin.ordered)

	write.table(long.saav.pie.df, saav.distfreq.outfile,
				row.names = F, quote = F, sep = "\t")
	return(long.saav.pie.df)
}


calcAndPlotPieChart <- function(long.saav.df,
								var.db.colname,
								piechart.title,
								saav.distfreq.outfile,
								saav.piechart.pdffile) {
	samplecount.colname <- "sample_count"
	samplebin.colname <- "sample_bin"
	saav.count.by.samplebin.vdb.colname <- "vardb_per_bin"

	long.saav.pie.df <- distFreqSAAVsPerSample(long.saav.df,
											   var.db.colname,
											   saav.distfreq.outfile)

	panelbg.theme <- theme(panel.grid.major = element_blank(),
						   panel.grid.minor = element_blank(),
						   panel.background = element_blank())
	
	pdf(saav.piechart.pdffile, width=7, height=6)
	cp <- coord_polar(theta = "y")
	cp$is_free <- function() TRUE

	saav.freq.piechart <-  ggplot(long.crc.dbsnp.cosmic.som.pie.df,
    	                           aes_string(x=factor(1),
                                          	  y=saav.count.by.samplebin.vdb.colname))
	saav.freq.piechart <- saav.freq.piechart+geom_bar(aes_string(colour=var.db.colname),
    	                                              size=3,
          		                                      stat="identity",
          		                                      show.legend = FALSE)
	saav.freq.piechart <- saav.freq.piechart+geom_bar(aes_string(fill=samplebin.colname),
    	                                              stat="identity")

	saav.freq.piechart <- saav.freq.piechart+facet_wrap(as.formula(paste("~",
                                                                     var.db.colname)),
    	                                                scales = "free")
	saav.freq.piechart <- saav.freq.piechart+scale_fill_grey(start=0.95, end=0.2)
	saav.freq.piechart <- saav.freq.piechart+cp
	saav.freq.piechart <- saav.freq.piechart+geom_text(aes_string(label=saav.count.by.samplebin.vdb.colname),
													   position = position_stack(vjust = 0.5),
    	                                               size=6,
        	                                           colour="red")
	saav.freq.piechart <- saav.freq.piechart+ggtitle(piechart.title)
	saav.freq.piechart <- saav.freq.piechart+guides(fill=guide_legend(title="",
                                                                  	  label.theme = element_text(size = 12)))
	saav.freq.piechart <- saav.freq.piechart+theme(axis.ticks = element_blank(),
    	                                           axis.text = element_blank(),
        	                                       axis.title = element_blank(),
            	                                   axis.line = element_blank(),
                	                               strip.text = element_text(face="plain",
                                                                         size=12),
                    	                           legend.key = element_rect(colour = "black"),
                        	                       plot.title = element_text(size=14,
                            	                                             face="bold",
                                	                                         colour = "black",
                                    	                                     hjust = 0.5),
                                        	       panel.border = element_rect(linetype = "solid",
                                                                           colour = "black",
                                                                           fill="NA", size=1))
	saav.freq.piechart <- saav.freq.piechart+panelbg.theme
	print(saav.freq.piechart)
	dev.off()
	
	return(long.saav.pie.df)
}

# Venn Diagram SAAVs
drawVennDiagram <- function(long.saav.df, 
							var.db.colname,
							saav.venn.pdffile) {

	pdf(saav.venn.pdffile, height=8, width=5)

	# Get list of proteins from each variant database for vennerable - venn object
	var.dbprots.list <- list(unique(long.saav.df[,var.db.colname]))
	var.dbprots.list <- lapply(var.dbs.list, function(var.db)
    		                     long.saav.df[long.saav.df[,var.db.colname] == var.db,
				                              saav.colname])

	# Create a vennerable object:
	venn.var.dbs <- Venn(Sets=var.dbprots.list, SetNames=var.dbs.list)

	# Plot the venn object in scaled mode
	Vennerable::plot(venn.var.dbs, doWeights=TRUE)
	dev.off()
	return(var.dbprots.list)
}


formatAndPlotSAAVsWrapper <- function(long.saav.df,
						  			  sample.colname,
						  			  var.db.colname,
						  			  saav.colname,
						  			  saav.sample.count.colname,
						  			  xname.title,
						  			  yname.title,
								      var.db.ordered,
								      saav.persample.outfile,
								      saav.persample.barplot.pdffile) {

	long.saav.count.df <- formatAndBarplotSAAVs(long.saav.df,
										  		sample.colname,
											    var.db.colname,
											    saav.colname,
											    saav.sample.count.colname,
											    xname.title,
											    yname.title,
											    var.db.ordered,
											    saav.persample.outfile,
											    saav.persample.barplot.pdffile)

	long.saav.pie.df <- calcAndPlotPieChart(samplecount.colname,
								   			samplebin.colname,
										    saav.count.by.samplebin.vdb.colname,
								   			var.db.colname,
								   			piechart.title,
								   			saav.distfreq.outfile,
								   			saav.piechart.pdffile)

	var.dbprots.list <- drawVennDiagram(long.saav.df,
										  var.db.colname,
										  saav.colname,
										  saav.venn.pdffile)
	return(list(long.saav.count.df, long.saav.pie.df, var.dbprots.list))
}

plotSAAVs <- function() {
	args = commandArgs(trailingOnly=TRUE)
	saav.file <- args[1]
	sample.colname <- args[2]
	var.db.colname <- args[3]
	saav.colname <- args[4]
	saav.sample.count.colname <- args[5]
	xname.title <- args[6]
	yname.title <- args[7]
	var.db.ordered.str <- args[8] 
	piechart.title <- args[9]
	saav.persample.barplot.pdffile <- args[10]
	saav.venn.pdffile <- args[11]
	saav.piechart.pdffile <- args[12]
	saav.persample.outfile <- args[13]
	saav.distfreq.outfile <- args[14]

	#saav.sample.count.colname <- "saav_per_sample"

	long.saav.count.pie.vardbprots.list <- formatAndPlotSAAVsWrapper(long.saav.df,
						  			  sample.colname,
						  			  var.db.colname,
						  			  saav.colname,
						  			  saav.sample.count.colname,
						  			  xname.title,
						  			  yname.title,
								      var.db.ordered,
								      saav.persample.outfile,
								      saav.persample.barplot.pdffile)
	var.db.ordered <- unlist(strsplit(var.db.ordered.str, ","))
	saav.df <- read.delim(saav.file, header = T, stringsAsFactors = F, check.names = F)

}

plotSAAVs()
