suppressMessages(library(org.Hs.eg.db))
suppressMessages(library(gage))
data(kegg.gs)
suppressMessages(library(biomaRt))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))

getEntrezIDs <- function(data.corr.df, geneid.colname, entrez.geneid.colname) {
  entrezid.colname <- "entrez_id_temp"
  entrez.bmid.colname <- "entrez_bm_id"
  gene.entrez.symbol.map.df <- setNames(AnnotationDbi::select(org.Hs.eg.db,
                                                              keys = unique(as.character(data.corr.df[,geneid.colname])),
                                                              columns=c("ENTREZID","SYMBOL"),
                                                              keytype="SYMBOL"),
                                        c(geneid.colname, entrezid.colname))

  data.corr.entrez <- merge(data.corr.df, gene.entrez.symbol.map.df, by=geneid.colname, all.x=TRUE)
  
  if(nrow(unique(data.corr.entrez[is.na(data.corr.entrez[,entrezid.colname]),])) > 0) {
    ensembl.biomart.name <- "ensembl"
    ensembl.dataset.name <- "hsapiens_gene_ensembl"
    
    # If not found above, try retrieving from biomart just incase.
    human.ensembl.mart <- biomaRt::useMart(biomart = ensembl.biomart.name,
                                  dataset = ensembl.dataset.name)
    entrez.genename.map.df <- setNames(getBM(attributes = c("entrezgene",
                                                            "hgnc_symbol"),#or "external_gene_name",
                                             filters = c("hgnc_symbol"),
                                             values = list(hgnc_symbol=unique(
                                               data.corr.entrez[is.na(data.corr.entrez[,entrezid.colname]),])),
                                             mart = human.ensembl.mart,
                                             uniqueRows = TRUE), c(entrez.bmid.colname,
                                                                   geneid.colname))
    
    data.corr.entrez <- merge(data.corr.entrez, entrez.genename.map.df, by=geneid.colname, all.x=TRUE)
    data.corr.entrez[,entrez.geneid.colname] <- ifelse(is.na(data.corr.entrez[,entrezid.colname]) &
                                                          !is.na(data.corr.entrez[,entrez.bmid.colname]),
                                                        data.corr.entrez[,entrez.bmid.colname], data.corr.entrez[,entrezid.colname])
  } else {
    data.corr.entrez[,entrez.geneid.colname] <- data.corr.entrez[,entrezid.colname]
  }
  data.corr.entrez <- data.corr.entrez[,!colnames(data.corr.entrez) 
                                       %in% c(entrezid.colname, entrez.bmid.colname)]
  return(data.corr.entrez)
}

#KEGG analysis
keggEnrichAnalysis <- function(data.df,
                               retain.pathways,
                               geneid.colname,
                               entrez.geneid.colname,
                               kegg.enrichby.colname,#estimate
                               bplot.corr.ytitle,
                               bplot.corr.xtitle,
                               kegg.ann.sig.out.colname,
                               kegg.padj.cutoff = 0.05,
                               gage.kegg.sig.outfile,
                               siggenes.data.outfile,
                               bplot.corrbygenes.pdffile,
                               kegg.bplot.pdffile) {
  reg.colname <- "up_or_down"
  qvalue.colname <- "q.val"
  kegg.ann.out.colname <- "kegg_ann"
  kegg.sig.out.colname <- "kegg_sig"
  kegg.sig.padj.out.colname <- "kegg_sig_padj"
  #kegg.ann.sig.out.colname <- "kegg_ann_sig"

  data.df <- getEntrezIDs(data.df, geneid.colname, entrez.geneid.colname)
  # Start GAGE analysis with KEGG dataset:
  print("Performing Kegg Enrichment analysis...")
  dataMatrix <- as.matrix(data.df[,kegg.enrichby.colname])
  row.names(dataMatrix) <- data.df[,entrez.geneid.colname]
  gage.data.df <- gage(dataMatrix,
                       gsets = kegg.gs,
                       saaTest = gs.KSTest,
                       samp = NULL)
  print("Done.")
  print("Sort and count the significant gene set based on the given cutoff p-values...")
  gage.input.kegg.sig <- sigGeneSet(gage.data.df, qpval = qvalue.colname,
                                    cutoff = kegg.padj.cutoff,
			                        heatmap=FALSE)
  print("Done.")
  gage.input.kegg.sig.bothdir <- rbind(gage.input.kegg.sig$greater,
                                       gage.input.kegg.sig$less)
  gage.input.kegg.sig.bothdir <- setNames(data.frame(row.names(gage.input.kegg.sig.bothdir),
                                            gage.input.kegg.sig.bothdir,
                                            stringsAsFactors = FALSE,
                                            check.names = F),
                                          c(kegg.ann.out.colname,
                                            colnames(gage.input.kegg.sig.bothdir)))
  
  write.table(gage.input.kegg.sig.bothdir, gage.kegg.sig.outfile, sep="\t", row.names = FALSE, quote = FALSE)
  
  if (nrow(gage.input.kegg.sig.bothdir) >1 ) {  
    kegg.gs.df <- melt(kegg.gs,
                       value.name = entrez.geneid.colname,
                       level = kegg.ann.out.colname)
    
    # Melting list adds L to levelname, strip the "L" from column names
    # Hack for now:
    colnames(kegg.gs.df)[2] <- kegg.ann.out.colname
    
    # something wrong but worry later??
    data.df <- merge(data.df, kegg.gs.df, by=entrez.geneid.colname, all.x=TRUE)
    
    data.df[,kegg.sig.out.colname] <- ifelse(data.df[,kegg.ann.out.colname] %in%
                                      gage.input.kegg.sig.bothdir[,kegg.ann.out.colname],
                                    1, 0)
    data.df <- merge(data.df, gage.input.kegg.sig.bothdir[,c(kegg.ann.out.colname, qvalue.colname)],
                      by=kegg.ann.out.colname, all.x=TRUE)
    data.df[,kegg.sig.padj.out.colname] <- data.df[,qvalue.colname]
    
    
    # File has been written. So remove preceding hsa_id from the "kegg_ann" column:
    data.df[,kegg.ann.out.colname] <- gsub("hsa\\d+ ?","", data.df[,kegg.ann.out.colname])
    data.df[,kegg.ann.sig.out.colname] <- ifelse(data.df[,kegg.sig.out.colname] == 1,
                                        paste0(data.df[,kegg.ann.out.colname],
                                               " (", formatC(data.df[,kegg.sig.padj.out.colname]),")"), NA)
    
    data.df <- unique(data.df[,c(entrez.geneid.colname,
                                   geneid.colname,
                                   kegg.enrichby.colname,
                                   kegg.ann.out.colname,
                                   kegg.sig.out.colname,
                                   kegg.sig.padj.out.colname,
                                   kegg.ann.sig.out.colname)])

    # Remove NAs from the kegg.ann.sig.out.colname column
    data.df <- data.df[!is.na(data.df[,kegg.ann.sig.out.colname]),]
    
    # Reorder the levels of geneid.colname by decreasing order of correlation values column
    data.df <- data.df[!is.na(data.df[,geneid.colname]),]
    data.df[,geneid.colname] <- factor(data.df[,geneid.colname],
                                      levels = unique(data.df[order(data.df[,kegg.enrichby.colname],
                                                              na.last = FALSE,
                                                              decreasing=TRUE), geneid.colname]))
    data.df[,reg.colname] <- as.factor(data.df[,kegg.enrichby.colname] < 0)
    write.table(data.df, siggenes.data.outfile, sep="\t", row.names = FALSE, quote = FALSE)
	
    panelbg.theme <- theme(panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank())
    
    # Further aesthetics later:
    pdf(bplot.corrbygenes.pdffile, width=8.25, height=3)
    corr.bygenes.bplot <-  ggplot(data=data.df,
                                  aes_string(x = geneid.colname,
                                             y = kegg.enrichby.colname,
                                             fill = reg.colname))
    corr.bygenes.bplot <-  corr.bygenes.bplot+geom_bar(stat="identity", position = "identity")
    corr.bygenes.bplot <-  corr.bygenes.bplot+scale_fill_manual(values=c("darkred", "darkgreen"),
                                                                guide=FALSE)
    corr.bygenes.bplot <- corr.bygenes.bplot+theme(axis.ticks.x=element_blank(),
                                                   axis.text.x=element_blank(),
                                                   axis.text.y=element_text(face = "bold",
                                                   							color = "black",
                                                   							size =12),
                                                   axis.title.x=element_text(face = "bold",
                                                                     		 color = "black",
                                                                     		 size = 14),
                                                   axis.title.y=element_text(angle = 0,
                                                                             face = "bold",
                                                                             color = "black",
                                                                             size = 14,
                                                                             vjust = 0.5),
                                                   axis.line.y=element_line(size=1))
    corr.bygenes.bplot <- corr.bygenes.bplot+labs(x=bplot.corr.xtitle, y=paste(bplot.corr.ytitle))
    corr.bygenes.bplot <- corr.bygenes.bplot+panelbg.theme
    print(corr.bygenes.bplot)
    dev.off()
    
    # Pathway annotations plot for the requested (arguments) pathways. If none, try plotting everything!
    print("Retaining the required pathways...")
    if(!is.na(retain.pathways)) {
      data.df <- data.df[data.df[,kegg.ann.out.colname] %in% retain.pathways, ]
    }
    
    pdf(kegg.bplot.pdffile, width=10, height=3)
    keggsig.bplot <- ggplot(data = data.df,
                            aes_string(x=geneid.colname,
                                       y=kegg.sig.out.colname,
                                       fill=reg.colname))
    keggsig.bplot <- keggsig.bplot+geom_bar(stat="identity", position="identity", width = 0.5)
    keggsig.bplot <- keggsig.bplot+scale_fill_manual(values=c("darkred", "darkgreen"),
                                                     guide=FALSE)
    keggsig.bplot <- keggsig.bplot+facet_wrap(as.formula(paste("~",kegg.ann.sig.out.colname)),
                                              ncol=1, switch = "y")
    keggsig.bplot <- keggsig.bplot+theme(axis.ticks=element_blank(),
                                         axis.text=element_blank(),
                                         axis.title=element_blank(),
                                         strip.text.y = element_text(angle = 180,
                                                                     face = "bold",
                                                                     size = 14,
                                                                     hjust = 0))
    keggsig.bplot <- keggsig.bplot+panelbg.theme
    print(keggsig.bplot)
    dev.off()
  } else {
    # Produce dummy files for wings
    pdf(bplot.corrbygenes.pdffile)
    dev.off()
    
    pdf(kegg.bplot.pdffile)
    dev.off()
  }
  return(data.df)
}

performKeggEnrichment <- function() {
  args = commandArgs(trailingOnly=TRUE)
  data.file <- args[1]
  retain.pathways.file <- args[2]
  geneid.colname <- args[3]
  entrez.geneid.colname <- args[4]
  kegg.enrichby.colname <- args[5]
  bplot.corr.ytitle <- args[6]
  bplot.corr.xtitle <- args[7]
  kegg.sig.ann.out.colname <- args[8]
  kegg.padj.cutoff <- as.numeric(as.character(args[9]))
  gage.kegg.sig.outfile <- args[10]
  siggenes.data.outfile <- args[11]
  bplot.corrbygenes.pdffile <- args[12]
  kegg.bplot.pdffile <- args[13]
  
  data.df <- read.delim(data.file, header = T, stringsAsFactors = F, check.names = F)
  retain.pathways.df <- read.delim(retain.pathways.file, header = F, stringsAsFactors = F, check.names = F)
  retain.pathways <- retain.pathways.df[,1]
  print("Performing Kegg Enrichment analysis and retaining only required pathways")
  kegg.pw.data.df <- keggEnrichAnalysis(data.df,
                     					retain.pathways,
                     					geneid.colname,
                     					entrez.geneid.colname,
                     					kegg.enrichby.colname,
                     					bplot.corr.ytitle,
                     					bplot.corr.xtitle,
                     					kegg.sig.ann.out.colname,
                     					kegg.padj.cutoff,
                     					gage.kegg.sig.outfile,
                     					siggenes.data.outfile,
                     					bplot.corrbygenes.pdffile,
                     					kegg.bplot.pdffile)
  print("Done!")
  print(paste("Output written to:", gage.kegg.sig.outfile, siggenes.data.outfile))
  print(paste("Output PDF files:", bplot.corrbygenes.pdffile, kegg.bplot.pdffile))
}

performKeggEnrichment()
