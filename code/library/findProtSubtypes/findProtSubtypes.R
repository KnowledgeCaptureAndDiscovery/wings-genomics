library(ConsensusClusterPlus)
library(gdata)
library(ComplexHeatmap)
library(cluster)
library(pamr)
library(dplyr)
library(circlize)

# Using mutate to apply formula to a dataset
mutateDplyrByFormulaAndGroup <- function(input.df, groupby.colnames,
                                         input.formula, grouped.output.colname) {

  group.by.list.symbolised <- lapply(groupby.colnames, function(x) as.symbol(x))
  inputgrouped.df <- as.data.frame(input.df %>%
                                   group_by_(.dots=group.by.list.symbolised)
                                   %>% mutate_(.dots=setNames(list(input.formula),
                                                              grouped.output.colname)))
  return(inputgrouped.df)
}

### Normalize data by rows and columns:
# Note: Written by Xiaojing Wang (Guessing she had proteins as columns - we have them as rows)
# Swapped normalization:
normalizeDataBeforeClustering <- function(data_for_clustering) {
  data_for_clustering_col <- apply(data_for_clustering, 2, function(x) (x-mean(x))/sd(x))
  data_for_clustering_norm <- apply(data_for_clustering_col, 1, function(x) (x-mean(x))/sd(x))
  data_col_row_norm <- t(data_for_clustering_norm)
  write.table(data_col_row_norm, "normDataForClustering.txt", sep="\t", row.names = T, quote = F)
  return(data_col_row_norm)
}

getMaxK <- function(input.matrix, clusterAlg = "hc", ...) {
  this_assignment <- NA
  this_cluster <- NA
  max.k <- NA
  if(clusterAlg=="hc"){
    ##prune to k for hc
    main.dist.obj <- as.dist(1-cor(input.matrix, ...))
    this_dist <- as.matrix(main.dist.obj)
    this_cluster = hclust(this_dist, method="average")
    n1 <- nrow(tree$merge)
    n <- n1 + 1
    print(n)
    max.k <- n
  }
  
  return(max.k)
}

# Post consensus clustering, select true "k" with no change in AUC for CDFs amongst k=i...n
selectTrueK <- function(cc.list) {
  # Start the function with 2 because the first in consensus cluster is a list of colors.
  cc.res.list <- lapply(2:length(cc.list), function(x) return(cc.list[[x]]$consensusMatrix))
  
  # Copied from ConsensusClusteringPlus package:
  # Harsh way of importing a private function:
  triangle <- environment(ConsensusClusterPlus)$triangle
  areaK <- c()
  breaks <- 100
  
  for (i in 1:length(cc.res.list)) {
    ml = cc.res.list
    v=triangle(ml[[i]],mode=1)
    
    #empirical CDF distribution. default number of breaks is 100    
    h = hist(v, plot=FALSE, breaks=seq(0,1,by=1/breaks))
    h$counts = cumsum(h$counts)/sum(h$counts)
    #calculate area under CDF curve, by histogram method.
    thisArea=0
    for (bi in 1:(length(h$breaks)-1)) {
      thisArea = thisArea + h$counts[bi]*(h$breaks[bi+1]-h$breaks[bi]) #increment by height by width
      bi = bi + 1
    }
    areaK = c(areaK, thisArea)
  }
  areaK.delta <- sapply(1:(length(areaK)-1), function(x) return(areaK[x+1] - areaK[x]))
  min.k.ix <- which(areaK.delta == min(areaK.delta, na.rm=T))
  
  if(length(min.k.ix) > 1) {
    min.k.ix <- which(areaK.delta == min(areaK.delta, na.rm=T))[1]
  }
  
  if(min.k.ix == 0) {
    min.k.ix <- min.k.ix - 1
  }
  
  # Add 1 to the index of minimum k found because the clusters start from 2..
  true.k <- min.k.ix+1
  
  print(paste("Selecting cluster#", true.k))
  return(true.k)
}

# Post consensus clustering, select the clusters with only > 5 samples
# input.df: First column -: sample_id, second_column- cluster#
# selectClustersFromTrueK
filterSmallClusters <- function(cc.sample.classes, #cc.class.df,
                                min.samples.count = 6,
                                sample.colname = "sample_id",
                                cluster.colname = "prot_subtype",
                                calc.freq.colname="cluster_freq") {
  
  cc.class.df <- setNames(data.frame(sample_id=names(cc.sample.classes),
                                     prot_subtype=cc.sample.classes,
                                     stringsAsFactors = FALSE),
                          c(sample.colname, cluster.colname))
  
  count.samples.in.cluster.formula <- as.formula(paste("~", "length","(",sample.colname,")"))
  cc.class.df <- mutateDplyrByFormulaAndGroup(cc.class.df, cluster.colname,
                                              count.samples.in.cluster.formula,
                                              calc.freq.colname)
  
  cc.class.filt.df <- cc.class.df[cc.class.df[,calc.freq.colname] >= min.samples.count,]
  
  if(nrow(cc.class.filt.df) < 1) {
    print(paste("No clusters found for the given data with min. sample count", min.samples.count))
    print(paste("NOT filtering the data!"))
    cc.class.filt.df <- cc.class.df
  }

  # Re-label the clusters
  new.cluster.colname <- paste0("new_",cluster.colname)
  old.cluster.colname <- paste0("old_",cluster.colname)
  unique.clusters <- unique(cc.class.filt.df[,cluster.colname])
  
  cluster.labels.map.df <- setNames(data.frame(unique.clusters,
                                               1:length(unique.clusters),
                                               stringsAsFactors = F),
                                    c(cluster.colname, new.cluster.colname))
  cc.class.filt.df <- plyr::join(cc.class.filt.df, cluster.labels.map.df)
  cc.class.filt.df[,old.cluster.colname] <- cc.class.filt.df[,cluster.colname]
  cc.class.filt.df[,cluster.colname] <- cc.class.filt.df[,new.cluster.colname]
  
  cc.class.filt.df <- unique(cc.class.filt.df[c(sample.colname, cluster.colname)])
  return(cc.class.filt.df)
}


## core sample identification
calcAndPlotSilhouetteWidth <- function(sample.data.matrix, sample2clu,
                                       id.colname, cluster.colname, 
                                       sp.pdffile, ...) {
  sample.data.matrix <- sample.data.matrix[,colnames(sample.data.matrix) %in% sample2clu[,id.colname]]
  
  unique.clusters <- unique(sample2clu[,cluster.colname])
  

  cluster_label <- as.integer(sample2clu[sample2clu[, id.colname] 
                                         %in% colnames(sample.data.matrix),
                                         cluster.colname])
  
  data_dis = as.dist(1-cor(sample.data.matrix,  method="pearson"))
  sil.obj <- silhouette(cluster_label, dist = data_dis)
  
  cols <- rainbow(length(unique.clusters))
  pdf(sp.pdffile)
  plot(sil.obj,
       col = cols,
       main = "Silhouette Plot", ...)
  dev.off()
  
  return(sil.obj)
}

# sample.data.matrix has samples as columns. The column names should be the sample_ids same format as input sample.ids list
identifyCoreSamples <- function(sample.data.matrix, sample2clu, ...) {
  sil.obj <- calcAndPlotSilhouetteWidth(sample.data.matrix, sample2clu, ...)
  core.sample.df <-  sample2clu[sil.obj[,3]>0, ]
  return(core.sample.df)
}

# PAM Analysis -> validate
pamAnalysis <- function(sample.data.matrix,
                        sample.colname = "sample_id",
                        sample.cluster.map.df,
                        cluster.colname) {
  
  data.filt.matrix <- sample.data.matrix
  genenames <- row.names(data.filt.matrix)
  geneid <- paste("GENE",1:length(genenames),sep="")
  cluster_label <- factor(sample.cluster.map.df[,cluster.colname])
  filt.data = list(x=data.filt.matrix,
                   y=cluster_label,
                   geneid=geneid,
                   genenames=genenames)
  
  filt.data.train = pamr.train(filt.data)
  filt.data.results = pamr.cv(filt.data.train,filt.data)
  
  genelist1 = pamr.listgenes(filt.data.train, filt.data, threshold=0.892)
  selected_geneid = genelist1[,1]
  selected_matrix = data.filt.matrix[geneid %in% selected_geneid,]
  return(list(selected_matrix, genelist1))
}

### Filter and format input matrix -> validate
subtypeFormatMatrix <- function(sample.data.matrix,
                                pam.analysis.genes.matrix.list,
                                sample.cluster.map.df,
                                cluster.colname) {
  pam.analysis.matrix <- pam.analysis.genes.matrix.list[[1]]
  genelist1 <- pam.analysis.genes.matrix.list[[2]]
  
  genenames <- row.names(sample.data.matrix)
  geneid <- paste("GENE",1:length(genenames),sep="")
  
  unique.clusters.count <- length(unique(sample.cluster.map.df[,cluster.colname]))

  row.names(genelist1) <- genelist1[, 1]
  genelist1 <- genelist1[,2:ncol(genelist1)]
  
  data = vector()
  label_set= vector()
  r.name = vector()
  for (i in 1:nrow(genelist1)) {
    score_vector = as.numeric(genelist1[i,])
    label_vector = rep(0,unique.clusters.count)
    label_vector[abs(score_vector)==max(abs(score_vector))] = score_vector[abs(score_vector)==max(abs(score_vector))]/max(abs(score_vector))
    label_set= rbind(label_set,label_vector)
    
    data_row = pam.analysis.matrix[genenames[geneid==row.names(genelist1)[i]],]
    data =  rbind(data,data_row)
    r.name = c(r.name,genenames[geneid==row.names(genelist1)[i]])
  }
  
  row.names(data) = r.name
  subtypes_down_up_list <- lapply(unique.clusters.count:1,
                                function(i) {
                                  return(rbind(subtype_i_down = data[which(label_set[,i]==(-1)),],
                                               subtype_i_up = data[which(label_set[,i]==1),]))
                                  })
  data <- do.call(rbind, subtypes_down_up_list)
  return(data[nrow(data):1,])
}

assignColorsToAnnGroups <- function(data.ann.df, data.matrix,
									sample.colname, data.ann.colname) {
		no.yes.scale <- c("No", "Yes")
		no.yes.palette <- colorRampPalette(c('grey', 'pink'))
		sp.palette <- colorRampPalette(c('salmon','purple'))
		ryg.palette <- colorRampPalette(c('red','orange' , 'yellow','lightgreen'))
		bybl.palette <- colorRampPalette(c('blue','yellow','black'))
		desousa.scale <- c("CCS1", "CCS2", "CCS3")
		desousa.palette <- c("lightblue", "lightgreen", "darkblue")
		sadanandam.scale <- c("Enterocyte/Goblet-like", "Inflammatory", "Stem-like", "TA")
		sadanandam.palette <- c("olivedrab1", "brown", "darkblue", "purple")
		
		data.ann.cols <- ""
		
		data.ann <- data.ann.df[data.ann.df[,sample.colname]  %in% 
                                               colnames(data.matrix),
                                data.ann.colname]
        data.ann.uniq <- unique(sort(data.ann))
		data.ann.nona.length <- length(data.ann.uniq[!is.na(data.ann.uniq)])
		data.ann.length <- length(data.ann.uniq)

		if(data.ann.nona.length == 2) {
			if(identical(sort(data.ann.uniq), no.yes.scale)) {
				data.ann.cols <- no.yes.palette(data.ann.length)
			} else {
				data.ann.cols <- sp.palette(data.ann.length)
			}
		} else if(data.ann.nona.length == 3) {
			if(identical(sort(data.ann.uniq), desousa.scale)) {
				data.ann.cols <- desousa.palette
			}
			else {
				data.ann.cols <- bybl.palette(data.ann.length)
			}
		} else if(data.ann.nona.length == 4) {
			if(identical(sort(data.ann.uniq), sadanandam.scale)) {
				data.ann.cols <- sadanandam.palette
			} else {
				data.ann.cols <- ryg.palette(data.ann.length)
			}
		} else if(data.ann.nona.length >= 5) {
			data.ann.cols <- rainbow(data.ann.length)
		}
		return(setNames(data.ann.cols, data.ann.uniq))
}

# Generate heatmap with proteomic subtype annotation
heatmapWithProtSubtypeAnn <- function(sample.data.matrix,
                                     sample.cluster.map.df,
                                     hm.ann.df,
                                     hm.subtype.pdffile,
                                     sample.colname,
                                     prot.st.colname,
                                     add.bottom.ann, ...) {
	prot.st.new.colname <- paste0(prot.st.colname,".")
    prot.ann <- sample.cluster.map.df[, prot.st.colname]
    prot.cols <- setNames(rainbow(length(unique(sort(prot.ann)))), unique(sort(prot.ann)))

	subtype.colnames <- prot.st.new.colname
    prot.subtypes.ann.df <- setNames(data.frame(prot.ann), prot.st.colname)
    colnames(prot.subtypes.ann.df)[colnames(prot.subtypes.ann.df) == prot.st.colname] <- prot.st.new.colname
    prot.subtypes.ann.cols.list <- setNames(list(prot.cols), subtype.colnames)

    ha.top.subtypes <- HeatmapAnnotation(df = prot.subtypes.ann.df,
                                     	col = prot.subtypes.ann.cols.list,
                                     	show_legend = FALSE)

	if(add.bottom.ann) {
		hm.ann.sample.ids <- hm.ann.df[,sample.colname]
		sample.cluster.map.df[,sample.colname] <- gsub(paste0("(.*)(",
                                               paste(hm.ann.sample.ids, collapse = "|"),
                                               ")(.*)"),
                                               "\\2",
                                        sample.cluster.map.df[,sample.colname])
		colnames(sample.data.matrix) <- gsub(paste0("(.*)(",
                                               paste(hm.ann.sample.ids, collapse = "|"),
                                               ")(.*)"),
                                               "\\2",
                                        colnames(sample.data.matrix))
		hm.width <- 9
		hm.height <- 10
		hm.ann.pt.subtype.df <- merge(sample.cluster.map.df,
                                  	  hm.ann.df,
                                  	  by=sample.colname,
                                  	  all.x=TRUE)
    	write.table(hm.ann.pt.subtype.df, "coreSampleswithAnnotations.txt",
				sep = "\t", quote = FALSE, row.names = FALSE)

		subtype.colnames <- colnames(hm.ann.pt.subtype.df)[!colnames(hm.ann.pt.subtype.df) %in% sample.colname]
		subtypes.ann.df <- hm.ann.pt.subtype.df[order(hm.ann.pt.subtype.df[,prot.st.colname]),
												colnames(hm.ann.pt.subtype.df) %in% subtype.colnames]
		subtypes.ann.cols.list <- setNames(lapply(subtype.colnames, function(x) 
														assignColorsToAnnGroups(hm.ann.pt.subtype.df,
																				sample.data.matrix,
																				sample.colname, x)),
										   subtype.colnames)
		ha.bottom.subtypes <- HeatmapAnnotation(df = subtypes.ann.df,
												col = subtypes.ann.cols.list,
                                          		gap = unit(1, "mm"),
                                          		annotation_legend_param=list(labels_gp = gpar(fontsize = 8),
                                                                       		 title_gp = gpar(fontsize = 9)))

	}
	else {
		hm.width <- 8
		hm.height <- 6
		empty.df <- "NO bottom annotations given."
		write.table(empty.df, "coreSampleswithAnnotations.txt",
				sep = "\t", quote = FALSE, row.names = FALSE)
		ha.bottom.subtypes <- ha.top.subtypes
	}

    # Saturate protein abundance values from -2 to 2.
    hm.col.bks <- c(-2, 0, 2)
    hm.cols <- c("blue", "white", "red")
    hm.cols.palette <- colorRamp2(hm.col.bks, hm.cols)
    pdf(hm.subtype.pdffile, width=hm.width, height=hm.height)
    hm.subtype <- Heatmap(sample.data.matrix,
              			  top_annotation = ha.top.subtypes,
			              bottom_annotation = ha.bottom.subtypes,
			              na_col = "lightgrey",
            			  show_row_names = F,
			              show_row_dend = F,
			              column_title = prot.st.colname,
			              col = hm.cols.palette, 
			              show_column_names = F,
			              show_column_dend = F,
			              cluster_columns = F,
			              cluster_rows = F,
			              heatmap_legend_param = list(title = "Relative\nProtein\nAbundance\n(log2)",
            			  							  color_bar = "continuous",
              										  title_position = "leftcenter",
              										  labels_gp = gpar(fontsize = 8),
			              							  title_gp = gpar(fontsize = 10)))
	draw(hm.subtype, heatmap_legend_side = "left", annotation_legend_side = "top")
	# Add labels to the left for bottom annotations only
	for(annot.name in subtype.colnames) { 
    	decorate_annotation(annot.name, {
    		grid.text(label = annot.name,
    				  x = 0, y = 0.5,
					  gp = gpar(cex = 0.7),
					  just = "right")
    	})
    }
	dev.off()
	return(hm.subtype)
}

# Perform normalization, consensus clustering, select core samples, identify subtypes and plot heatmap with subtype annotations
performProtSubtyping <- function(prot.matrix, 
									hm.ann.df, 
									prot.st.colname, 
									sample.colname,
									geneid.colname,
									hm.subtype.pdffile, 
									sp.pdffile,
									add.bottom.ann, ...){
  
  
  #Center the data as needed. Use the function by Xiaojing to replicate the process:
  cc.input.matrix <- normalizeDataBeforeClustering(prot.matrix)

  cc.list <- ConsensusClusterPlus(cc.input.matrix, ...)

  cluster.k <- selectTrueK(cc.list)
  
  cc.sample.classes <- cc.list[[cluster.k]]$consensusClass
  cc.class.filt.df <- filterSmallClusters(cc.sample.classes,
                                          sample.colname = sample.colname,
                                          cluster.colname = prot.st.colname)

  sample.ids.from.clusters <- unique(cc.class.filt.df[,sample.colname])
  
  # Get sample_ids from filtered clustsers.
  # Select "Core" samples based on positive silhouette scores for the selected subtypes:
  core.sample.df <- identifyCoreSamples(cc.input.matrix,
                                         sample2clu = cc.class.filt.df,
                                         id.colname = sample.colname,
                                         cluster.colname = prot.st.colname,
                                         sp.pdffile = sp.pdffile)
  core.sample.ids <- core.sample.df[,sample.colname]
  write.table(core.sample.ids, "coreSamples.txt", sep="\t", row.names = F, quote = F)
  core.samples.matrix <- cc.input.matrix[,colnames(cc.input.matrix) %in% core.sample.ids]
  
  pam.analysis.genes.matrix.list <- pamAnalysis(core.samples.matrix,
                                                sample.colname = sample.colname,
                                                sample.cluster.map.df = core.sample.df,
                                                cluster.colname = prot.st.colname)
  
  pam.filt.matrix <- subtypeFormatMatrix(cc.input.matrix,
                                         pam.analysis.genes.matrix.list,
                                         sample.cluster.map.df = cc.class.filt.df,
                                         cluster.colname = prot.st.colname)

  cc.class.core.df <- cc.class.filt.df[cc.class.filt.df[,sample.colname] %in% core.sample.ids,]
  cc.class.core.df <- cc.class.core.df[order(cc.class.core.df[,prot.st.colname]),]
  write.table(cc.class.core.df, "subtypeSampleMapping.txt",
              sep="\t", row.names = F, quote = F)
  
  pam.core.samples.matrix <- pam.filt.matrix[,cc.class.core.df[,sample.colname]]
  write.table(pam.core.samples.matrix, "heatmapInputMatrix.txt",
              sep="\t", row.names = T, quote = F)
  
  # Make this another script?
  hm.subtype <- heatmapWithProtSubtypeAnn(sample.data.matrix = pam.core.samples.matrix,
                                          sample.cluster.map.df = cc.class.core.df,
                                          hm.ann.df = hm.ann.df,
                                          hm.subtype.pdffile = hm.subtype.pdffile,
                                          sample.colname = sample.colname,
                                          prot.st.colname = prot.st.colname,
                                          add.bottom.ann)
	return(list(cc.input.matrix, cc.class.core.df, core.sample.ids, pam.core.samples.matrix, hm.subtype))
}


#Input dataframe with values to cluster - samples as columns and genes as rows.
findProtSubtypes <- function() {
  args <- commandArgs(TRUE)
  input.file <- args[1]
  sampleid.file <- args[2]
  hm.ann.file <- args[3]
  max.clusters <- args[4]
  cluster.method <- args[5]
  distance.method <- args[6]
  inner.linkage.type <- args[7]
  final.linkage.type <- args[8]
  subsample.protein.count <- args[9]
  subsample.sample.count <- args[10]
  subsamples.number <- args[11]
  cc.title <- args[12]
  plot.type <- args[13]
  sample.colname <- args[14]
  geneid.colname <- args[15]
  prot.st.colname <- args[16]
  add.bottom.ann <- args[17]
  hm.subtype.pdffile <- args[18]
  sp.pdffile <- args[19]
  
  if(is.na(max.clusters)) {
    max.clusters <- 8
  } else {
    max.clusters <- as.numeric(max.clusters)
  }
  
  if(is.na(cluster.method)) {
    cluster.method <- "hc"
  }
  
  if(is.na(distance.method)) {
    distance.method <- "pearson"
  }
  
  if(is.na(inner.linkage.type)) {
    inner.linkage.type <- "average"
  }
  
  if(is.na(final.linkage.type)) {
    final.linkage.type <- "average"
  }
  
  if(is.na(subsamples.number)) {
    subsamples.number <-  1000
  } else {
    subsamples.number <- as.numeric(subsamples.number)
  }
  
  if(is.na(subsample.protein.count)) {
    subsample.protein.count <- 0.8
  } else {
    subsample.protein.count <- as.numeric(subsample.protein.count)
  }
  
  if(is.na(subsample.sample.count)) {
    subsample.sample.count <- 0.8
  } else {
    subsample.sample.count <- as.numeric(subsample.sample.count)
  }
  
  if(is.na(cc.title)) {
    cc.title <- "ConsensusClusterPlus"
  }
  
  if(is.na(plot.type)) {
    plot.type <- "pdf"
  }
  if(is.na(prot.st.colname)) {
    prot.st.colname <- "prot_subtype"
  }
  
  
  if(is.na(sample.colname)) {
    sample.colname <- "sample_id"
  }
  
  if(is.na(geneid.colname)) {
    geneid.colname <- "GeneId"
  }
  if(is.na(add.bottom.ann)) {
    add.bottom.ann <- FALSE
  }
  
  input.df <- read.delim(input.file, header=T, stringsAsFactors = FALSE, check.names = FALSE)
  sampleid.df <- read.delim(sampleid.file, header = F, stringsAsFactors = F)
  sample.ids <- sampleid.df[,1]
  hm.ann.df <- read.delim(hm.ann.file, header=T, stringsAsFactors = FALSE, check.names = FALSE)

  input.matrix <- as.matrix(input.df[,colnames(input.df) %in% sample.ids])
  subtyping.reqdata.list <- performProtSubtyping(input.matrix, 
  												hm.ann.df,
												prot.st.colname, 
												sample.colname,
												geneid.colname,
												hm.subtype.pdffile,
												sp.pdffile,
												add.bottom.ann,
                                				clusterAlg=cluster.method,
                                  				distance=distance.method,
                                  				maxK=max.clusters,
                                  				innerLinkage=inner.linkage.type,
                                  				finalLinkage=final.linkage.type,
				                                pItem=subsample.protein.count,
				                                pFeature=subsample.sample.count,
				                                reps=subsamples.number,
				                                title=cc.title,
				                                plot=plot.type,
				                                writeTable=TRUE)
}

findProtSubtypes()
