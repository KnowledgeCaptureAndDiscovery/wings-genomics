
addNewVarsToData <- function(long.snv.saav.maf.df,
								sample.colname,
								saav.colname,
								dbsnp.colname,
								cosmic.colname,
								tcga.som.colname,
								new.var.colname,
								new.var.id,
								var.dbid.colname,
								var.db.colname) {
	var.db.temp.colname <- "temp_var_db"
	long.snv.saav.maf.df[,new.var.colname] <- ifelse(is.na(long.snv.saav.maf.df[,dbsnp.colname]) &
    	is.na(long.snv.saav.maf.df[,cosmic.colname]) &
	    is.na(long.snv.saav.maf.df[,tcga.som.colname]), new.var.id, NA)

	long.snv.saav.maf.newvars.df <- melt(unique(long.snv.saav.maf.df[,c(sample.colname,
    	                                saav.colname,
        	                            dbsnp.colname,
            	                        cosmic.colname,
                	                    tcga.som.colname,
                    	                new.var.colname)]),
	     id.vars = c(sample.colname, saav.colname),
    	 variable.name = var.db.temp.colname,
	     value.name = var.dbid.colname)

	# Remove the NA values from new - redundant
	long.snv.saav.maf.newvars.df <- long.snv.saav.maf.newvars.df[!is.na(long.snv.saav.maf.newvars.df[,var.dbid.colname]),]
	
	long.snv.saav.maf.newvars.df[,var.db.colname] <-  ifelse(long.snv.saav.maf.newvars.df[,var.db.temp.colname] == dbsnp.colname,
       dbsnp.out.colname, ifelse(long.snv.saav.maf.newvars.df[,var.db.temp.colname] == cosmic.colname, cosmic.out.colname,
        ifelse(long.snv.saav.maf.newvars.df[,var.db.temp.colname] == tcga.som.colname, 
        tcga.som.colname, ifelse(long.snv.saav.maf.newvars.df[,var.db.temp.colname] == new.var.colname, 
        new.var.id, NA))))	

	long.snv.saav.maf.newvars.df <- unique(long.snv.saav.maf.newvars.df[,!colnames(long.snv.saav.maf.newvars.df) %in%
																 var.db.temp.colname])
	write.table(long.snv.saav.maf.newvars.df, data.withnewvars.outfile,
				row.names = F, quote = F, sep = "\t")
	
	return(long.snv.saav.maf.newvars.df)
}

longFormatSAAVSNV <- function(saav.snv.df,
							  sample.colname,
							  sc.colname,
							  saav.colname, 
							  pro.colname, 
							  aamod.colname, 
							  dbsnp.colname, 
							  cosmic.colname,
							  saav.rm.colnames) {
	id.var.colnames <- c(saav.colname, pro.colname,
						 aamod.colname, 
						 dbsnp.colname, cosmic.colname)
	long.snv.saav.df <- melt(unique(saav.snv.df[,!colnames(saav.snv.df)
                                            %in% saav.rm.colnames]),
    						 id.vars = id.var.colnames,
						     variable.name = sample.colname,
						     value.name = sc.colname)
	long.snv.saav.df <- long.snv.saav.df[long.snv.saav.df[,sc.colname] != 0,]
	return(long.snv.saav.df)
}

mergeSNVSAAVsAndSNVMAFs <- function(saav.snv.df,
									long.snv.maf.df, 
					 				saav.snv.df,
					 				sample.colname,
								    sc.colname,
								    saav.colname, 
								    pro.colname, 
								    aamod.colname, 
								    dbsnp.colname, 
								    cosmic.colname,
								    tcga.som.colname,
								    saav.rm.colnames,
								    new.var.colname,
								    new.var.id,
								    var.dbid.colname,
								    var.db.colname) {

	long.snv.saav.df <- longFormatSAAVSNV(saav.snv.df,
							  			  sample.colname,
										  sc.colname,
										  saav.colname, 
										  pro.colname, 
										  aamod.colname, 
										  dbsnp.colname, 
										  cosmic.colname,
										  saav.rm.colnames)

	snv.maf.sample.ids <- unique(long.snv.maf.df[,sample.colname])
	long.snv.saav.df[,sample.colname] <- gsub(paste0("(.*)(",
                                               paste(snv.maf.sample.ids, collapse = "|"),
                                               ")(.*)"),
                                               "\\2",
                                               long.snv.saav.df[,sample.colname])
	
	long.snv.saav.maf.df <- merge(unique(long.snv.saav.df[,!colnames(long.snv.saav.df)
                                                        %in% sc.colname]),
                                  long.snv.maf.df,
                                  by = c(pro.colname,
                                         sample.colname,
                                         aamod.colname),
                                  all.x = T)

	long.snv.saav.maf.newvars.df <- addNewVarsToData(long.snv.saav.maf.df,
													 sample.colname,
													 saav.colname,
													 dbsnp.colname,
													 cosmic.colname,
													 tcga.som.colname,
													 new.var.colname,
													 new.var.id,
													 var.dbid.colname,
													 var.db.colname)
	return(long.snv.saav.maf.df)
}

combineSNVSAAVMAF <- function() {
	args = commandArgs(trailingOnly=TRUE)
	saav.snv.file <- args[1]
	long.snv.maf.file <- args[2]
	sample.colname <- args[3]
	sc.colname <- args[4]
	saav.colname <- args[5]
	pro.colname <- args[6]
	aamod.colname <- args[7]
	dbsnp.colname <- args[8]
	cosmic.colname <- args[9]
	tcga.som.colname <- args[10]
	saav.rm.colnames.str <- args[11]
	new.var.colname <- args[12]
	new.var.id <- args[13]
	var.dbid.colname <- args[14]
	var.db.colname <- args[15]
	dbsnp.out.colname <- args[16]
	cosmic.out.colname <- args[17]
	snv.saav.maf.outfile <- args[18]
	
	saav.snv.df <- read.delim(saav.snv.file, header = T,
							  stringsAsFactors = F, check.names = F)
	long.snv.maf.df <- read.delim(long.snv.maf.file, header = T,
							  	  stringsAsFactors = F, check.names = F)

	saav.rm.colnames <- unlist(strsplit(saav.rm.colnames.str, ","))

	print("Merge SNV+SAAV, SNV+MAF files and format the dataframe for plotting...")
	long.snv.saav.maf.df <- mergeSNVSAAVsAndSNVMAFs(saav.snv.df,
									long.snv.maf.df, 
					 				sample.colname,
					 				sc.colname,
					 				pro.colname,
					 				aamod.colname,
					 				id.var.colnames,
					 				snv.saav.maf.outfile)

	print("Done!")
	print(paste("Filename:", snv.saav.maf.outfile))
}

combineSNVSAAVMAF()
