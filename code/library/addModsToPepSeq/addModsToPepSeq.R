strsplit.pepmods.withnames <- function(str.tosplit, split.pattern) {
    split.pattern.list <- ""
    if(split.pattern != "") {
        split.pattern.list <- setNames(lapply(unlist(strsplit(str.tosplit,split.pattern)),
                                              function(x) round(as.numeric(strsplit(x,"@")[[1]][1]))),
                                       as.numeric(substring(sapply(unlist(strsplit(str.tosplit,split.pattern)),
                                                                   function(x) strsplit(x,"@")[[1]][2]),2)))
    } else {
        split.pattern.list <= ""
    }
    return(split.pattern.list)
}

insertCharAtStrPos <- function(var.pep, var.mods) {
    var.mods.split.list <- list()
    var.pep.orig <- var.pep
    if(var.mods != "") {
        var.mods.split.list <- strsplit.pepmods.withnames(var.mods,";")
        test <- vector(mode="numeric",0)
        for (i in 1:length(var.mods.split.list)) {
            pepmod.with.sqlbrkts <- paste0("[",var.mods.split.list[[i]],"]")
            var.mods.pos <- as.numeric(names(var.mods.split.list))[i]
            n <- var.mods.pos
            if(i != 1) {
                pepmod.earlier.with.sqlbrkts <- paste0("[",var.mods.split.list[[i-1]],"]")
            	test <- c(test, nchar(pepmod.earlier.with.sqlbrkts))
                n <- var.mods.pos+(sum(test))
            }
            var.pep <- sub(paste0("(.{",n,"})(.*)"),
                           paste0("\\1",
                                  pepmod.with.sqlbrkts,
                                  "\\2"),
                           var.pep)
        }
    }
    return(var.pep)
}

addModsToPepSeq <- function() {
	args = commandArgs(trailingOnly=TRUE)
	pep.data.file <- args[1]
	pep.seq.colname <- args[2]
	pep.mod.colname <- args[3]
	pep.charge.colname <- args[4]
	pep.withmods.outfile <- args[5]

	pep.data.df <- read.delim(pep.data.file, header = T, stringsAsFactors = F, check.names = F)

	print("Inserting modifications into required positions of the peptide sequence...")	
	pep.data.df[,pep.seq.colname] <- unlist(lapply(1:nrow(pep.data.df),
							              function(x) {pep.mod.seq <- insertCharAtStrPos(pep.data.df[x,pep.seq.colname],
															              pep.data.df[x,pep.mod.colname])
														pep.mod.seq <- paste0(pep.mod.seq,
																				" (+",pep.data.df[x,pep.charge.colname],")")}))
	write.table(pep.data.df, pep.withmods.outfile, row.names = F, sep = "\t", quote = F)
	print("Done!")
	print(paste("Filename:", pep.withmods.outfile))
}

addModsToPepSeq()
