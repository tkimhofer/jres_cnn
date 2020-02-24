
#path='/Volumes/TK_1/ASD U/NMR/Autism_Urine_Rack01_RFT_150817/10/'

#readB1d('/Volumes/TK_1/ASD U/NMR/Autism_Urine_Rack01_RFT_150817/10/')
readB1d=function (path, filter = T, n=1) 
{
  warnDef <- options("warn")$warn
  warnRead <- options(warn = -1)
  if (grepl("/$", path)) {
    path <- gsub("/$", "", path)
  }

  # check dir level
  if(length(list.files(path, pattern="^acqus$"))>0){
    # path includes spectrum folder
    n=1
    ind='file_level'
  }else{
    ind='folder_level'
  }
  
  afile <- list.files(path = path, pattern = "^acqus$", all.files = FALSE, 
                      full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  pfile <- list.files(path = path, pattern = "^procs$", all.files = FALSE, 
                      full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  pfile <- grep("pdata/1/", pfile, value = T)
  rfile <- list.files(path = path, pattern = "^1r$", all.files = FALSE, 
                      full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  rfile <- grep("pdata/1/", rfile, value = T)
  
  if(n<length(pfile)){pfile=pfile[1:n]; print(pfile)}
  
  
  Lp <- length(pfile)
  if (Lp == 0 || length(rfile) == 0 || length(afile) == 0) {
    stop("No existing Bruker files in specified path.")
  }
  
  
  if(ind=='folder_lev'){
    a.exp <- gsub(paste("^", path, "/|/acqus$", sep = ""), "", 
                  afile)
    p.exp <- gsub(paste("^", path, "/|/pdata/1/procs$", sep = ""), 
                  "", pfile)
    r.exp <- gsub(paste("^", path, "/|/pdata/1/1r$", sep = ""), 
                  "", rfile)
    idx.a <- a.exp %in% r.exp & a.exp %in% p.exp
    idx.p <- p.exp %in% r.exp & p.exp %in% a.exp
    idx.r <- r.exp %in% a.exp & r.exp %in% p.exp
    
    
    if (filter == T) {
      afile <- afile[idx.a]
      pfile <- pfile[idx.p]
      rfile <- rfile[idx.r]
    }
    else {
      not.present <- list()
      not.present[[1]] <- afile[!idx.a]
      not.present[[2]] <- pfile[!idx.p]
      not.present[[3]] <- rfile[!idx.r]
      if (length(not.present[[1]]) > 0) {
        cat("File structure depreciated: missing Bruker acquisition file(s). See output for file names.\n")
      }
      if (length(not.present[[2]]) > 0) {
        cat("File structure depreciated: missing Bruker processing file(s). See output for experiment folder names.\n")
      }
      if (length(not.present[[3]]) > 0) {
        cat("File structure depreciated: missing aquisition file(s). See output for experiment folder names.\n")
      }
      if (sum(sapply(not.present, length)) > 0) {
        return(sort(unlist(not.present)))
      }
    }
    
  }else{
    if(length(afile)!=1 | length(pfile)!=1 | length(rfile)!=1){
      cat("File structure depreciated: missing acqus, procs or 1r file.\n")
      return(NULL)
    }
    a.exp <- afile
    p.exp <- pfile
    r.exp <- rfile
    
  }
  
  
  Lp <- length(pfile)
  if (Lp == 0) {
    stop("No matching Bruker files in specified path.")
  }
  if (Lp == 1) {
    cat("Reading ", Lp, " spectrum.\n", sep = "")
  }
  else {
    cat("Reading ", Lp, " spectra.\n", sep = "")
  }
  out <- apply(rbind(1:Lp), 2, function(i, pf = pfile, rf = rfile, 
                                        af = afile) {
    con <- file(af[i], open = "r")
    aLine <- readLines(con, n = -1, warn = FALSE)
    close(con)
    aLine <- aLine[-c(1:9)]
    aLine <- aLine[!grepl("##END", aLine)]
    idx <- grep("#", aLine)
    aLineC <- array()
    for (j in 1:length(idx)) {
      if (grepl("#", aLine[idx[j] + 1])) {
        aLineC[j] <- aLine[idx[j]]
        next
      }
      end_idx <- grep("#", aLine[(idx[j] + 1):length(aLine)])[1]
      aLineC[j] <- paste(aLine[idx[j]:(idx[j] + end_idx - 
                                         1)], collapse = " ")
    }
    aLineC <- gsub(" \\(.*\\)", "", aLineC)
    myV <- strsplit(aLineC, "=")
    df.acqu <- unique(as.data.frame(do.call(rbind, myV), 
                                    stringsAsFactors = F))
    df.acqu$V1 <- paste("a", gsub("##\\$", "", df.acqu$V1), 
                        sep = "_")
    df.acqu$V2 <- gsub("^ | $", "", df.acqu$V2)
    con <- file(pf[i], open = "r")
    aLine <- readLines(con, n = -1, warn = FALSE)
    myV <- strsplit(aLine, "=")
    close(con)
    idx <- grepl("##", aLine) & sapply(myV, length) == 2
    myV <- myV[idx]
    df.proc <- unique(as.data.frame(do.call(rbind, myV[1:(length(myV) - 
                                                            1)]), stringsAsFactors = F))
    df.proc$V1 <- paste("p", gsub("^##[\\$]?", "", df.proc$V1), 
                        sep = "_")
    df.proc$V2 <- gsub("^ | $", "", df.proc$V2)
    meta <- rbind(df.acqu, df.proc)
    if (as.numeric(meta$V2[grep("p_BYTORDP", meta$V1)]) != 
        0) {
      endianness <- "big"
    }
    else {
      endianness <- "little"
    }
    spec <- readBin(rf[i], what = "int", n = as.numeric(meta$V2[grep("p_FTSIZE", 
                                                                     meta$V1)]), size = 4, signed = T, endian = endianness)
    spec <- ((2^as.numeric(meta$V2[grep("p_NC_proc", meta$V1)])) * 
               spec)
    swp <- as.numeric(meta$V2[grep("p_SW_p", meta$V1)])/as.numeric(meta$V2[grep("^p_SF$", 
                                                                                meta$V1)])
    dppm <- swp/(length(spec) - 1)
    offset <- as.numeric(meta$V2[grep("OFFSET", meta$V1)])
    ppm <- seq(from = offset, to = (offset - swp), by = -dppm)
    return(list(meta, spec, ppm))
  })
  #browser()
  ids <- unique(as.vector(unlist(sapply(out, function(x) x[[1]]$V1))))
  meta <- data.frame(t(sapply(out, function(x) {
    x[[1]] <- unique(x[[1]])
    x[[1]]$V2[match(ids, x[[1]]$V1)]
  })), stringsAsFactors = F)
  colnames(meta) <- ids
  nums <- sapply(meta, function(x) {
    suppressWarnings(as.numeric(x))
  })
  if(ind=='file_level'){
    idx=which(!is.na(nums));
    meta[, idx] <- nums[idx]
  }else{
      idx <- complete.cases(t(nums))
      meta[, idx] <- nums[, idx]
      }
  
  
  meta$a_Date <- strptime("1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%M") + 
    as.numeric(meta$a_DATE)
  meta$a_RunOrder <- rank(meta$a_Date, na.last = "min")
  rownames(meta) <- r.exp
  ppm <- seq(meta$p_OFFSET[1], meta$p_OFFSET[1] - meta$a_SW[1], 
             -(meta$a_SW[1]/meta$a_TD[1]))
  if (length(unique(meta$a_TD)) > 1) {
    warning("Some experiments in this folder differ in digital resolution, you might want to filter these out.")
  }
  if (length(unique(round(meta$a_SW), 4)) > 1) {
    warning("The sweep width is different for some experiments in this folder, please make sure you read-in the correct experiments.")
  }
  
  #browser()
  X <- t(sapply(out, function(x, ppmt = ppm) {
    sInter <- approxfun(x[[3]], x[[2]])
    sInter(ppmt)
  }))
  colnames(X) <- ppm
  fnames <- strsplit(dirname(afile), "/")
  rnames <- suppressWarnings(do.call(rbind, fnames))
  idx <- apply(rnames, 2, function(x) length(unique(x)) == 1)
  idx <- which(idx == F)
  
  #browser()
  if (length(idx) > 1) {
    rownames(X) <- sapply(fnames, function(x) {
      paste(x[idx[1]:length(x)], collapse = "/")
    })
  }
  else {
    if(length(idx)==0){
      rownames(X) <- rnames[length(rnames)]
    }else{
      rownames(X) <- rnames[, idx] 
    }
  }
  idx <- unique(which(is.na(X), arr.ind = T)[, 2])
  if (length(idx) >0 & length(idx) < 300) {
    X <- X[, -idx]
    ppm <- ppm[-idx]
  }
  else {
    warning("The ppm range is much different for some experiments, please double check if you read-in the correct spectra!")
  }
  
  assign("X", X, envir = .GlobalEnv)
  assign("ppm", ppm, envir = .GlobalEnv)
  assign("meta", meta, envir = .GlobalEnv)
}