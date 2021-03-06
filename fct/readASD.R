library(MetaboMate)
library(plyr)
load('../jRes/NMR_Ringtest.Rdata')

sqc=spec.quality(X, ppm)
an=data.frame(File=rownames(X), ExpNo=as.numeric(basename(rownames(X))), SpecNo=dirname(rownames(X)))
an$SF=round(meta$a_SFO1, 4)
an=ddply(an, .(SpecNo), function(x){
  #browser()
  x$Usample=NA
  out=list()
  for(i in 1:6){
    out[[i]]=c(rep(LETTERS[i], each=8), 'QC')
  }
  
  x$Usample[order(x$ExpNo)]=unlist(out)
  x
})

idx=match(gsub('1$', '0', names(dat)), an$File)
an=an[idx,]


idxC=which(an$Usample=='C')

idx.sample=idxC[24]

noise=noise2D(dat[[idx.sample]])
sf=meta$a_SFO1[idx.sample]

f2ppm=c(1,2)
f1hz=c(-7,7)
idxf2=get.idx(f2ppm,  as.numeric(colnames(dat[[1]])))
idxf1=get.idx(f1hz,  as.numeric(rownames(dat[[1]])))


jr=dat[[idx.sample]][idxf1, idxf2]

# 

#jr=dat[[idx.sample]]
a=Sys.time()
test=pickPeaks_rcpp(jr=jr, f1hz = as.numeric(rownames(jr)), f2ppm = as.numeric(colnames(jr)), noise = noise*1, boundary = 10, sf =sf)
b=Sys.time()
b-a

cl=makeCluster(10)
registerDoParallel(cl)
a=Sys.time()

ffiles=list.files('/Volumes/TK_1/ASD U/NMR/Autism_Urine_Rack01_RFT_150817', full.names = T, pattern = '[1-9]1$', recursive = F)

out=foreach(i=1:length(ffiles[1:10]), fls=ffiles, .packages = c('MetaboMate','Rcpp', 'jRes', 'fields'), .noexport = c('convolve3cpp', 'd2gauss_cpp', 'expandGrid_rcpp', 'getBBdim_rcpp', 'getBoundary', 'logOfG', 'matpow', 'matsum', 'matsumup', 'MinmaxMat', 'pickPeaks_rcpp')) %dopar% {
  
  Rcpp::sourceCpp('/Volumes/Torben_1/Rproj/JresSVM/scr/convolve3cpp.cpp')
  Rcpp::sourceCpp('/Volumes/Torben_1/Rproj/JresSVM/scr/d2gauss_cpp.cpp')
  Rcpp::sourceCpp('/Volumes/Torben_1/Rproj/JresSVM/scr/other.cpp')
  
  readBruker2dd=function(datapath, n){
    #require(fields)
    ppm <- NULL
    
    # searches for f2 processing parameter files
    pfile <- list.files(path = datapath, pattern = "^procs$",
                        all.files = FALSE, full.names = TRUE, recursive = TRUE,
                        ignore.case = TRUE)
    pfile=grep('pdata/1/', pfile, value=T)
    
    
    # searches for f1 processing parameter files
    p2file <- list.files(path = datapath, pattern = "^proc2s$",
                         all.files = FALSE, full.names = TRUE, recursive = TRUE,
                         ignore.case = TRUE)
    p2file=grep('pdata/1/', p2file, value=T)
    print(length(p2file))
    if(n<length(p2file)){p2file=p2file[1:n]; print(p2file)}
    
    # filter pfiles for 2d experiments by experiment numbers
    if(n==1){
      out=strsplit(gsub('/pdata/1/procs$', '', pfile), split = '/')[[1]]
      exp.no=out[[length(out)]]
      
      out=strsplit(gsub('/pdata/1/proc2s$', '', p2file), split = '/')[[1]]
      exp.no2=out[[length(out)]]
    }else{
      exp.no=gsub('/pdata/1/procs$', '', gsub(paste('^', datapath, '/', sep=''), '', pfile))
      exp.no2=gsub('/pdata/1/proc2s', '', gsub(paste('^', datapath, '/', sep=''), '', p2file))
      
    }
    
    pfile=pfile[exp.no %in% exp.no2]
    exp.no=exp.no[exp.no %in%  exp.no2]
    
    # searches for 2rr files
    rfile <- list.files(path = datapath, pattern = "^2rr$", all.files = FALSE,
                        full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    rfile=grep('pdata/1/', rfile, value=T)
    
    if(n==1){
      out=strsplit(gsub('/pdata/1/2rr$', '', rfile), split = '/')[[1]]
      exp.no2rr=out[[length(out)]]
    }else{
      exp.no2rr=gsub('/pdata/1/2rr$', '', gsub(paste('^', datapath, '/', sep=''), '', rfile))
    }
    
    # check if required files are present
    idx1=which(!exp.no2rr %in% exp.no)
    idx2=which(!exp.no %in% exp.no2rr)
    if(length(idx1)>0){
      cat('File proc2s or procs file is missing for experiment No',  exp.no2rr[idx1]);
      rfile=rfile[exp.no2rr %in% exp.no]}
    if(length(idx2)>0){
      cat('File 2rr is missing for experiment No',  exp.no[idx2]);
      pfile=pfile[exp.no %in% exp.no2rr]
      p2file=p2file[exp.no %in% exp.no2rr]
    }
    
    L <- length(pfile)
    L2<-length(p2file)
    Lr <- length(rfile)
    sa <- NULL
    snam <- NULL
    if (L == 0 || Lr == 0 || L != Lr) {
      return(cat("Bruker file does not exist in datapath, or other problems with bruker files...\n"))
    } else {
      out=apply(rbind(1:Lr), 2, function(i, pf=pfile, p2f=p2file, rf=rfile){
        print(i)
        
        # extract procs information for t2
        con <- file(pf[i], open = "r")
        aLine <- readLines(con, n = -1, warn = FALSE)
        myV <- strsplit(aLine, "=")
        close(con)
        ftsize <- 70000
        for (j in 1:length(myV)) {
          if (match("##$OFFSET", myV[[j]][1], nomatch = 0)) {
            offset <- as.numeric(myV[[j]][2])
            #print(offset)
          }
          if (match("##$SW_p", myV[[j]][1], nomatch = 0)) {
            sw <- as.numeric(myV[[j]][2])
          }
          if (match("##$SF", myV[[j]][1], nomatch = 0)) {
            sf <- as.numeric(myV[[j]][2]) # Radiopulse frequency (slightly different for each NMR spectrometer, leading to slightly different ppm scales)
            #print(sf)
          }
          if (match("##$SI", myV[[j]][1], nomatch = 0)) {
            si <- as.numeric(myV[[j]][2])
          }
          if (match("##$BYTORDP", myV[[j]][1], nomatch = 0)) {
            bytordp <- as.numeric(myV[[j]][2])
          }
          if (match("##$NC_proc", myV[[j]][1], nomatch = 0)) {
            ncproc <- as.numeric(myV[[j]][2])
          }
          if (match("##$FTSIZE", myV[[j]][1], nomatch = 0)) {
            ftsize <- as.numeric(myV[[j]][2])
          }
        }
        if (bytordp == 0) {
          machine_format = "little"
        } else {
          machine_format = "big"
        }
        rm(myV)
        
        # extract procs information for t1
        con2 <- file(p2f[i], open = "r")
        a2Line <- readLines(con2, n = -1, warn = FALSE)
        myV2 <- strsplit(a2Line, "=")
        close(con2)
        for (j in 1:length(myV2)) {
          if (match("##$OFFSET", myV2[[j]][1], nomatch = 0)) {
            offset2 <- as.numeric(myV2[[j]][2])
            #print(offset)
          }
          if (match("##$SW_p", myV2[[j]][1], nomatch = 0)) {
            sw2 <- as.numeric(myV2[[j]][2])
          }
          if (match("##$SF", myV2[[j]][1], nomatch = 0)) {
            sf2 <- as.numeric(myV2[[j]][2]) # Radiopulse frequency (slightly different for each NMR spectrometer, leading to slightly different ppm scales)
          }
          if (match("##$SI", myV2[[j]][1], nomatch = 0)) {
            si2 <- as.numeric(myV2[[j]][2])
          }
          if (match("##$BYTORDP", myV2[[j]][1], nomatch = 0)) {
            bytordp2 <- as.numeric(myV2[[j]][2])
          }
          if (match("##$NC_proc", myV2[[j]][1], nomatch = 0)) {
            ncproc2 <- as.numeric(myV2[[j]][2])
          }
          if (match("##$FTSIZE", myV2[[j]][1], nomatch = 0)) {
            ftsize2 <- as.numeric(myV2[[j]][2])
          }
        }
        
        # read-in binary file
        
        s <- readBin(rf[i], what = "int", n = ftsize*ftsize2,
                     size = 4, signed = T, endian = machine_format) # this is spectra
        s <- ((2^ncproc) * s)
        nspec <- length(s)
        
        # generate ppm values t2
        tmpppm <- ppm
        swp <- sw/sf # sweep width / radio pulse frequency for standardised ppm scale
        dppm <- swp/(si - 1) # how much difference is there between each ppm variable (swp/number of spectral points -1)
        ppm <- offset
        ppm <- seq(offset, (offset - swp), by = -dppm)
        
        # generate ppm values t1
        swp2 <- sw2/sf2 # sweep width / radio pulse frequency for standardised ppm scale
        dppm2 <- swp2/(si2 - 1) # how much difference is there between each ppm variable (swp/number of spectral points -1)
        ppm2 <- offset2
        ppm2 <- seq(offset2, (offset2 - swp2), by = -dppm2)
        
        ppm2=ppm2*sf2
        
        # reshape into 2d matrix
        test=matrix(s, nrow = si, ncol=si2)
        test=t(test)
        
        test=as.data.frame(test, stringsAsFactors = F)
        colnames(test)=ppm
        rownames(test)=ppm2
        
        # test$ppm2=ppm2
        # test=melt(test, id.vars = 'ppm2')
        # test$ppm2=as.numeric(ppm2)
        # test$variable=as.numeric(as.character(test$variable))
        #
        #
        # ggplot(data=subset(test, variable<2.7 & variable>2.45), aes(x=variable, y=ppm2, z=value))+geom_contour()+scale_x_reverse()
        #
        # # rotate by 45 degrees
        # rotate <- function(df, degree) {
        #   dfr <- df
        #   degree <- pi * degree / 180
        #   l <- sqrt(df[,1]^2 + df[,2]^2)
        #   teta <- atan(df[,2] / df[,1])
        #   dfr[,1] <- (l * cos(teta - degree))
        #   dfr[,2] <- (l * sin(teta - degree))
        #   return(dfr)
        # }
        #
        # tt=cbind(test$ppm2, test$variable) %*% matrix(c(cos(45), -sin(45), sin(45), cos(45)), ncol=2, byrow = T)
        # testr[,1:2]=tt
        #
        # ggplot(data=subset(test, variable<4.1 & variable>4), aes(x=variable, y=ppm2, z=value))+
        #   geom_contour()+scale_x_reverse()
        
        return(test)
        #return(offset)
      })
      
      cat('Interpolate 2D grid.\n')
      # bring t2 ppm on the same points (interpolating grid)
      out.sc=lapply(1:length(out), function(i){
        print(i)
        df=out[[i]]
        obj<- list( x= as.numeric(rownames(df)), y=as.numeric(colnames(df)), z= df)
        
        t1.temp<- c(min(obj$x), seq( -38,38,0.30), max(obj$x))
        t2.temp<- c(obj[[2]][which.min(abs((-1)-(obj[[2]])))], seq(-1,10,, ncol(df)-2), obj[[2]][which.min(abs((10)-(obj[[2]])))-1])
        loc=make.surface.grid( list( t1.temp,t2.temp))
        look=interp.surface( obj, loc)
        s1=as.surface( loc, look)
        
        df=s1$z[,-c(1,ncol(s1$z))]
        colnames(df)=s1$y[-c(1,ncol(s1$z))]
        rownames(df)=s1$x
        
        return(df)
      })
      
      
      
      names(out.sc)=gsub(paste('^', datapath, '/', sep=''), '', gsub('/pdata/1/procs', '', pfile))
      return(out.sc)
    }
  }
  
  
  
  
  jr=readBruker2dd(fls[i], n=1)[[1]]
  #return(jr)
  noise=noise2D(jr)
  sf=600
  idxf2=c(get.idx(c(.5, 4.7), as.numeric(colnames(jr))), get.idx(c(5,10), as.numeric(colnames(jr))))
  idxf1=get.idx(c(-20, 20), as.numeric(rownames(jr)))
  jr=jr[idxf1, idxf2]

  test=pickPeaks_rcpp(jr=jr, f1hz = as.numeric(rownames(jr)), f2ppm = as.numeric(colnames(jr)), noise = as.numeric(noise)*2, boundary = 10, sf =sf)

  pl=do.call(rbind, test)
  pl$sample=fls[i]
  return(pl)

} 
b=Sys.time()
b-a
stopCluster(cl)

pl=do.call(rbind, out)


ggplot(pl, aes(cent.f2, cent.f1, colour=Int, size=Int))+
  geom_point( alpha=0.3)+
  scale_colour_gradientn(colours=matlab.like2(10), trans='log10')+
  theme_bw()+
  scale_x_reverse()+
  facet_wrap(~file)




