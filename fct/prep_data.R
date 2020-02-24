# read-in and prep for Jres browser

readIn_jresbrowser=function(path, fname, sidx){
  require(MetaboMate)
  require(jres)
  
  if(!is.null(sidx)){
    s.idx=c(sidx, sidx+1)
  }
  sample.idx=1
  dat=readBruker2d(path, n=2)

  # read and prep matching 1D
  readBruker(path)
  ppm=as.numeric(colnames(X))
  idx=match(names(dat), gsub('0$', '1', rownames(X)))
  X=X[idx,]
  X[is.na(X)]=0
  meta=meta[idx,]
  qc=spec.quality(X, ppm)
  Xbl=bline(X)
  meta$lw=qc$TSP.lw.ppm

  sample.idx=1
  jr=dat[[sample.idx]]
  noi=noise2D(jr)
  Xbl=Xbl[sample.idx,]

  fiID=paste0(path, rownames(meta)[1])

  # pick peaks
  js=jr
  f1hz=as.numeric(rownames(js))
  f2ppm=as.numeric(colnames(js))


  # exlude water and TSP
  idx.c.rm=c(get.idx(c(min(f2ppm), 0.6), f2ppm), get.idx(c(4.7,5), f2ppm), get.idx(c(9.7, max(f2ppm)), f2ppm))
  #idx.r.rm=c(get.idx(c(25, max(f1hz)), f1hz), get.idx(c(min(f1hz), -25), f1hz))

  js[, idx.c.rm]=0

  idx.1d=c(get.idx(c(min(ppm), 0.6), ppm), get.idx(c(4.7,5), ppm), get.idx(c(9.7, max(ppm)), ppm))
  Xbl[idx.1d]=0

  # plotjres_overlay1D_bboxNull(js, spec.1d = Xbl, ppm.1d = ppm, SF=meta$a_SFO1[1], bbox=NULL, t2.lim = c(4.6,5.1), t1.lim = c(-15,15))

  a=Sys.time()
  pl=pickPeaks_rcpp(js, as.numeric(rownames(js)), as.numeric(colnames(js)), noi, sf=meta$a_SFO1[1], boundary=10)
  plist=do.call(rbind, pl)
  b=Sys.time()

  print(a-b)
  print(nrow(pl))

  #plotjres_overlay1D_bboxNull(log(js), spec.1d = Xbl, ppm.1d = ppm, SF=meta$a_SFO1[1], bbox=plist, t2.lim = c(3,3.1), t1.lim = c(-40, 40))

  plist=plist[order(plist$Int, decreasing = T),]
  plist$featID=1:nrow(plist)

  sf=meta$a_SFO1[1]
  f1hz=as.numeric(rownames(jr))
  f2ppm=as.numeric(colnames(jr))
  f2hz=f2ppm*sf

  # 
  # return(list(
  #   path,
  #   jr, 
  #   plist, 
  #   noi, Xbl, 
  #   ppm, 
  #   meta, 
  #   fiID, 
  #   sf, 
  #   f1hz, 
  #   f2ppm, 
  #   f2hz
  # ))
  
  outname=paste0(fname, 'Rdata')
  cat('Data stored in', outname)
  save(jr, plist, noi, Xbl, ppm, meta, fiID, sf, f1hz, f2ppm, f2hz, file=outname)

}