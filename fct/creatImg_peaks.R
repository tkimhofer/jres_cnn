
extr_resize=function(jr, pl, sf){
  
  require(EBImage)
  require(MetaboMate)
  
  f2=as.numeric(colnames(jr))
  f1=as.numeric(rownames(jr))
  
  f2.idx=get.idx(c(pl$cent.f2 - (pl$bb.width.f2/sf), pl$cent.f2 + (pl$bb.width.f2/sf)), f2)
  f1.idx=get.idx(c(pl$cent.f1 - pl$bb.width.f1, pl$cent.f1 + pl$bb.width.f1), f1)
  
  if(length(f1.idx)>1 & length(f2.idx)>1){
    
    sub=jr[f1.idx, f2.idx]
    gis=minmax(resize(sub, w=32, h=32))
    
    return(gis)
  }
  
  return(NULL)
  
}




ppjres_par=function(js, plist, sf, n){
  
  # print(i)
  # print(ddf$files[i])
  
  # library(jres)
  # library(MetaboMate)
  # source('/Volumes/Torben_1/Rproj/jRes/R/filter_multiplicity_new.R')
  # source('/Users/TKimhofer/Rproj/NMRparallel/readB_sf.R')
  # 
  # jres=readB_sf(ddf$files[i], 1)
  # jr=jres[[1]][[1]]
  # sf=jres[[1]][[2]]
  # 
  # 
  # f2ppm=as.numeric(colnames(jr))
  # # f1hz=as.numeric(rownames(jr))
  # 
  # idxy=c(get.idx(c(0.6, 4.7), f2ppm), get.idx(c(5, 10), f2ppm))
  # 
  # noi = noise2D(jr)
  # js=jr[,idxy]
  # 
  # pl=pickPeaks_rcpp(jr = js, f1hz = as.numeric(rownames(js)), f2ppm = as.numeric(colnames(js)), noise = as.numeric(noi), boundary = 10, sf = sf)
  # plist=do.call(rbind, pl)
  # plist$featID=1:nrow(plist)
  plist=plist[order(plist$Int, decreasing = T),]
  
  #browser()
  #add svm prob
  test=lapply(1:n, function(i, ps=plist, jr=js, ss=sf){
    extr_resize(jr=js, pl=ps[i,], sf=ss)
  })
  
  #browser()
  #plist=do.call(rbind, test)
  
  return(list(plist, test))
  
  # 
  # test2=filter_multiplicity(dfp=plist, noise=noi)
  # mult.match=do.call(rbind, test2[[1]])
  # mult.match$status='Matched'
  # 
  # 
  # if(length(test2[[2]])>0){
  #   mult.prob=do.call(rbind, test2[[2]])
  #   mult.prob$Signal.ID=NA
  #   mult.prob$Signal.nPeaks=NA
  #   mult.prob$Signal.PeakID=NA
  #   mult.prob$status='Unmatched'
  #   
  #   mult=rbind(mult.match, mult.prob)
  # }else{
  #   mult=mult.match
  # }
  # 
  # mult=mult[order(mult$featID),]
  # return(mult)
  
  
}