library(MetaboMate)
library(plotly)
library(jres)
library(DT)
library(colorRamps)
library(shinybusy)
library(plyr)
library(keras)



source('fct/db_match.R')
source('fct/readB_sf.R')
source('fct/readB1D_sf.R')
source('fct/filter_multiplicity_new.R')

# # 
# # prepare data and load R object
# # read in 2D
# 
 path='/Volumes/Torben_1/Epi_data/Airwave/AIRWAVE_Urine_Rack09_RCM_190914/381/'
# # path='/Volumes/TORBEN/std/495-69-2_300K_IVDR03_PN_140220_HippuricAcid/2/'
# # 
# dat=readB_sf(path, n=1)
# readB1d('/Volumes/Torben_1/Epi_data/Airwave/AIRWAVE_Urine_Rack09_RCM_190914/380')
# spec(ppm, X)
# meta$lw=lw(X[c(1,1),], ppm)[1]
# # 
# Xbl=bline(X)
# Xbl[is.na(Xbl)]=0
# 
# jr=dat[[1]][[1]]
# 
# noi=noise2D(jr)
# 
# fiID=path
# 
# # pick peaks
# js=jr
# f1hz=as.numeric(rownames(js))
# f2ppm=as.numeric(colnames(js))
# 
# 
# # exlude water and TSP
# idx.c.rm=c(get.idx(c(min(f2ppm), 0.6), f2ppm), get.idx(c(4.7,5), f2ppm), get.idx(c(9.7, max(f2ppm)), f2ppm))
# #idx.r.rm=c(get.idx(c(25, max(f1hz)), f1hz), get.idx(c(min(f1hz), -25), f1hz))
# 
# js[, idx.c.rm]=0
# 
# idx.1d=c(get.idx(c(min(ppm), 0.6), ppm), get.idx(c(4.7,5), ppm), get.idx(c(9.7, max(ppm)), ppm))
# Xbl[idx.1d]=0
# 
# print('okay - start ppick')
# #a=Sys.time()
# pl=pickPeaks_rcpp(js, as.numeric(rownames(js)), as.numeric(colnames(js)), noi, sf=meta$a_SFO1[1], boundary=10)
# #save(pl, file='RuntTest.Rdata')
# #load(file='RuntTest.Rdata')
# plist_all=list(do.call('rbind', pl))
#plist=do.call('rbind', lapply(pl, '[[', 1))
#save(plist, file='plistTest.Rdata')
#load(file='plistTest.Rdata')

#plist=do.call(rbind, pl)
#b=Sys.time()

# print('okay - pfilter')
# 
# # perform peak matching
# mm=lapply(plist_all, function(plist){
#   # #browser()
#   #plist=plist[which(plist$bb.width.f1>0.33 & plist$bb.width.f2>0.41 & abs(plist$cent.f1) < 20 ),]
# 
#   plist$featID=1:nrow(plist)
# 
#   noi=plist$noise[1]
# 
#   test2=filter_multiplicity(dfp=plist, noise=noi)
#   mult.match=do.call(rbind, test2[[1]])
#   mult.match$status='Matched'
# 
# 
#   if(length(test2[[2]])>0){
#     mult.prob=do.call(rbind, test2[[2]])
#     mult.prob$Signal.ID=NA
#     mult.prob$Signal.nPeaks=NA
#     mult.prob$Signal.PeakID=NA
#     mult.prob$status='Unmatched'
# 
#     mult=rbind(mult.match, mult.prob)
#   }else{
#     mult=mult.match
#   }
#   #
#   mult=mult[order(mult$featID),]
# })

# #mm=plist_all
# # get ratios of all splitting patterns
# # test=lapply(mm, function(sam){
# #   ddply(sam, .(Signal.ID), function(feat){
# #     max(feat$Int)/min(feat$Int)
# #   })
# # })
# 
# ms=lapply(mm, function(sam){
#   ddply(sam, .(Signal.ID), function(feat){
#     #browser()
#     outs=feat$Int/max(feat$Int)
#     feat$dmax=outs
#     feat
#   })
# })
# 
# plot(ms[[1]]$dmax)
# te=lapply(ms, function(plist){
#   
#   pl=plist[plist$dmax>0.1,]
#   noi=pl$noise[1]
#   test2=filter_multiplicity(dfp=pl, noise=noi)
#   mult.match=do.call(rbind, test2[[1]])
#   mult.match$status='Matched'
#   
#   
#   if(length(test2[[2]])>0){
#     mult.prob=do.call(rbind, test2[[2]])
#     mult.prob$Signal.ID=NA
#     mult.prob$Signal.nPeaks=NA
#     mult.prob$Signal.PeakID=NA
#     mult.prob$status='Unmatched'
#     
#     mult=rbind(mult.match, mult.prob)
#   }else{
#     mult=mult.match
#   }
#   
#   mult=mult[order(mult$Signal.ID),]
#   
# })
# ### STOP
# 
#save.image(file='Imagetest.Rdata')
load('Imagetest.Rdata')
plist=plist_all[[1]]

plist=plist[order(plist$Int, decreasing = T),]
mult=plist

print('ppick done')
# 
# # plist$featID=1:nrow(plist)
# # 
# sf=meta$a_SFO1[1]
# f1hz=as.numeric(rownames(jr))
# f2ppm=as.numeric(colnames(jr))
# f2hz=f2ppm*sf
# # 
# # plist2=plist
# # plist2$cent.f1=round(plist2$cent.f1,2)
# # test2=filter_multiplicity(dfp=plist2, noise=noi)
# # sapply(test2, length)
# # 
# # 
# # mult.match=do.call(rbind, test2[[1]])
# # mult.match$status='Matched'
# # 
# # 
# # if(length(test2[[2]])>0){
# #   mult.prob=do.call(rbind, test2[[2]])
# #   mult.prob$Signal.ID=NA
# #   mult.prob$Signal.nPeaks=NA
# #   mult.prob$Signal.PeakID=NA
# #   mult.prob$status='Unmatched'
# #   
# #   mult=rbind(mult.match, mult.prob)
# # }else{
# #   mult=mult.match
# # }
# # 
# # mult=mult[order(mult$Int, decreasing = T),]
# 
# 
# 
# # load(file='UK_PBC_U1.Rdata')
# 
# # keep track which features have already been processed
# # feat.count=0
# # save(feat.count, file='feat.count.Rdata')
 load('feat.count.Rdata')
 feat.count=1
 sc=cbind(rev(minmax(log10(1/1:30))), matlab.like2(30))
# # #
# #plist=plist[plist$Int> (25* noi),]
# 
# print('okay - ui output')
# 
# #plist=mult
load(file='db_compounds.Rdata')


#save.image('img_1.Rdata')
#load('img_1.Rdata')

print('peak prob')


source('fct/creatImg_peaks.R')
sf=sf=meta$a_SFO1[1]
n=nrow(plist)
submat_img=ppjres_par(js=js, plist, sf=meta$a_SFO1[1], n)

idx=which(!sapply(submat_img[[2]], is.null))

fpred=array(NA, dim=c(length(idx), 32, 32))
for(i in 1:length(idx)){
  
  fpred[i,,]=submat_img[[2]][[idx[i]]]
  
}


jres_cnn= load_model_tf('jres_model')

fp=predict(jres_cnn , fpred)

plist$P.peakOri=NA
plist$P.peakOri[idx]=fp[,2]
plot(plist$P.peakOri, log(plist$Int))


  plist$featID=1:nrow(plist)

  plist1=plist[which(plist$P.peakOri>0.3),]

  test2=filter_multiplicity(dfp=plist1, noise=as.numeric(noi))
  mult.match=do.call(rbind, test2[[1]])
  mult.match$status='Matched'

  plist=mult=mult.match


# fnam_img=array()
# count=1
# for(i in 1:n){
#   if(!is.null(submat_img[[2]][[i]])){
# 
#     fnam=paste0('/Volumes/Torben_1/Rproj/JresSVM/www/Feat_', submat_img[[1]]$featID[i], '.png')
#     fnam_img[count]=fnam
#     png(fnam,  width=240, height=240)
#     image((submat_img[[2]][[i]]),  xaxt='n',  yaxt='n')
#     text(0.75, 0.1, as.character(i))
#     points(0.5, 0.5, pch='+', col='cyan')
#     dev.off()
#     count=count+1
#   }
# 
# }
# 
# print('generated')
# #fnam_img=paste0('www/', list.files('www/'))
# fnam_img=list.files('www/')
# #paste0('<img scr="', gsub('/Volumes/Torben_1/Rproj/JresSVM/www/', '', fnam_img), '"></img>')
# # six rows
# cidx=rep(1:6, length.out=length(fnam_img))
# 
# add_max=max(table(cidx))
# clist=list()
# for(i in 1:6){
#   clist[[i]]=paste0('<img src="', fnam_img[which(cidx==i)], '"></img>')
# if(length(clist[[i]])<add_max){
#   clist[[i]]=c(clist[[i]], rep('', add_max-length(clist[[i]])))
# }
# }
# 
# #test=do.call(cbind, clist)
# 
# img_df=data.frame(do.call(cbind, clist), stringsAsFactors = F)
# 
# 
print('start')
