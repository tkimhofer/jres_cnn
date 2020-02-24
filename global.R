library(MetaboMate)
library(plotly)
library(jres)
library(DT)
library(colorRamps)
library(shinybusy)
library(plyr)
library(keras)
library(png)
library(EBImage)

use_condaenv(condaenv = 'jres', conda = "auto", required = T)

#devtools::install_github('tkimhofer/jres', auth_token = 'cb0b26573bd528f6265c1831baa7d32e107a1e70')

source('fct/db_match.R')
source('fct/readB_sf.R')
source('fct/readB1D_sf.R')
#source('fct/filter_multiplicity_new.R')

# decide if cnn training or just browsing using existing data

# start with cnn training
# 
# # # prepare data and load R object
# # # read in 2D
# path='/Volumes/TK_1/ASD U/NMR/Autism_Urine_Rack05_NH_160817/101'
# fiID=path
# 
# dat=readB_sf(path, n=1)
# js=jr=dat[[1]][[1]]
# sf=meta$a_SFO1[1]
# noi=noise2D(jr)
# 
# # # read in 1D
# readB1d(gsub('1$', '0', path))
# spec(ppm, X)
# meta$lw=lw(X[c(1,1),], ppm)[1]
# 
# Xbl=bline(X)
# Xbl[is.na(Xbl)]=0
# 
# # pick peaks
# f1hz=as.numeric(rownames(jr))
# f2ppm=as.numeric(colnames(jr))
# 
# # exlude water and TSP
# idx.c.rm=c(get.idx(c(min(f2ppm), 0.6), f2ppm), get.idx(c(4.7,5), f2ppm), get.idx(c(9.7, max(f2ppm)), f2ppm))
# js[, idx.c.rm]=0
# 
# idx.1d=c(get.idx(c(min(ppm), 0.6), ppm), get.idx(c(4.7,5), ppm), get.idx(c(9.7, max(ppm)), ppm))
# Xbl[,idx.1d]=0
# 
# print('okay - start ppick')
# #a=Sys.time()
# 
# pl=pickPeaks_rcpp(js, as.numeric(rownames(js)), as.numeric(colnames(js)), noi, sf=meta$a_SFO1[1], boundary=10)
# 
# 
# pplist=lapply(pl, '[[', 1)
# pplist=data.frame(do.call(rbind, pplist), stringsAsFactors = F)
# pplist$feature.ID=1:nrow(pplist)
# 
# pplist$noise=noi
# pplist$sf=dat[[1]][[2]]
# pplist$spec=path
# 
# 
# # either train model (and display sub) or use existing model to show peaks
# #add svm prob
# 
# feat_norm=lapply(1:length(pl), function(i, ps=pl, ppl=pplist){
#   
#   sub=minmax(resize(ps[[i]]$sub, w=32, h=32))
#   png(filename=paste0('www/Feat_', ppl$feature.ID[i], '.png'), width=240, height=240)
#   par(mar=c(0,0,0,0))
#       image(sub,  xaxt='n',  yaxt='n')
#       text(0.75, 0.1, as.character(ppl$feature.ID[i]))
#       points(0.5, 0.5, pch='+', col='cyan')
#       dev.off()
#   return(sub)
# })
# featm=abind(feat_norm, rev.along=3)
# save(featm, file='www/Feature_mat_Rdata')

# save.image('Imagetest.Rdata')
 load('Imagetest.Rdata')

plist=mult=pplist


print('ppick done')







# # keep track which features have already been processed
# # feat.count=0
# # save(feat.count, file='feat.count.Rdata')
feat.count=1
sc=cbind(rev(minmax(log10(1/1:30))), matlab.like2(30))
# # #
# #plist=plist[plist$Int> (25* noi),]
# 
# print('okay - ui output')
# 
# #plist=mult
load(file='dat/db_compounds.Rdata')
print('peak prob')
sf=meta$a_SFO1[1]

# source('fct/creatImg_peaks.R')
# sf=sf=meta$a_SFO1[1]
# n=nrow(plist)
# submat_img=ppjres_par(js=js, plist, sf, n)
# 
# idx=which(!sapply(submat_img[[2]], is.null))
# 
# fpred=array(NA, dim=c(length(idx), 32, 32))
# for(i in 1:length(idx)){
#   
#   fpred[i,,]=submat_img[[2]][[idx[i]]]
#   
# }
# 
# 
# jres_cnn= load_model_tf('jres_model')
# 
# fp=predict(jres_cnn , fpred)
# 
# plist$P.peakOri=NA
# plist$P.peakOri[idx]=fp[,2]
# plot(plist$P.peakOri, log(plist$Int))
# 
# 
#   plist$featID=1:nrow(plist)
# 
#   plist1=plist[which(plist$P.peakOri>0.3),]
# 
#   test2=filter_multiplicity(dfp=plist1, noise=as.numeric(noi))
#   mult.match=do.call(rbind, test2[[1]])
#   mult.match$status='Matched'
# 
#   plist=mult=mult.match

test2=filter_multiplicity(dfp=plist, noise=as.numeric(noi))
mult.match=do.call(rbind, test2[[1]])
mult.match$status='matched'
mult.um=cbind(do.call(rbind, test2[[2]]), Signal.ID=NA, Signal.nPeaks=NA, Signal.PeakID=NA, status='unmatched')

plist=mult=rbind(mult.match, mult.um)


#   mult.match$status='Matched'



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
#fnam_img=paste0('www/', list.files('www/'))
fnam_img=list.files('www/')
# paste0('<img scr="', gsub('/Volumes/Torben_1/Rproj/JresSVM/www/', '', fnam_img), '"></img>')

# six cols
cidx=rep(1:6, length.out=length(fnam_img))
 
add_max=max(table(cidx))
clist=list()
for(i in 1:6){
  clist[[i]]=paste0('<img src="', fnam_img[which(cidx==i)], '"></img>')
if(length(clist[[i]])<add_max){
  clist[[i]]=c(clist[[i]], rep('', add_max-length(clist[[i]])))
}
}

#test=do.call(cbind, clist)
# 
img_df=data.frame(do.call(cbind, clist), stringsAsFactors = F)
# 
# 
print('start')
