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

#devtools::install_github('tkimhofer/jres', auth_token = 'cb0b26573bd528f6265c1831baa7d32e107a1e70')

source('fct/db_match.R')
source('fct/readB_sf.R')
source('fct/readB1D_sf.R')
# 
# use_condaenv(condaenv = 'jres', conda = "auto", required = T)
# use_condaenv(condaenv = 'jresppick', conda = "auto", required = T)
#
#
#
#
#
# # decide if cnn training or just browsing using existing data
#
# start with cnn training
#
# # prepare data and load R object
# # read in 2D
# path='/Volumes/Torben_1/Autism/U 600 B1/Autism_Urine_Rack1_NH_030816/11/'
# fiID=path
# 
# dat=readB_sf(path, n=1)
# js=jr=dat[[1]][[1]]
# noi=noise2D(jr)
# 
# # # read in 1D
# readB1d(gsub('1.$', '0', path))
# spec(ppm, X)
# meta$lw=lw(X[c(1,1),], ppm)[1]
# sf=meta$a_SFO1[1]
# 
# Xbl=bline(X)
# Xbl[is.na(Xbl)]=0
# 
# 
# idx.1d=c(get.idx(c(min(ppm), 0.6), ppm), get.idx(c(4.7,5), ppm), get.idx(c(9.7, max(ppm)), ppm))
# Xbl[,idx.1d]=0
# 
# # pick peaks
# f1hz=as.numeric(rownames(jr))
# f2ppm=as.numeric(colnames(jr))
# 
# # exlude water and TSP
# idx.c.rm=c(get.idx(c(min(f2ppm), 0.6), f2ppm), get.idx(c(4.7,5), f2ppm), get.idx(c(9.7, max(f2ppm)), f2ppm))
# js[, idx.c.rm]=0
# 
# 
# print('okay - start ppick')
# #a=Sys.time()
# #
# library(jres)
# pl=pickPeaks_rcpp(js, as.numeric(rownames(js)), as.numeric(colnames(js)), noi, sf=meta$a_SFO1[1], boundary=10)
# 
# 
# # idx.c=getBoundary(pl[[i]][[1]]$f2.idx, pl[[i]][[1]]$add_col_feat, pl[[i]][[1]]$ncol, '-'):getBoundary(pl[[i]][[1]]$f2.idx, pl[[i]][[1]]$add_col_feat, pl[[i]][[1]]$ncol, '+');
# #
# # idx.r=getBoundary(pl[[i]][[1]]$f1.idx, pl[[i]][[1]]$add_row_feat, pl[[i]][[1]]$ncol, '-'):getBoundary(pl[[i]][[1]]$f1.idx, pl[[i]][[1]]$add_row_feat, pl[[i]][[1]]$ncol, '+');
# #
# # image(jr[idx.r, idx.c])
# 
# 
# pplist=lapply(pl, '[[', 1)
# pplist=data.frame(do.call(rbind, pplist), stringsAsFactors = F)
# 
# idx=which(pplist$bb.width.f1<0.22)
# pplist=pplist[-idx,]
# pl=pl[-idx]
# 
# pplist$feature.ID=1:nrow(pplist)
# #
# pplist$noise=noi
# pplist$sf=dat[[1]][[2]]
# pplist$spec=path
# 
# 
# # either train model (and display sub) or use existing model to show peaks
# #add svm prob
# 
# feat_norm=lapply(1:length(pl), function(i, ps=pl, ppl=plist){
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
# 
# plist=pplist
# rm(pplist)
# # 
# # jres_cnn_old= load_model_tf('jres_model')
# # fpo=predict(jres_cnn_old , featm)
# 
 jres_cnn= load_model_tf('newsequential_mod_Philip.Rdat')
 fp=predict(jres_cnn , featm)
# 
# 
# 
# #
# plist$P.peakOri=NA
# plist$P.peakOri=fp[,2]
# plot(plist$P.peakOri, log(plist$Int))
# 
# 
# print('saving')
# save.image('Imagetest.Rdata')
load('Imagetest.Rdata')




print('ppick done')







# # keep track which features have already been processed
# # feat.count=0
# # save(feat.count, file='feat.count.Rdata')
#feat.count=1
sc=cbind(rev(minmax(log10(1/1:30))), matlab.like2(30))
# # #
# #plist=plist[plist$Int> (25* noi),]
# 
# print('okay - ui output')
# 
# #plist=mult
load(file='dat/db_compounds.Rdata')
print('peak prob')


test2=filter_multiplicity(dfp=plist, noise=as.numeric(noi))
mult.match=do.call(rbind, test2[[1]])
mult.match$status='Matched'
mult.um=cbind(do.call(rbind, test2[[2]]), Signal.ID=NA, Signal.nPeaks=NA, Signal.PeakID=NA, status='Unmatched')

plist=mult=rbind(mult.match, mult.um)
plist=plist[order(plist$feature.ID),]

# print('generated')
# generate table for manual peak classification
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
