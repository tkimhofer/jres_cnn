

load(file='Cardia_28_35Peaks.Rdata') # tall,  Xe, ppe, 
readBruker('/Volumes/Torben_1/Rproj/KMeyer/dat/U NMR raw/CARDIA_Urine_Rack04_NH_220818/')

# generate ppm position for each item

# test=tall[1:2,]
# ts=fit.2Ddist1(cent.ppm=mean(test$cent.f2), Jconst.ppm=test$cent.f1/600, split=c(1,1), ppm=ppe, scale=mean(test$Imean3), f=0.5)


library(plyr)
tall=do.call(rbind, ts)

#test=tall[which(tall$id=='JRES_CARDIA_Urine_Rack04_NH_220818/131'),]
test=tall[which(tall$id=='121'),]

library(plyr)
test=do.call(rbind, pf[[1]])
#test=test[order(test$Signal.ID, decreasing = T),]
test=ddply(test, .(Signal.ID), function(x) {x$mm=mean(x$Imean3, na.rm = T); x})
test=test[order(test$mm, decreasing = T),]
test=test[which(!is.nan(test$mm)),]
test$Signal.ID=factor(test$Signal.ID, levels=unique(test$Signal.ID))

map=as.character(unique(test$Signal.ID))
test$IDn=match(as.character((test$Signal.ID)), as.character(unique(test$Signal.ID)))


library(plyr)
sf=meta$a_SFO1[1]

Xe=X
ppe=ppm
idx=which(is.na(test$Imean3))
test$Imean3[idx]=noise/10
tte=dlply(test, .(IDn), function(x){
  #browser()
  df=data.frame(fit.2Ddist1(cent.ppm=mean(x$cent.f2), Jconst.ppm=x$cent.f1/sf, split=x$Imean3/max(x$Imean3), ppm=ppe, scale=max(x$Imean3), f=0.5))
  df$Signal.ID=x$Signal.ID[1]
  
  return(df)
})


test$Irel=test$mm/max(test$mm, na.rm=T)

idds=1
idx=get.idx(c(2.9,3.1), ppe)
spec(ppe[idx], minmax(Xe[idds,idx]), shift=c(3,3.5), col='gray')

iid=unique(test$IDn[which(!is.nan(test$Irel))])
for(i in iid){
  points(tte[[i]]$ppm, minmax(tte[[i]]$fit)/1.3*test$Irel[which(test$IDn==i)[1]], type='l', add=T, lty=2, col=i)
}

idx=which(test$bb.width<=0.2)
idx=which(test$bb.width>0.2)
for(i in 1:nrow(test)){
  abline(v=test$cent.f2[idx[i]], col='red')
}


for(i in 1:nrow(pf$ori)){
  abline(v=pf$ori$cent.f2[i], col='red')
}



spec(ppe[idx], minmax(Xs), col='gray')
points(tte[[1]]$ppm, minmax(tte[[1]]$fit)/1.3, type='l', add=T, lty=2)
points(tte[[2]]$ppm, minmax(tte[[2]]$fit)/1.3*test$Irel[2], type='l', add=T, lty=2, col='cyan')


points(tte[[3]]$ppm, minmax(tte[[3]]$fit)/1.3*test$Irel[which(test$IDn==3)[1]], type='l', add=T, lty=2, col='green')
points(tte[[4]]$ppm, minmax(tte[[4]]$fit)/1.3*test$Irel[which(test$IDn==4)[1]], type='l', add=T, lty=2, col='blue')

points(tte[[5]]$ppm, minmax(tte[[5]]$fit)/1.3*test$Irel[which(test$IDn==5)[1]], type='l', add=T, lty=2, col='darkgreen')

i=14
points(tte[[i]]$ppm, minmax(tte[[i]]$fit)/1.3*test$Irel[which(test$IDn==i)[1]], type='l', add=T, lty=2, col='darkgreen')




points(tte[[5]]$ppm, minmax(tte[[5]]$fit)/1.3*test$Irel[which(test$IDn==5)[1]], type='l', add=T, lty=2, col='darkgreen')
points(tte[[5]]$ppm, minmax(tte[[5]]$fit)/1.3*test$Irel[which(test$IDn==5)[1]], type='l', add=T, lty=2, col='darkgreen')




ggplot(tte, aes(ppm, fit, Signal.ID))+
  geom_line()

plot(tte$ppm, tte$fit, type='l')

test=ddply(tte, .(ppm), function(x){
  sum(x$fit)
})

plot(test$ppm, (test$V1)/max(test$V1, na.rm = T), type='l')
idx=get.idx(c(6,10), ppe)
Xs=bline(Xe[1:2,idx], lambda = 1e+08)[1,]
spec(ppe[idx], minmax(Xs), add=T, col='red', lty=2)

spec(ppe, Xe[1,], shift=c(6,9.5))

fit.2Ddist1=function(cent.ppm, Jconst.ppm, split, ppm, scale, f){
  
  # calculate center positions of all signals 
  d1=which.min(abs(ppm-cent.ppm))
  d.le=array()
  for(i in 1:length(Jconst.ppm)){
    d.le[i]=length(get.idx(c(cent.ppm, cent.ppm+Jconst.ppm[i]), ppm))
  }
  
  # generate distributions
  # LG columns: get index from min to max peak (*3 to includer margins)
  # LG rows: one row for one split
  # extend this in case it is singlett
  if(length(Jconst.ppm)>1){
    idx=get.idx(c(cent.ppm-(sum(abs(Jconst.ppm)*3)), cent.ppm+(sum(abs(Jconst.ppm))*3)), ppm)
  }else{
    idx=get.idx(c(cent.ppm-0.1, cent.ppm+0.1), ppm)
  }
  
  LG=matrix(NA, nrow=length(split), ncol=length(idx))
  for(i in 1:length(split)){
    if(length(split)==1){
      if(Jconst.ppm[i]<0){
        sc=0.001
        L=dcauchy(ppm[idx], location=cent.ppm, scale=0.001)
        G=dnorm(ppm[idx], mean=cent.ppm, sd=sc)
      }else{
        L=dcauchy(ppm[idx], location=cent.ppm, scale=0.001)
        G=dnorm(ppm[idx], mean=cent.ppm, sd=0.001)
      }
    }else{
      if(Jconst.ppm[i]<0){
        sc=0.001
        L=dcauchy(ppm[idx], location=cent.ppm+(Jconst.ppm[i]), scale=0.001)
        G=dnorm(ppm[idx], mean=cent.ppm+(Jconst.ppm[i]), sd=sc)
      }else{
        L=dcauchy(ppm[idx], location=cent.ppm+(Jconst.ppm[i]), scale=0.001)
        G=dnorm(ppm[idx], mean=cent.ppm+(Jconst.ppm[i]), sd=0.001)
      }}
    LG[i,]=((f*L) + ((1-f) *G))*split[i]
    #spec(ppm[idx], LG[i,])
  }
  comb=minmax(apply(LG, 2, sum))*scale
  #spec(ppm[idx], comb)
  #abline(v=cent.ppm, col='red')
  return(cbind(ppm=ppm[idx], fit=comb))
}