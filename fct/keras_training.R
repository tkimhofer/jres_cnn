# exctract trainig set 



source('fct/creatImg_peaks.R')

n=min(c(nrow(plist), 2000))
submat_img=ppjres_par(js=js, plist, sf, n)

fnam_img=array()
count=1
for(i in 1:n){
  if(!is.null(submat_img[[2]][[i]])){
    
    fnam=paste0('/Volumes/Torben_1/Rproj/JresSVM/www/Feat_', submat_img[[1]]$featID[i], '.png')
    fnam_img[count]=fnam
    png(fnam,  width=240, height=240)
    image((submat_img[[2]][[i]]),  xaxt='n',  yaxt='n')
    text(0.75, 0.1, as.character(i))
    points(0.5, 0.5, pch='+', col='cyan')
    dev.off()
    count=count+1
  }
  
}

print('generated')
#fnam_img=paste0('www/', list.files('www/'))
fnam_img=list.files('www/')
#paste0('<img scr="', gsub('/Volumes/Torben_1/Rproj/JresSVM/www/', '', fnam_img), '"></img>')
# six rows
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
load(file='www/Feature_mat_Rdata') # featm

load('FeatClassManual_2020_02_26_10_54.Rdata')
load('FeatClassManual_2020_02_27_16_03.Rdata')


img_df # df six cols showing features
realPeaks # position peaks in img.df
feats=apply(realPeaks, 1, function(i, df=img_df){
  
  df[i[1], i[2]]
  
})

fidx=as.numeric(gsub('.png\"></img>', '', gsub('<img src=\"Feat_', '', feats, fixed = T), fixed = T))

image(featm[fidx[200], ,])

out=rep(0, dim(featm)[1])
out[fidx]=1
save(out, featm, file='FeatManual_2020_02_27_16_03_Philip_Training.Rdata')


library(reticulate)
library(tensorflow)
library(jres)
library(keras)

use_condaenv(condaenv = 'jresppick', conda = "auto", required = T)

load('FeatManual_2020_02_27_16_03_Philip_Training.Rdata')


modelc <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(1, 32,32, 1)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu")

modelc %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 2, activation = "softmax")

modelc %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

modelc %>% 
  fit(
    x = featm, y = out,
    epochs = 10,
    validation_split = 0.3,
    verbose = 2
  )




model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(32, 32)) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(2, activation = "softmax")

model %>% 
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

model %>% 
  fit(
    x = featm, y = out,
    epochs = 14,
    validation_split = 0.3,
    verbose = 2
  )

model %>% 
  evaluate(featm, out, verbose = 2)

predictions=predict(model, featm)
save_model_tf(model, file='newsequential_mod_Philip.Rdat')






load(file='FeatClassManual.Rdata') # img_df, realPeaks


out=apply(img_df, 2, function(x){
  gsub('|\"></img>', '', gsub('<img src=\"', '', x,  fixed=T))
})

peaks=sapply(1:nrow(realPeaks), function(i, dd=out, rp=realPeaks){
  
  #browser()
  dd[rp[i,1], rp[i,2]]
  
})

nopeaks= as.vector(out)[(!as.vector(out) %in% as.character(peaks)) & as.vector(out) != '']
le.tot=length(nopeaks) + length(peaks)


train=list(
  x=array(NA, dim=c(32, 32, le.tot)),
  y=c(rep(1, length(peaks)), rep(0, length(nopeaks)))
  )

feat=c(peaks, nopeaks)
count=1
iidc=array()
for(i in 1:le.tot){
  
  idx=which(submat_img[[1]]$featID==as.numeric(gsub('.png', '', gsub('Feat_', '', feat[i]), fixed=T)))
  if(length(idx)==1){
    train$x[, ,count]=submat_img[[2]][[idx]]
    iidc[count]=submat_img[[1]]$featID[idx]
    count=count+1
  }
  

}

save(train, file='svmset.Rdata')

image(train$x[,,1059])



load('/Users/TKimhofer/Downloads/cnn_pred.Rdata')

idx=match(iidc, plist$featID)
plist$P.peakOri=NA
plist$P.peakOri[idx]=predictions[-nrow(predictions),2]

plotjres_overlay1D_bboxNull(log(jr), spec.1d = Xbl, ppm.1d = ppm, SF=sf, bbox = plist, t1.lim = c(-20,20), 
                            t2.lim = c(6.8, 7))
plist1=plist[plist$P.peakOri>0.5,]
plotjres_overlay1D_bboxNull(log(jr), spec.1d = Xbl, ppm.1d = ppm, SF=sf, bbox = plist1, t1.lim = c(-20,20), 
                            t2.lim = c(1,1.2))



plotjres_overlay1D_bboxNull(log(jr), spec.1d = Xbl, ppm.1d = ppm, SF=sf, bbox = plist1, t1.lim = c(-20,20), 
                            t2.lim = c(3,3.1))



