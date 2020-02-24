jresPeak3d=function(pp, nois, sym=F, smooth, brush, bbox.coord=NA){
  require(colorRamps)
  require(plotly)
  
  sub=pp[[2]]
  f1.hz=as.numeric(rownames(sub))
  f2.ppm=as.numeric(colnames(sub))
  p.idx=pp[[3]]
  
  if(length(bbox.coord)==4){
    sub[which(f1.hz<bbox.coord[1] | f1.hz>bbox.coord[2]),]=0
    sub[,which(f2.ppm<bbox.coord[3] | f2.ppm>bbox.coord[4])]=0
    
    #browser()
    idx.r=get.idx(c(bbox.coord[1], bbox.coord[2]), f1.hz)
    idx.c=get.idx(c(bbox.coord[3], bbox.coord[4]), f2.ppm)
    if(length(idx.r)<3){
      noiss=sub
    }else{noiss=sub[idx.r, idx.c]}
    
    #print(bbox.coord)
  }else{
    if(sym==T){
      sub=symmROI(sub, i1=p.idx[1], i2 = p.idx[2], smoo=smooth, br=brush)
    }
    noiss=sub
  }
 
  
  p.int=sub[pp[[3]][1], pp[[3]][2]]
  cat('Peak intensity: ', round(p.int,1), '\n')
  p.f1=apply(sub, 1, max, na.rm=T)
  p.f2=apply(sub, 2, max, na.rm=T)
  
  
  #noiss=sub[bbox.coord[1]:bbox.coord[2], bbox.coord[3]: bbox.coord[4]]
  #print(dim(noiss))
  noiss[,]=nois
  
  
  #   plot_ly(showscale = T, x=as.numeric(colnames(jr)), y=as.numeric(rownames(jr))) %>%
  #     add_surface(z = ~jr, colorscale = sc) %>%
  #     add_surface(z = ~noiss, color='black', opacity = 0.7)%>%
  #     layout(
  #       title = "Homonuclear J-resolved <sup>1</sup>H NMR",
  #       scene = list(
  #         xaxis = list(title = 'F2 (ppm)'),
  #         yaxis = list(title = 'F1 (Hz)'),
  #         zaxis = list(title = "Intensity")
  #       ))
  # define colour scale for plotting 3d surfaces
  sc=cbind(rev(minmax(log10(1/(1:30)))), matlab.like2((30)))
  
  plot_ly(x=f2.ppm, y=f1.hz) %>%
    add_surface(z=~sub, name='Jres', colorscale = sc) %>%
    add_surface(z~noiss, y=as.numeric(rownames(noiss)), x=as.numeric(colnames(noiss)), color='rgb(0,0,0)', opacity = 0.8, name='Noise thresh', showscale=FALSE ) %>%
    add_markers(x=f2.ppm[ p.idx[2] ], y=f1.hz[ p.idx[1] ], z=p.int*1.1,
                opacity = 0.2, name='Peak position',
                marker = list(
                  color = 'rgb(10,101,10)',
                  size = 40,
                  
                  line = list(
                    color = 'rgb(231, 99, 250)',
                    width = 2
                  )
                )
    ) %>%
    layout(
      title = paste0("Homonuclear J-resolved <sup>1</sup>H NMR:", f1.hz[ p.idx[1] ], ', ', f2.ppm[ p.idx[2] ]),
      scene = list(
        xaxis = list(title = 'F2 (ppm)'),
        yaxis = list(title = 'F1 (Hz)'),
        zaxis = list(title = "Intensity")
      )) %>%
    add_surface(z=~sub, showscale=FALSE,colorscale = sc,
                contours = list(
                  z = list(
                    show=TRUE,
                    usecolormap=TRUE,
                    highlightcolor="#ff0000",
                    project=list(z=TRUE)
                  )
                )
    )%>%
    #add_surface(~noiss) %>%
    add_paths(z=~p.f1, x=max(f2.ppm, na.rm=T), y=f1.hz, color = 'rgb(255, 255, 255)', name='F1 projection') %>%
    add_paths(z=~p.f2, y=min(f1.hz, na.rm=T), x=f2.ppm, color = 'rgb(0, 0, 0)', name='F2 projection')
  
}



symmROI=function (gim, i1, i2, smoo=F, br=3){
  
  if(smoo==T){
    f = makeBrush(br, shape = "disc", step = FALSE)
    fgis = filter2(gim, f)
  }else{fgis=gim}
  
  tts = fgis
  tts[, ] = NA
  runs1 = min(c(nrow(gim) - i1, i1))
  runs2 = min(c(ncol(gim) - i2, i2))
  if ((runs1*2 + 1) > nrow(gim) | (runs2*2 + 1) > ncol(gim)) 
    return(fgis)
  for (i in -runs2:runs2) {
    for (j in -runs1:runs1) {
      if1 = i1 + j
      if2 = i2 + i
      if1r = i1 - j
      if2r = i2 - i
      if (fgis[if1, if2] >= 0) {
        tts[if1, if2] = tts[if1r, if2r] = max(c(0, min(c(fgis[if1, 
                                                              if2], fgis[if1r, if2r]), na.rm = T)))
      }
      else {
        tts[if1, if2] = tts[if1r, if2r] = min(c(0, max(c(fgis[if1, 
                                                              if2], fgis[if1r, if2r]), na.rm = T)))
      }
    }
  }
  return(tts)
}