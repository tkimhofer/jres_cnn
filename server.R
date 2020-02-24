library(plotly)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # fid is feature ID of feature shown
  fid=reactiveVal()

  # fidx is index of feature shown
  fidx=reactiveVal()
  fidx(1)
  
  # data frame that stores features matched as multiplet
  feat_multdf=reactiveValues()
  
  # store features characts
  featurelist=reactiveValues()
  
  observeEvent(fidx(), {
    browser()
    # update feat ID
    fid(plist$feature.ID[fidx()])

    # generate dataframe that contains all signals with the same f2 location
    idx.sim=which(round(plist$cent.f2, 4)==round(plist$ppm.f2R[fidx()]))
    if(length(idx.sim>0)){
      odf=mult[idx.sim,]
      mac=c('s'=1, 'd'=2, 't'=3, 'dd'=4, 'm'=5)
      odf$mtype=names(mac)[match(odf$Signal.nPeaks, mac)]
      odf$mtype[which(odf$Signal.nPeaks>5)]='m'
      odf$RelInt=round(odf$Int/mult$Int[idx.f],2)
      odf$mult=paste0(odf$mtype,' (', odf$Signal.PeakID, ')')
      odf$mult[odf$status=='Unmatched']=NA
      
      mach=c("ID"="feature.ID", "F2 position"="ppm.f2R", "F1 position"="cent.f1", 'Rel. Intensity'='RelInt', 'Mult ID'='Signal.ID',  'Mult Pattern'='mult', 'Mult Comment'='ok', 'Mult Status'='status')
      idx.col=match(mach, colnames(odf))
      odf=odf[,idx.col]
      colnames(odf)=names(mach)
      
      feat_multdf$mults=odf
    }
  }
  )
  
  
  # 
  # featurelist=reactiveValues()
  # feat_df=reactiveValues()
  # 
  # Hz to ppm
  observeEvent(input$'matchdev',{
    output$ppm=renderText(paste(round(input$'matchdev'/sf,4), 'ppm'))
  }
  )
  
  
  ### set up folder where results will be stored
  
  # get and display path and file ID
  output$IDspec <- renderText({
    fiID[1]
  })
  
  # feature and file summary
  output$summarytbl=DT::renderDT({
    datatable(data.frame(desc=c(
      
      'Total Number of Features',
      'Noise Threshold',
      'Median Feature Intensity'), 
      value=c(
        nrow(plist),
        round(noi,0), 
        round(median(mult$Int))
      )), rownames=F, selection='none', options=list(dom='t'))
  })
  
  
  # produce feature table from peak picked daata
  output$featTbl=DT::renderDT({
    #browser()
    odf=plist[,-1]
    odf$cent.f1=round(odf$cent.f1, 1)
    odf$cent.f2=round(odf$cent.f2, 4)
    odf$Int=round(odf$Int, 0)
    odf$RelInt=round(odf$Int/max(odf$Int)*100, 1)
    
    mac=c('s'=1, 'd'=2, 't'=3, 'dd'=4, 'm'=5)
    odf$mtype=names(mac)[match(odf$Signal.nPeaks, mac)]
    odf$mtype[which(odf$Signal.nPeaks>5)]='m'
    
    odf$mult=paste0(odf$mtype,' (', odf$Signal.PeakID, ')')
    odf$mult[odf$status=='Unmatched']=NA
    
    mach=c("ID"="feature.ID", "F2 position"="cent.f2", "F1 position"="cent.f1", 'Rel. Intensity'='RelInt', 'Mult ID'='Signal.ID',  'Mult Pattern'='mult', 'Mult Comment'='ok', "F2 box size"="bb.width.f2",  "F1 box size"="bb.width.f1",  "F2 center idx"="f2.idx", "F1 center idx"="f1.idx", "Intensity"="Int")
    
    idx=match(mach, colnames(odf))
    odf=odf[,idx]
    colnames(odf)=names(mach)
    
    
    datatable(odf, rownames=F, options=list(pageLength=50), selection='single', filter = list(position = 'top', clear = FALSE))
  })
  

  observeEvent({
    input$nextFeat
  }, {
    if((fidx()+1)>nrow(plist)){return(NULL)}
    fidx(fidx()+1)
  }, ignoreInit = T, ignoreNULL = T)
  
  
  observeEvent({
    input$prevFeat
  }, {
    if((fidx()-1)<1){return(NULL)}
    fidx(fidx()-1)
  }, ignoreInit = T, ignoreNULL = T)
  
  observeEvent({
    input$'manID'
  }, {
    idx=which(plist$feature.ID==as.numeric(input$'manID'))
    if(length(idx)==1){
      fidx(idx)
    }
  }, ignoreInit = T, ignoreNULL = T)
  
  observeEvent({
    input$featTbl_cell_clicked
  }, {
    tblcl=input$featTbl_cell_clicked
    if(length(tblcl)>1){
      fidx(tblcl$row)
      updateTabsetPanel(session, inputId='ppopt', selected = "Feature Description")}
  }, ignoreInit = T, ignoreNULL = T)
  
  
  observeEvent({
    input$info_mult_cell_clicked
  }, {
    tblcl=input$info_mult_cell_clicked
    
    if(length(tblcl)>1){
      
      browser()
      odf=feat_multdf$mults
      fidx(which(plist$feature.ID==odf$ID[tblcl]))
      
      updateTabsetPanel(session, inputId='ppopt', selected = "Feature Description")
    }
    
    
  }, ignoreInit = T, ignoreNULL = T)
  
  
  output$FeatureID <- renderText({
    paste0('Select feature by clicking on a row of the feature table')
  })
  
  output$featbb_Int=renderPlot(
    
    ggplot()+
      scale_x_continuous(trans='log10')+
      geom_density(data=mult, aes(Int))+
      geom_density(data=subset(mult, bb.width.f2>0.5 & bb.width.f1>0.5), aes(Int), colour='blue', linetype=6)+
      theme_bw()
  )
  
  
  
  # change summary stats and plot depending on selected input ID
  observeEvent({fidx()}, {
    
    
    #browser()
    idx.f= fidx()
    
    
    output$FeatureID <- renderText({
      paste0('Feature ID ', fid())
    })
    # update plot showing feat intensity distribution
    output$featbb_Int=renderPlot(
      
      ggplot()+
        scale_x_continuous(trans='log10')+
        geom_density(data=mult, aes(Int))+
        geom_vline(xintercept = mult$Int[idx.f], colour='green')+
        #geom_density(data=subset(mult, bb.width.f2>0.5), aes(Int), colour='green', linetype=2)+
        geom_density(data=subset(mult, bb.width.f2>0.5 & bb.width.f1>0.5), aes(Int), colour='blue', linetype=6)+
        scale_colour_manual(values=c("green", "blue", "black"))+
        theme_bw()+labs(x='Feature Intensity')
    )
    
    
    print(plist[idx.f,])
    output$Fsummarytbl=DT::renderDT({
      
      datatable(
        data.frame(desc=c('F2 Position', 'F1 Position', 'Exceeding Noise Thresh',  'Relative Int to max feat Int', 'F2 box size', 'F1 box size', 'Abs Intensity'), 
                   value=c( paste0(round(plist$cent.f2[idx.f], 4), ' ppm'), 
                            paste0(round(plist$cent.f1[idx.f], 1), ' Hz'), 
                            paste0(round((plist$Int[idx.f]/noi)*100, 0), ' %'),
                            paste0(round((plist$Int[idx.f]/max(plist$Int))*100, 1), ' %'),
                            paste0(round(plist$bb.width.f2[idx.f], 1), ' Hz'), 
                            paste0(round(plist$bb.width.f1[idx.f], 1), ' Hz'),
                            paste0(round(plist$Int[idx.f], 0)))),
        rownames=F, selection='none', options=list(dom='t'))
    })
    
    #browser()
    # Overview plot
    # define submatrix that contains signal within bb dimensions and more (for overview plot)
    bbf2ppm=(plist$bb.width.f2[idx.f]/sf)
    idx.c=get.idx(c(plist$cent.f2[idx.f]-bbf2ppm-0.05, plist$cent.f2[idx.f]+bbf2ppm+0.05), as.numeric(colnames(jr)))
    idx.r=get.idx(c(plist$cent.f1[idx.f]-plist$bb.width.f1[idx.f]-5, plist$cent.f1[idx.f]+plist$bb.width.f1[idx.f]+5), as.numeric(rownames(jr)))
    
    sub2=jr[idx.r, idx.c]
    f2sub2=as.numeric(colnames(sub2))
    f1sub2=as.numeric(rownames(sub2))
    
    plot_ly() %>% add_surface(z=sub2, x=f2sub2, y=f1sub2) %>%
      add_markers(y=plist$cent.f1[idx.f], x=plist$cent.f2[idx.f], z=plist$Int[idx.f])
    #image(sub2)
    
    
    test=lapOfG(sub=sub2, f1hz=f1sub2, f2hz=f2sub2*sf, cent_f1hz=plist$cent.f1[idx.f], cent_f2hz=plist$cent.f2[idx.f]*sf, sf=sf, npix=seq(0.1, 3, by=0.05))
    plot(seq(0.1, 3, by=0.05)[1:length(test[[2]])], test[[2]])
    
    # 
    # browser()
    # ssub=jres::matsymminf2(sub2, which(colnames(sub2)==plist$cent.f2[idx.f]))
    #              
    # test1=lapOfG(sub=ssub, f1hz=f1sub2, f2hz=f2sub2*sf, cent_f1hz=plist$cent.f1[idx.f], cent_f2hz=plist$cent.f2[idx.f]*sf, sf=sf, npix=seq(0.1, 3, by=0.05))
    # points(seq(0.1, 3, by=0.05)[1:length(test1[[2]])], minmax(test1[[2]])+min(test[[2]]), col='red')
    # 
    # plot(seq(0.1, 3, by=0.05)[1:length(test1[[2]])], test1[[2]], col='red')
    # 
    # plot_ly(z=~ssub, x=as.numeric(colnames(ssub)), y=as.numeric(rownames(ssub))) %>% add_surface()
    # plot_ly(z=~sub2) %>% add_surface()
    
    
    # idx.c=get.idx(c(plist$cent.f2[idx.f]-bbf2ppm-0.01, plist$cent.f2[idx.f]+bbf2ppm+0.01), as.numeric(colnames(jr)))
    # idx.r=get.idx(c(plist$cent.f1[idx.f]-plist$bb.width.f1[idx.f]-5, plist$cent.f1[idx.f]+plist$bb.width.f1[idx.f]+5), as.numeric(rownames(jr)))
    # 
    # sub2=jr[idx.r, idx.c]
    # f2sub2=as.numeric(colnames(sub2))
    # f1sub2=as.numeric(rownames(sub2))
    # 
    # plot_ly() %>% add_surface(z=sub2, x=f2sub2, y=f1sub2) %>%
    #   add_markers(y=plist$cent.f1[idx.f], x=plist$cent.f2[idx.f], z=plist$Int[idx.f])
    # #image(sub2)
    # 
    # test=lapOfG(sub=sub2, f1hz=f1sub2, f2hz=f2sub2*sf, cent_f1hz=plist$cent.f1[idx.f], cent_f2hz=plist$cent.f2[idx.f]*sf, sf=sf, npix=seq(0.01, 3, by=0.005))
    # 
    # points(seq(0.01, 3, by=0.005), test[[2]], col='red')
    # 
    # 
    # 
    # 
    # 
    # idx.c=get.idx(c(plist$cent.f2[idx.f]-bbf2ppm-0.001, plist$cent.f2[idx.f]+bbf2ppm+0.001), as.numeric(colnames(jr)))
    # idx.r=get.idx(c(plist$cent.f1[idx.f]-plist$bb.width.f1[idx.f]-1, plist$cent.f1[idx.f]+plist$bb.width.f1[idx.f]+1), as.numeric(rownames(jr)))
    # 
    # sub2=jr[idx.r, idx.c]
    # f2sub2=as.numeric(colnames(sub2))
    # f1sub2=as.numeric(rownames(sub2))
    # 
    # plot_ly() %>% add_surface(z=sub2, x=f2sub2, y=f1sub2) %>%
    #   add_markers(y=plist$cent.f1[idx.f], x=plist$cent.f2[idx.f], z=plist$Int[idx.f])
    # #image(sub2)
    # 
    # test=lapOfG(sub=sub2, f1hz=f1sub2, f2hz=f2sub2*sf, cent_f1hz=plist$cent.f1[idx.f], cent_f2hz=plist$cent.f2[idx.f]*sf, sf=sf, npix=seq(0.1, 3, by=0.05))
    # 
    # points(seq(0.1, 3, by=0.05), test[[2]], col='green')
    # 
    # 
    # 
    # 
    # idx.c=get.idx(c(plist$cent.f2[idx.f]-bbf2ppm, plist$cent.f2[idx.f]+bbf2ppm), as.numeric(colnames(jr)))
    # idx.r=get.idx(c(plist$cent.f1[idx.f]-plist$bb.width.f1[idx.f]-1, plist$cent.f1[idx.f]+plist$bb.width.f1[idx.f]+1), as.numeric(rownames(jr)))
    # 
    # sub2=jr[idx.r, (idx.c-10) : (idx.c+10)]
    # f2sub2=as.numeric(colnames(sub2))
    # f1sub2=as.numeric(rownames(sub2))
    # 
    # plot_ly() %>% add_surface(z=sub2, x=f2sub2, y=f1sub2) %>%
    #   add_markers(y=plist$cent.f1[idx.f], x=plist$cent.f2[idx.f], z=plist$Int[idx.f])
    # #image(sub2)
    # 
    # test=lapOfG(sub=sub2, f1hz=f1sub2, f2hz=f2sub2*sf, cent_f1hz=plist$cent.f1[idx.f], cent_f2hz=plist$cent.f2[idx.f]*sf, sf=sf, npix=seq(0.1, 3, by=0.05))
    # 
    # points(seq(0.1, 3, by=0.05), test[[2]], col='green')
    
    
    
    
    
    
    
    plist1=plist
    # plist1$P.peakOri=0.4
    # plist1$P.peakOri[idx.f]=1
    idx=which(plist$cent.f1>=min(f1sub2) & plist$cent.f1<=max(f1sub2))
    plist1=plist1[idx,]
    
    
    output$featbb_overview=renderPlot(
      
      plotjres_overlay1D_bboxNull(sub2, t2.lim=range(f2sub2), t1.lim=range(f1sub2), spec.1d=Xbl, ppm.1d=ppm, z.probs=0,  SF=sf , bbox=plist1,  title='', pPeak= 0, tilt=F, addProjpp=NULL)
    )
    
    output$featbb_overviewLOG=renderPlot(
      plotjres_overlay1D_bboxNull(log10(sub2+abs(min(sub2))+1), t2.lim=range(f2sub2), t1.lim=range(f1sub2), spec.1d=Xbl, ppm.1d=ppm, z.probs=0,  SF=sf , bbox=plist1,  title='', pPeak= 0.6, tilt=F, addProjpp=NULL)
    )
    
    featurelist$featureMatrisForLoG = sub2
    featurelist$sf = sf
    featurelist$noise = noi
    
    
    npix=seq(0.01, 2, by=0.05)
    log_sd=jres::lapOfG(sub2, f1sub2, f2sub2*sf, plist$cent.f1[idx.f], plist$cent.f2[idx.f]*sf, sf, npix)
    #plot(npix, log_sd[[2]])
    
    output$log_sums=renderPlot(
      { 
        #browser()
        dfs=data.frame(npix=npix[1:length(log_sd[[2]])], LoG=log_sd[[2]])
        #browser()
        ggplot(dfs, aes(npix, LoG))+
          geom_point(colour='gray')+
          geom_line()+
          geom_vline(xintercept=npix[which.min(log_sd[[2]])], colour='red')+
          theme_bw()+
          labs(x='sigma (Hz)', y='Delta sum', title='Laplacian of Gaussian matching')
        
      })
    
    
    # create multiplicity overview
    output$info_mult=renderDT(
      { 
        #browser()
        odf=feat_multdf$mults
        if(!is.null(odf)){
          datatable(odf, options=list(dom='t', pageLength=6), selection='none',rownames=F) %>%
            formatStyle(
              'ID',
              target = 'row',
              backgroundColor = styleEqual(c(0, idx.f), c('green', '#A9D3DA'))
            ) 
        }
        
        }
      )
    
    
    
    
    
    
    
    # # create plotly figures with peaks in bounding box
    bbf2ppm=(plist$bb.width.f2[idx.f]/sf)
    idx.c=get.idx(c(plist$cent.f2[idx.f]-bbf2ppm, plist$cent.f2[idx.f]+bbf2ppm), f2ppm)
    idx.r=get.idx(c(plist$cent.f1[idx.f]-plist$bb.width.f1[idx.f], plist$cent.f1[idx.f]+plist$bb.width.f1[idx.f]), f1hz)
    if(length(idx.c)<2 | length(idx.r)<2){
      idx.r=c(max((idx.r[1]-2), 1):min(length(f1hz),(idx.r[length(idx.r)]+2)));
      idx.c=c(max((idx.c[1]-2), 1):min(length(f2ppm),(idx.c[length(idx.c)]+2)));
      output$info=renderText(paste('BBox extended as feature too small'))
    }else{
      output$info=renderText(paste(''))
    }
    sub=jr[idx.r, idx.c]
    
    #image(featm[idx.f,,])
    
    
    # create plot
    noiss=sub
    noiss[,]=noi/plist$Int[idx.f]
    
  
    subf1=as.numeric(rownames(sub))
    subf2=as.numeric(colnames(sub))
    
    #if(idx.f==5){browser()}
    featurelist$featureMatrix = sub
    featurelist$featureInfo = plist[idx.f,]
    
    
    output$featbb=renderPlotly(
      plot_ly(x=subf2, y=subf1) %>% 
        add_surface(z=~noiss, colors='black', opacity = 0.7, showscale=F) %>%
        add_surface(z=~sub, colorscale = sc, showscale=T) %>%
        add_markers(x=plist$cent.f2[idx.f], y=plist$cent.f1[idx.f], z=plist$Int[idx.f], sizes=40, colors='pink')
    )
    
    if(min(sub)<0){
      sub=sub+abs(min(sub))
    }
    
    output$featbb_minmax=renderPlotly(
      plot_ly(x=subf2, y=subf1) %>% 
        add_surface(z=~minmax(sub), showscale=T) %>%
        add_markers(x=plist$cent.f2[idx.f], y=plist$cent.f1[idx.f], z=1.2, sizes=40, colors='pink')
    )
    
    
    # db match output
    #observeEvent({input$'matchdev'},{
    
  }, ignoreInit = T, ignoreNULL = T)
  
  
  # db match output
  observeEvent({
    fidx()
    input$'matchdev'
  },{
    
    idx.f= fidx()
    
    if(!is.null(idx.f)){
      print(paste('re-calc db matches', as.numeric(input$'matchdev'), 'Hz'))
      mac=c('s'=1, 'd'=2, 't'=3, 'dd'=4, 'm'=5)
      #browser()
      plist$mtype=NA
      plist$mtype=names(mac)[match(plist$Signal.nPeaks, mac)]
      plist$mtype[which(plist$Signal.nPeaks>5)]='m'
      
      #browser()
      
      matches=db_match(ppm.cent = plist$cent.f2[idx.f], mpl = plist$mtype[idx.f], dev.hz = as.numeric(input$'matchdev'), db = csh_red, sf = sf)
      #print(matches)
      #if(nrow(matches)>1){
      output$dbmatch=DT::renderDT({datatable(
        data.frame(matches[,c(1:3)]),
        rownames=F, selection='none', options=list(dom='t'))
      })
      # }
      
    }
    
    
    
  }, ignoreInit = T, ignoreNULL = T)
  
  
  
  # display db in new tab
  output$db_full=DT::renderDT({datatable(
    data.frame(csh_red),
    rownames=F, selection='single', editable = TRUE, options=list(pageLength=50), filter = list(position = 'top', clear = FALSE))
  })
  
  proxy = dataTableProxy('db_full')
  observeEvent(input$db_full_cell_edit, {
    info = input$db_full_cell_edit
    str(info)
    i = info$row
    j = info$col+1
    v = info$value
    #browser()
    csh_red[i, j] <<- DT::coerceValue(v, csh_red[i, j])
    replaceData(proxy, csh_red, resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  # 'bboxfit'
  # 'signal_def'
  # 'mult_fine'
  
  observeEvent({
    input$bboxfit
    input$signal_def
    input$mult_fine
  },
  {
    output$findings=renderDT(
      datatable(
        data.frame(Descr=c('Bounding box fit', 'Peak resolution', 'Multiplicity match'),
                   Parameter=c(input$bboxfit, input$signal_def, input$mult_fine))
        , rownames=F, selection='none', options=list(dom='t')))
  }
  )
  
  # save feature list
  observeEvent(input$submit,
               {
                 featurelist$index=fidx()
                 featurelist$user_input_bboxfit=input$bboxfit
                 featurelist$user_input_signal_def=input$signal_def
                 featurelist$user_input_mult_fine=input$mult_fine
                 featurelist$user=list(sysinfo=Sys.info(), time=Sys.time())
                 
                 
                 #browser()
                 featurelist$NMRfile=fiID[1]
                 if(!dir.exists('feat_data')){dir.create('feat_data', showWarnings = TRUE)}
                 fname=paste0('feat_data/JresFeat_', fid(), '.Rdata')
                 out=reactiveValuesToList(featurelist)
                 save(out, file=fname)
                 
                 updateTabsetPanel(session, inputId='ppopt', selected = "Feature Table")
                 
                 
                 
                 # featurelist$featureMatrix # -> define and assign obove
                 # featurelist$featureInfo # -> define and assign obove
               }, ignoreNULL = T, ignoreInit = T)
  
  
  
  output$featimg <- DT::renderDataTable({
    
    DT::datatable(img_df, escape = F, selection=list(target='cell'), options = list(pageLength = nrow(img_df))) # HERE
  })
  
  observeEvent(input$featimg_cells_selected, {
    
    print(input$featimg_cells_selected)
  })
  
  observeEvent(input$'save_img_train', {
    realPeaks=input$featimg_cells_selected
    save(img_df, realPeaks, file='FeatClassManual.Rdata')
  })
  
  
}


