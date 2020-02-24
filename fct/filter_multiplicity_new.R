#' Peak filter based on observed splitting pattern
#' @param dfp Dataframe describing detected Jres features (see description)
#' @param noise Noise threshold
#' @param dev.f1.hz max diff in f1 (Hz), used for matching singlets to zero 
#' @param dev.Jcoup.hz max diff Jcopuling symmetry
#' @description dfp must have the following two columns: cent.f2 (center position f2 dimensions), cent.f1 (center position f1 dimension)
#' @author [Torben Kimhofer](https://tkimhofer.com)

filter_multiplicity=function(dfp, noise, dev.f1.hz=0.2, dev.Jcoup.hz=1.5){

  # use multiplicity as filter, all have same f2 value
  dfp$ppm.f2R=round(dfp$cent.f2, 4)
  un=unique(dfp$ppm.f2R)

  tl=list() # peaks that have acceptable multiplicity pattern
  er=list() # peaks that have some problems in multiplicity
  dfp$ok=NA
  c=1
  cr=1


  # add in peak missing
  for(i in 1:length(un)){
    
    #print(i)
    # which peaks have the same f2 value
    idx=which(dfp$ppm.f2R==un[i])

    # if there is only one value, the it must be a singlet
    if(length(idx)==1){
      # make sure f1 value is close to zero (=singlet)
      if(abs(dfp$cent.f1[idx])<dev.f1.hz){
        tl[[c]]=dfp[idx,]
        tl[[c]]$Signal.ID=c
        tl[[c]]$Signal.nPeaks=length(idx)
        tl[[c]]$Signal.PeakID=1
        #tl[[c]]$type='S'
        c=c+1
      }else{
        er[[cr]]=dfp[idx,]
        cr=cr+1
        dfp$ok[idx]='Centre position missing'
      }
    }else{
      mid=which(abs(dfp$cent.f1[idx])<dev.f1.hz)

      # if uneven number of peaks and no center position
      if(length(mid)==0 & length(idx)/2!=round(length(idx)/2)){
        dfp$ok[idx]='no center';
        er[[cr]]=dfp[idx,]
        cr=cr+1
        next}

      # if multiple centres
      if(length(mid)>1){
        dfp$ok[idx]='multiple centers';
        er[[cr]]=dfp[idx,]
        cr=cr+1
        next}

      # number of peaks is not symmetric when line mirrored from f1=0
      if((length(idx)-length(mid))/2 != round((length(idx)-length(mid))/2)){
        er[[cr]]=dfp[idx,]
        cr=cr+1
        dfp$ok[idx]='uneven peaks'
        next
      }

      # J coupling
      sub=dfp[idx,]
      if(length(mid)==0){
        Jc=diff(sort(abs(sub$cent.f1)))
      }else{
        Jc=diff(sort(abs(sub$cent.f1[-mid])))
      }

      # abs J coupling larger than 1.5, so that
      if(length(which(Jc[seq(1, length(Jc), by=2)]>dev.Jcoup.hz))>0){
        er[[cr]]=dfp[idx,]
        cr=cr+1
        dfp$ok[idx]='Jcoup wrong'
      }


      # if this is okay, that means not error has been reported before
      if(is.na(dfp$ok[idx])[1]){
        tl[[c]]=dfp[idx,]
        #tl[[c]]=tl[[c]][order(abs(tl[[c]][,5])),]
        tl[[c]]$Signal.ID=c
        tl[[c]]$Signal.nPeaks=length(idx)
        tl[[c]]$Signal.PeakID=NA
        if(length(mid)==1){
          tl[[c]]$Signal.PeakID[mid]=1
          sub=dfp$cent.f1[idx]
          sub[mid]=0
          iid=sub<0
          iid1=sub>0
          tl[[c]]$Signal.PeakID[iid]=rank(0+abs(sub[iid]))+1
          tl[[c]]$Signal.PeakID[iid1]=(rank(0+abs(sub[iid1]))+1)*(-1)
        }else{
          iid=dfp$cent.f1[idx]<0
          tl[[c]]$Signal.PeakID[iid]=rank(0+abs(dfp$cent.f1[idx])[iid])
          tl[[c]]$Signal.PeakID[!iid]=rank(0+abs(dfp$cent.f1[idx])[!iid])*(-1)
        }

        c=c+1
      }

      # # I'm not sure this is still needed: (sum is not a column any more)
      # if(!is.na(dfp$ok[idx])[1] & length(mid)==1){
      #   if(dfp$sum[mid]>noise*10){
      #     tl[[c]]=dfp[idx[mid],]
      #     tl[[c]]$Signal.ID=c
      #     tl[[c]]$Signal.nPeaks=length(idx)
      #     c=c+1
      #   }
      #
      # }



    }
  }

  return(list(matched=tl, unmatched=er, ori=dfp))


}
#
# filter_multiplicity=function(dfp, noise){
#
#   # use multiplicity as filter
#   dfp$ppm.f2R=round(dfp$ppm.f2, 4)
#   un=unique(dfp$ppm.f2R)
#   tl=list()
#   er=list()
#   dfp$ok=NA
#   c=1
#   cr=1
#
#   for(i in 1:length(un)){
#     idx=which(dfp$ppm.f2R==un[i])
#
#     if(length(idx)==1){
#       if(abs(dfp$ppm.f1[idx])<0.5){
#         tl[[c]]=dfp[idx,]
#         c=c+1
#       }else(dfp$ok[idx]='S no center')
#     }else{
#       mid=which(abs(dfp$ppm.f1[idx])<0.5)
#
#       # if uneven number of peaks and no center position
#       if(length(mid)==0 & length(idx)/2!=round(length(idx)/2)){
#         dfp$ok[idx]='no center';
#         er[[cr]]=dfp[idx,]
#         cr=cr+1
#         next}
#
#       # if multiple centres
#       if(length(mid)>1){
#         dfp$ok[idx]='multiple centers';
#         er[[cr]]=dfp[idx,]
#         cr=cr+1
#         next}
#
#       #if uneven number of peaks without centre
#       if((length(idx)-length(mid))/2 != round((length(idx)-length(mid))/2)){
#         er[[cr]]=dfp[idx,]
#         cr=cr+1
#         dfp$ok[idx]='uneven peaks'
#         next
#       }
#
#       # J coupling
#       sub=dfp[idx,]
#       if(length(mid)==0){
#         Jc=diff(sort(abs(sub$ppm.f1)))
#       }else{
#         Jc=diff(sort(abs(sub$ppm.f1[-mid])))
#       }
#
#       if(length(which(Jc[seq(1, length(Jc), by=2)]>1.5))>0){
#         dfp$ok[idx]='Jcoup wrong'
#       }
#
#       if(is.na(dfp$ok[idx])[1]){
#         tl[[c]]=dfp[idx,]
#         tl[[c]]=tl[[c]][order(abs(tl[[c]][,5])),]
#         c=c+1
#       }
#
#       if(!is.na(dfp$ok[idx])[1] & length(mid)==1){
#         if(dfp$sum[mid]>noise*10){
#           tl[[c]]=dfp[idx[mid],]
#           c=c+1
#         }
#
#       }
#
#
#
#     }
#   }
#
#   return(list(matched=tl, unmatched=er, ori=dfp))
#
#
# }
