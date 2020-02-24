bbox_lplgau=function(sub, f1.hz, f2.ppm, i, j, fsf){
  
  f2.hz=f2.ppm*fsf
  box.hz = jRes:::bbox_lplgauss(sub, f1.hzz = f1.hz[i],
                         f2.hzz = f2.hz[j], sf = fsf)
  add1 = box.hz[[1]] * 2
  add1.f1 = add1 * 0.6
  add1.f2 = add1 * 1.3
  
  bb.f1hz = get.idx(range = c(f1.hz[i] - add1.f1,
                              f1.hz[i] + add1.f1), f1.hz)
  bb.f2ppm = get.idx(range = c(f2.ppm[j] - (add1.f2/fsf),
                               f2.ppm[j] + (add1.f2/fsf)), f2.ppm)
  if (length(bb.f1hz) < 2 | length(bb.f2ppm) <
      3) {
    message("skip one")
    out.df = data.frame(cent.f2 = f2.ppm[j], cent.f1 = f1.hz[i],
                        bb.width = box.hz[[1]], Gsigma = box.hz[[1]],
                        Imax = NA, Imean3 = NA, Imean3w = NA, P.peakOri = NA,
                        P.peakSym = NA)
    return(list(c(f1.hz[i] - add1.f1, f1.hz[i] + add1.f1, f2.ppm[j] - (add1.f2/fsf), f2.ppm[j] + (add1.f2/fsf)), box.hz))
    #return(out.df)
  }
  
  
  
  
  # return bounding box coordinates and 
  return(list(c(f1.hz[i] - add1.f1, f1.hz[i] + add1.f1, f2.ppm[j] - (add1.f2/fsf), f2.ppm[j] + (add1.f2/fsf)), box.hz))
  
  # 
  # 
  # 
  # if (length(bb.f1hz) < 3 | length(bb.f2ppm) <
  #     3) {
  #   roi_ext = interp.surface.grid(obj = list(x = as.numeric(rownames(sub)),
  #                                            y = as.numeric(colnames(sub)), z = sub),
  #                                 grid.list = list(x = seq(from = (f1.hz[i] -
  #                                                                    add1.f1), to = (f1.hz[i] + add1.f1), by = 0.1),
  #                                                  y = seq(from = (f2.ppm[j] - (add1.f2/fsf)),
  #                                                          to = (f2.ppm[j] + (add1.f2/fsf)), by = 0.1/fsf)))
  #   roi = roi_ext[[3]]
  #   rownames(roi) = roi_ext[[1]]
  #   colnames(roi) = roi_ext[[2]]
  # }
  # else {
  #   roi = jr[bb.f1hz, bb.f2ppm]
  # }
  # i2 = which.min(abs(as.numeric(colnames(roi)) -
  #                      f2.ppm[j]))[1]
  # i1 = which.min(abs(as.numeric(rownames(roi)) -
  #                      f1.hz[i]))[1]
  # 
  # 
  # 
  
  
  
  
  
  
  
}
       