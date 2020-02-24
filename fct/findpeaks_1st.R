pickpeaks_1st=function (jr, noise, sf, boundary=10) {
  f2.ppm = as.numeric(colnames(jr))
  f2.hz = as.numeric(colnames(jr)) * sf
  f1.hz = as.numeric(rownames(jr))
  add.row = round(boundary/median(diff(f1.hz)))
  add.col = round(boundary/median(diff(f2.hz)))
  
  ttf = lapply(2:(nrow(jr) - 1), function(i, fsf = sf) {
    
    xf = c(NA, jr[i, -1] - jr[i, -ncol(jr)])
    xb = c(jr[i, -ncol(jr)] - jr[i, -1], NA)
    
    idx = which(xf > 0 & xb > 0)
    idx = idx[idx < (ncol(jr) - 1) & idx > 2]
    
    test = lapply(idx, function(j) {
      #if(i==76 & j==578){browser()}
      mm = jr[i, j]
      if (jr[i - 1, j] <= mm & jr[i + 1, j] <= mm & mm > noise) {
        #cat(i, "-", j, "\n")
        f1l.idx = max(c(i - add.row, 0))
        f1h.idx = min(c(i + add.row, nrow(jr)))
        
        f2l.idx = max(c(j - add.col, 0))
        f2h.idx =min(c(j + add.col, ncol(jr)))
        
        sub = jr[f1l.idx:f1h.idx, f2l.idx:f2h.idx]
        
        idout=which(sub==jr[i,j], arr.ind=T)
        print(idout)
        return(list(idx=c(i, j), submat=sub, idx.submat=idout))
      }else{ return(NULL)}
      
    })

    idx.p=!as.logical(sapply(test, is.null))
    if(length(which(idx.p))==0) return(NULL)
    
    return(test[idx.p])
    
  })
  
  return(ttf)
}