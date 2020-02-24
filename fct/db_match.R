# database matching

db_match=function(ppm.cent, mpl, dev.hz, db=csh_red, sf){
  
  dhz=abs(db$deltaH-ppm.cent)*sf
  
  if(is.na(mpl)){
    idx=which(dhz<=dev.hz)
  }else{
    idx=which(dhz<=dev.hz & mpl==db$mult_abbrev)
  }
  out=db[idx,]
  
  
  return(out[order(dhz[idx]),])
  
}