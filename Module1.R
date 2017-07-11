GetShapefile <- function(InShapefile, OutShapefile){
    if (is.null(InShapefile)) 
        return(NULL)  
    dir<-dirname(InShapefile[1,4])
      print(paste("Directory name:",dir))
    for ( i in 1:nrow(InShapefile)) {
    file.rename(InShapefile[i,4], paste0(dir,"/",InShapefile[i,1]))}
    OutShapefile <- grep(list.files(dir, pattern="*.shp", full.names=TRUE), pattern="*.xml", inv=T, value=T)
     }
      
