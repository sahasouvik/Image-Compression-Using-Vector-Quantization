require(jpeg)
x=as.matrix(readJPEG("C:\\Users\\Souvik Saha\\Documents\\Study material\\Presidency University PG notes\\PG Sem3\\Applied Multivariate\\Project\\References\\PCM.jpg"))
drawPic = function(y,title)
{ 
dim(y) = c(430,346)
plot(1:2,ty='n',main=title)
rasterImage(as.raster(y),1,1,2,2)
}
drawPic(x,"PCM")
#(b)
#Step-1
V=matrix(0,nrow=430*346/4,ncol=4,byrow=T)
for(i in 1:430){
  if(i%%2!=0){
    for(k in 1:86)
    {
      v=x[i,((4*k)-3):(4*k)]
      V=rbind(V,v)
    }
  }else{
    v1=c(x[(i-1),345:346],x[i,1:2])
    V=rbind(V,v1)
  for(k in 1:86)
  {
       v=x[i,((4*k)-1):((4*k)+2)]
       V=rbind(V,v)
  }
  }
}
#Step-2
V=na.omit(V)
V
z=kmeans(V,15)
table(z$cluster)
S=z$centers
drawPic = function(y,title)
{ 
  dim(y) = c(15,4)
  plot(1:2,ty='n',main=title)
  rasterImage(as.raster(y),1,1,2,2)
}
drawPic(S,"PCM")
