tobin<-function(db){
v1<-table(db[,1])
v2<-table(db[,2])
v3<-table(db[,3])
bin<-matrix(NA,dim(db)[1],sum(length(v1),length(v2),length(v3)))
for(i in 1:length(v1)){
bin[,i]<-db[,1]==names(v1)[i]
}
j<-1
for(i in (length(v1)+1):(length(v1)+length(v2))){
  bin[,i]<-db[,2]==names(v2)[j]
  j<-j+1
}
j<-1
for(i in (length(v1)+length(v2)+1):(length(v1)+length(v2)+length(v3))){
  bin[,i]<-db[,3]==names(v3)[j]
  j<-j+1
}
return(bin)
}