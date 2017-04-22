casvd<-function(tb,p=0,d=2,lf,lc,colf="blue",colc="red"){
#d numero de dimensiones a retener
#p perfiles a mostrar 0: ambos, 1:filas 2:columnas
N<-as.matrix(tb)
n<-sum(tb)  
P<-N/n
r<-margin.table(P,1)
c<-margin.table(P,2)
Dr<-diag(r**(-0.5),length(r),length(r))
Dc<-diag(c**(-0.5),length(c),length(c))
S<-Dr%*%(P-r%*%t(c))%*%Dc
ds<-svd(S)
U<-ds$u
Da<-ds$d
V<-ds$v
X<-Dr%*%U
Y<-Dc%*%V
pine<-round(prop.table(Da**2)*100,2)
if(d==2){
plot(X[,1:2],type="n",xlab=paste("Dimensión 1 (",pine[1],"%)"),ylab=paste("Dimensión 2 (",pine[2],"%)"),xlim=c(min(X[,1:2]),max(X[,1:2])),ylim=c(min(Y[,1:2]),max(Y[,1:2])))
if(p==1 | p==0){  
text(X[,1:2],lf,col=colf,cex=0.7)
} 
if(p==2 | p==0){
text(Y[,1:2],lc,col=colc,cex=0.7)
}
inercia<-sum((P-r%*%t(c))**2/r%*%t(c))
abline(h=0,v=0,lty=2,col="gray")
title(main=paste("Inercia",round(inercia,4)))
} else if(d==1){
  plot(X[,1],rep(0,dim(X)[1]),type="n",xlab=paste("Dimensión 1 (",pine[1],"%)"),ylab="")
  text(X[,1],rep(0,dim(X)[1]),lf,col="blue",cex=0.7)
  text(Y[,1],rep(0,dim(Y)[1]),lc,col="red",cex=0.7)
  abline(h=0,lty=2)
  inercia<-sum((P-r%*%t(c))**2/r%*%t(c))
  title(main=paste("Inercia Total",round(inercia,4)))
}
}
##################################################
##################################################
mcasvd<-function(tb,p=0,d=2,lf,lc,colf="blue",colc="red"){
  #d numero de dimensiones a retener
  #p perfiles a mostrar 0: ambos, 1:filas 2:columnas
  N<-as.matrix(tb)
  n<-sum(tb)  
  P<-N/n
  r<-margin.table(P,1)
  c<-margin.table(P,2)
  Dr<-diag(r**(-0.5),length(r),length(r))
  Dc<-diag(c**(-0.5),length(c),length(c))
  S<-Dr%*%(P-r%*%t(c))%*%Dc
  ds<-svd(S)
  U<-ds$u
  Da<-ds$d
  V<-ds$v
  X<-Dr%*%U
  Y<-Dc%*%V
  pine<-round(prop.table(Da**2)*100,2)
  if(d==2){
    plot(X[,1:2],type="n",xlab=paste("Dimensión 1"),ylab=paste("Dimensión 2"))
    if(p==1 | p==0){  
      text(X[,1:2],lf,col=colf,cex=0.7)
    } 
    if(p==2 | p==0){
      text(Y[,1:2],lc,col=colc,cex=0.7)
    }
    inercia<-sum((P-r%*%t(c))**2/r%*%t(c))
    abline(h=0,v=0,lty=2,col="gray")
    #title(main=paste("Inercia",round(inercia,4)))
  } else if(d==1){
    plot(X[,1],rep(0,dim(X)[1]),type="n",xlab=paste("Dimensión 1"),ylab="")
    text(X[,1],rep(0,dim(X)[1]),lf,col="blue",cex=0.7)
    text(Y[,1],rep(0,dim(Y)[1]),lc,col="red",cex=0.7)
    abline(h=0,lty=2)
    inercia<-sum((P-r%*%t(c))**2/r%*%t(c))
    #title(main=paste("Inercia Total",round(inercia,4)))
  }
}
