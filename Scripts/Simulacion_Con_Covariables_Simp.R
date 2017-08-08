devtools::install_github('jucdiaz/ZOIP', force=TRUE)
library(ZOIP)

n<-c(75,seq(150,3500,by=100))
nrep=1000
family='Simplex'
link=c('logit','exp','identity','logit')

mu.b0<-NULL
mu.b1<-NULL

sigma.b0<-NULL
sigma.b1<-NULL
sigma.b2<-NULL

p0.b0<-c<-NULL

p1.b0<-c<-NULL
p1.b1<-c<-NULL

j=1
while(j<=length(n)){
  i=1
  while(i<=nrep){
    try({
    x1<-runif(n[j]);x2<-runif(n[j])
    
    c1<-0.2;c2<--1
    
    mu_i<-inv.logit(c1+c2*x1)
    
    b1<-0.3;b2<-3;b3<-0.9
    
    sigma_i<-exp(b1+b2*x1+b3*x2)
    
    d1<-0.1
    p0_i<-rep(d1,n[j])
    
    e1<-0.02;e2<--4
    p1_i<-inv.logit(e1+e2*x2)
    
    param<-cbind(mu_i,sigma_i,p0_i,p1_i)
    y_i<-apply(param,1,function(x){rZOIP(1,mu=x[1],sigma=x[2]
                                         ,p0=x[3],p1=x[4]
                                         ,family=family)})
    
    data<-as.data.frame(cbind(y_i,x1,x2))
    
    mod<-RM.ZOIP(formula.mu=y_i~x1,formula.sigma=~x1+x2
                 ,formula.p0=~1,formula.p1=~x2
                 ,data=data,link=link,family=family)
    coefi<-coef(mod)
    mu.b0[i]<-coefi$Parameters.mu[1]
    mu.b1[i]<-coefi$Parameters.mu[2]
    
    sigma.b0[i]<-coefi$Parameters.sigma[1]
    sigma.b1[i]<-coefi$Parameters.sigma[2]
    sigma.b2[i]<-coefi$Parameters.sigma[3]
    
    p0.b0[i]<-coefi$Parameters.p0[1]
    
    p1.b0[i]<-coefi$Parameters.p1[1]
    p1.b1[i]<-coefi$Parameters.p1[2]
    i=i+1
    },silent=TRUE)
  }
  data_i<-as.data.frame(cbind(tamaño_muestra=n[j],mu.b0,mu.b1,sigma.b0,sigma.b1,sigma.b2,p0.b0,p1.b0,p1.b1
                              ,mu.b0_verd=c1,mu.b1_verd=c2,sigma.b0_verd=b1,sigma.b1_verd=b2,sigma.b2_verd=b3
                              ,p0.b0_verd=d1,p1.b0_verd=e1,p1.b1_verd=e2))
  
  write.table(data_i,file='D:\\datos_sim_cv_Simplex.csv',append=TRUE,sep = ",",col.names = FALSE, row.names = FALSE)
  j=j+1
}
