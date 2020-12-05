library(spc)
library(MSQC)
library(readxl)
library(ggplot2)
Project_dataset<- read_excel("C:/Users/suchavan/Desktop/614 Project/Dataset/Project_dataset.xlsx")
str(Project_dataset)
summary(Project_dataset)
pr.out<-prcomp(Project_dataset,scale. = F)
pr.out
names(pr.out)
pr.out$sdev^2 #eigen values
pr.out$center 
pr.out$scale
pr.out$x[,1:4]
plot(pr.out) #scree plot

#not a necessity~lines 14~17
pr.out$rotation
pr.out$rotation <- -pr.out$rotation
pr.out$rotation
plot(pr.out, type = "l") 

(VE <- pr.out$sdev^2)
pve <- VE*100 / sum(VE)
pve <- round(pve, 2) #proportion contribution of each eigen corresponding to PCs
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,100),type='b')

PC1<-as.data.frame(pr.out$x[,1])
PC2<-as.data.frame(pr.out$x[,2])
PC3<-as.data.frame(pr.out$x[,3])
PC4<-as.data.frame(pr.out$x[,4])
PC5<-as.data.frame(pr.out$x[,5])

PC_old<-cbind(PC1,PC2,PC3,PC4,PC5)

#Plotting mewma chart

mw<-mult.chart(type = "mewma", PC_old,lambda=0.5, phase=4)
mewma.crit(l=0.5,L0=200,p=4)


PC_mewma <- PC_old

for (i in seq(1)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  mw<-mult.chart(type = "mewma", PC_mewma, lambda=0.5, phase=1)
  critical_val = mewma.crit(l=0.5,L0=200,p=4)

  outliers = mw$t2 > critical_val
  #mw<-as.data.frame(mw$t2,)
  #mw$Index<- seq(1,nrow(mw))

  
  #p <- ggplot(mw,aes(y = V1,x=Index))
  #p + geom_point() +geom_hline(yintercept = critical_val)
  
  print(sum(outliers))
  print(paste('Outliers:',sum(outliers)))
  print(paste('UCL:',critical_val))
  
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_1<-PC_mewma[inctrl_index,]
  
  PC_mewma <- PC_1
  nrow(PC_mewma)
}
if (1<2){
mw<-as.data.frame(mw$t2,)
mw$Index<- seq(1,nrow(mw))
names(mw)<-c("T_2","Index")
critical_val = mewma.crit(l=0.5,L0=200,p=4)

p <- ggplot(mw,aes(y = T_2,x=Index,color = ifelse( T_2 < critical_val, "In-Control Data Points", "Out of Control")))
p + geom_point() +geom_hline(yintercept = critical_val)+
  geom_text(aes(0,critical_val,label = paste('UCL:',critical_val), vjust = -1,hjust=-2))+
  theme(legend.position='bottom')+ggtitle("MEWMA")
      
}







### MCUSUM
PC_mcusum <- PC_old

for (i in seq(1)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  
  
  mcusum3 <-mult.chart(type = "mcusum", PC_mcusum,k=1.5, h=6, phase=1)
  
  outliers = mcusum3$t2 > mcusum3$ucl
  print(paste('Outliers:',sum(outliers)))
  print(paste('UCL:',mcusum3$ucl))
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_mcusum<-PC_mcusum[inctrl_index,]
  nrow(PC_mcusum)
}
if (1<2){
  mc<-as.data.frame(mcusum3$t2,)
  mc$Index<- seq(1,nrow(mc))
  names(mc)<-c("T_2","Index")
  
  p <- ggplot(mc,aes(y = T_2,x=Index,color = ifelse( T_2 < mcusum3$ucl, "In-Control Data Points", "Out of Control")))
  p + geom_point() +geom_hline(yintercept = mcusum3$ucl)+
    geom_text(aes(0,mcusum3$ucl,label = paste('UCL:',mcusum3$ucl), vjust = -1,hjust=-2))+
    theme(legend.position='bottom')+ggtitle("MCUSUM")
  
}
mcusum3$ucl
### Hotelling Tsquare

PC_T_Square<-PC_old

for (i in seq(1)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  
  tsquare<-mult.chart(PC_T_Square,type="t2",phase=1,alpha=0.005, keep.source = FALSE)
  #mw<-mult.chart(type = "mewma", PC_T_MEWMA_comb, lambda=0.5, phase=1)
  #critical_val = mewma.crit(l=0.5,L0=200,p=4)
  outliers = (tsquare$t2 > tsquare$ucl)
  print(sum(outliers))
  print(paste('Outliers:',sum(outliers)))
  print(paste('UCL:',tsquare$ucl))
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_T_Square<-PC_T_Square[inctrl_index,]
  
  
  nrow(PC_T_Square)
}

if (1<2){
  ts<-as.data.frame(as.vector(tsquare$t2,))
  ts$Index<- seq(1,nrow(ts))
  names(ts)<-c("T_2","Index")
  
  p <- ggplot(ts,aes(y = T_2,x=Index,color = ifelse( T_2 < tsquare$ucl, "In-Control Data Points", "Out of Control")))
  p + geom_point() +geom_hline(yintercept = tsquare$ucl)+
    geom_text(aes(0,tsquare$ucl,label = paste('UCL:',tsquare$ucl), vjust = -1,hjust=-2))+
    theme(legend.position='bottom')+ggtitle("Hotelling T-Squared")
  
}



### Hotelling Tsquare and Mcusum
PC_T_Mcumsum_comb <- PC_old

for (i in seq(1)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  
  tsquare<-mult.chart(PC_T_Mcumsum_comb,type="t2",phase=1,alpha=0.005, keep.source = FALSE)
  mcusum <-mult.chart(type = "mcusum", PC_T_Mcumsum_comb,k=1.5, h=6, phase=1)
  outliers = (tsquare$t2 > tsquare$ucl | mcusum$t2 > mcusum$ucl)
  print(sum(outliers))
  print(paste('Outliers:',sum(outliers)))  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_T_Mcumsum_comb<-PC_T_Mcumsum_comb[inctrl_index,]
  
  nrow(PC_T_Mcumsum_comb)
}


if (1<2){
  mc<-as.data.frame(mcusum$t2,)
  mc$Index<- seq(1,nrow(mc))
  names(mc)<-c("T_2","Index")
  
  p <- ggplot(mc,aes(y = T_2,x=Index,color = ifelse( T_2 < mcusum$ucl, "In-Control Data Points", "Out of Control")))
  p + geom_point() +geom_hline(yintercept = mcusum3$ucl)+
    geom_text(aes(0,mcusum3$ucl,label = paste('UCL:',mcusum3$ucl), vjust = -1,hjust=-2))+
    theme(legend.position='bottom')+ggtitle("MCUSUM & T-Squared Combined")
  
}







### Hotelling Tsquare and MEWMA


PC_T_MEWMA_comb <- PC_old

for (i in seq(1)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  
  tsquare<-mult.chart(PC_T_MEWMA_comb,type="t2",phase=1,alpha=0.005, keep.source = FALSE)
  mw<-mult.chart(type = "mewma", PC_T_MEWMA_comb, lambda=0.5, phase=1)
  critical_val = mewma.crit(l=0.5,L0=200,p=4)
  outliers = (tsquare$t2 > tsquare$ucl | mw$t2 > critical_val)
  print(sum(outliers))
  print(paste('Outliers:',sum(outliers))) 
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_T_MEWMA_comb<-PC_T_MEWMA_comb[inctrl_index,]
  
  
  nrow(PC_T_MEWMA_comb)
}
if (1<2){
  me<-as.data.frame(mw$t2,)
  me$Index<- seq(1,nrow(me))
  names(me)<-c("T_2","Index")
  
  p <- ggplot(me,aes(y = T_2,x=Index,color = ifelse( T_2 < critical_val, "In-Control Data Points", "Out of Control")))
  p + geom_point() +geom_hline(yintercept = critical_val)+
    geom_text(aes(0,critical_val,label = paste('UCL:',critical_val), vjust = -1,hjust=-2))+
    theme(legend.position='bottom')+ggtitle("MEWMA & T-Squared Combined")
  
}




