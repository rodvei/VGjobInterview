# profit returns the profit earned from calling
###############################################
# prob= model predicted probability
# real= what the costommer choosed
# plim= probability limit for when to call if > than => call
# CofC= Average Crice of Calling
# Earn= Expected eranings if 'yes' by costumer
profit<-function(pred=NA,real=NA, plim=0.5, CofC=NA, Earn=NA){
  totalCofC<-sum(pred>plim)*CofC
  totalEarn<-sum(real[which(pred>plim)])*Earn
  return(totalEarn-totalCofC)
}

scroingTabel<-function(pred,test){
  tab<-matrix(rep(NA,10),1)
  # predicted values
  colnames(tab)<-c('0-10%','10-20%','20-30%','30-40%','40-50%','50-60%','60-70%','70-80%','80-90%','90-100%')
  rownames(tab)<-'realAverage'
  for(i in 1:10){
    tab[i]<-mean(test[pred<(i/10)&pred>((i-1)/10)])
  }
  return(tab)
}