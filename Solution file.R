# Bank term deposit (Can only withdrawn after the term has ended or 
# by giving a predetermined number of days notice)

bank_full<-read.csv("bank-full.csv",header=TRUE,sep=";")
bank_full$y<-bank_full$y=="yes"
# Find length of data set
n<-nrow(bank_full)
for(i in 1:17){
  cat(i, class(bank_full[,i]),'\n')
}

# Controll that factors are non empty with respect to response
xtabs(~y + job, data = bank_full)
#....for all factors, no zeros!

# Add variable, NPC (client was not previously contacted 1=yes)
# and remove -1 in pdays
bank_full$NPC<-(bank_full$pdays==-1)
bank_full$NPC<-factor(bank_full$NPC)
bank_full[bank_full$pdays==-1,14]<-0
bank_full<-bank_full[,c("age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","NPC","previous","poutcome","y")]
# Ordering the dataframe, so NPC is after pdays


##################### Model fitting ########################

# Minus poutcome, day, month, duration
logitMod <- glm(y ~age+job+marital+education+default+balance+housing+loan+contact+campaign+pdays+NPC+previous+poutcome, data = bank_full, family = binomial(logit))
# possible check for probit too? neural network?
summary(logitMod)

##################### Model selection ########################

# step (forward, backward and both), could also use likelihood-ratio test with more time.
###DONT RUN###
step(logitMod,direction="both") 
# Result: full model minus age and pday
step(logitMod,direction="backward") 
# Result: full model minus age and pday
step(glm(bank_full$y~1,family=binomial (link=logit)), direction="forward",scope=~bank_full$age+bank_full$job+bank_full$marital+bank_full$education+bank_full$default+bank_full$balance+bank_full$housing+bank_full$loan+bank_full$contact+bank_full$campaign+bank_full$pdays+bank_full$NPC+bank_full$previous+bank_full$poutcome,data=bank_full)
# Result: full model minus age and pday
# "step" conclude the full model minus age and pday is best (but probably over fitted)
optimMod <- glm(y ~job+marital+education+default+balance+housing+loan+contact+campaign+NPC+previous+poutcome, data = bank_full, family = binomial(logit))
summary(optimMod)
# Minus age and pday
predict.glm(optimMod,bank_full[1,],type='response')
table(pred =round(fitted(optimMod)),true=bank_full$y)


##################### Cross validation ########################

# x% using in training and test set (cross validation)
x=60
set.seed(1989)
randomSet<-sample(1:n)
bankTrain<-bank_full[randomSet[1:floor(x/100*n)],]
bankTest<-bank_full[randomSet[ceiling(x/100*n):n],]

crossMod<-glm(y ~job+marital+education+default+balance+housing+loan+contact+campaign+NPC+previous+poutcome, data = bankTrain, family = binomial(logit))
predTest<-predict.glm(crossMod,bankTest,type='response')
AIC<-rep(NA,13)


##################### ROC on Cross validation ########################
#ROC (Receiver operating characteristic)
library(ROCR)
col<-rainbow(13)
AIC<-rep(NA,13)
AIC[1]<-crossMod$aic

pred <- prediction(predTest, bankTest$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=col[1],main='ROC')
abline(0,1,col='red',lty=2)

test<-c(2,3,4,5,6,7,8,9,13,15,16,17)
for(i in 1:12){ #7:12){ 
  modi<-glm(y ~ ., data = bankTrain[,c("y",colnames(bankTrain)[test[-i]])], family = binomial(logit))
  predTesti<-predict.glm(modi,bankTest,type='response')
  predi <- prediction(predTesti, bankTest$y)
  perfi <- performance(predi, measure = "tpr", x.measure = "fpr")
  plot(perfi,col=col[i+1],add=TRUE)
  AIC[i+1]<-modi$aic
}
legend("bottomright", inset=0,paste(rep('mod',12), 0:12, sep = ""), fill=col, horiz=FALSE,cex=1/2)
# looks OK with the original model
AIC
which(AIC==min(AIC))
colnames(bankTrain)[test[which(AIC==min(AIC))]]




##################### Scoring Tabel ########################
# see function
scroingTabel(predTest,bankTest$y)



##################### Other Relationships ########################
summary(glm(y ~age+job+marital+education+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+NPC+previous+poutcome, data = bank_full, family = binomial(logit)))
# non-significant age, default, pdays, NPC, previous...

# call as late in the moneth as possible (why, need futher investigation)
# good month to call: mar, sep, oct, dec
# bad month to call: jan, jul, nov
# Make the call last long (duration)



##################### MAX PROFIT ########################
# Seller Hour Payment (SHP)
SHP<-10 # Euro
# Cost of call (CofC)=SHP*(averagetalktime(in seconds)*%brakes+time between call)/3600
CofC<-SHP*(mean(bankTrain$duration)*(1+1/8)+10)/3600
# Earn if respons is yes
Earn<-10 # assume
prof<-rep(NA,50)
for(i in 1:50){
  prof[i]<-profit(predTest,bankTest$y,plim=(i*2)/100,CofC=CofC, Earn=Earn)
}
plot(2*(1:50),prof,type='l',xlab='Prob in %',ylab='Profit')
which(prof==max(prof))*2





##################### Ekstra info oensket ########################
# Ikke tilgjengelig:
# 1. plasebo gruppe (kontroll funker det aa ring? stoerelse ish 10%?)
# 2. ABtesting forskjellig budskap (foreks 2 salgs argumenter) 
# fra selgeren (kryss til eldre)
# 3. Tidligere kjoept over tlf eller enda bedre #kjoep/#ringt.
# 4. Laann (gjennomsnittlig maanedlig intekt, banken har vel tilgang paa det?)
# kanskje i stedet for yrke?
# 5. Hvem var selgeren (% sukse rate)
# 6. Naar paa dagen ble det ringt
# 7. ukedag/hellig dag
# 8. (if sale) expected earnings from that costummer.
# contact!! unknown, hva betyr det?? Hvor mange kjoepere 




################################################################
############# Oppgave 2                            #############
################################################################
# Tenke gjennom og komme med konkrete anbefalinger for hvordan 
# du ville anvendt modellen i praksis?
# 
# 1. hvordan selge ABplan
# 2. bruke data for predict frafall fra banken
# 3. Hvem skal man bruke tid og ressurser paa for aa ringe for aa selge.
# 4. Naar man skal ringe (bedre hvis mer data var tilgjengelig her
# som klokkeslett og uke/heligdag)
# 5. Hvis vi hadde selgeren % sukse rate, Hvem skal ringe.
# 6. Hvis banken tilbyr flere producter, kunne slik statistikk
# fortelle hva kunden sannsynligvis ville vaere mest interessert i, 
# og for anbefaling.
# 7. Forventet telfon tid => pris for aa ringe opp.




# (X. Hvor naar er man ferdig med ? snakke med kunde som ikke er interessert?)

