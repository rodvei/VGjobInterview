bank_full<-read.csv("bank.csv",header=TRUE,sep=";")
# Find length if data set
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

# Minus poutcome, day, month, duration
logitMod <- glm(y ~age+job+marital+education+default+balance+housing+loan+contact+campaign+pdays+NPC+previous, data = bank_full, family = binomial(logit))
# Check for probit too?

confint(logitMod)
summary(logitMod)
Q<-step(logitMod)

test <- glm(y ~marital+education+housing+loan+contact+day+month+duration+campaign+pdays+NPC, data = bank_full, family = binomial(logit))
table(true=bank_full$y,pred =round(fitted(logitMod)))
table(true=bank_full$y,pred =round(fitted(test)))
table(true=bank_full$y,pred =bank_full$poutcome)
