bank_full<-read.csv("bank-full.csv",header=TRUE,sep=";")
# Find length if data set
n=length(bank_full[,1])
for(i in 1:17){
  cat(i, class(bank_full[,i]),'\n')
}

# Controll that factors are non empty with respect to response
xtabs(~y + job, data = bank_full)
#....for all factors, no zeros!

# Add variable, NPC (client was not previously contacted 1=yes)
# and remove -1 in pdays
bank_full$NPC=(bank_full$pdays==-1)
bank_full$NPC=factor(bank_full$NPC)
bank_full[bank_full$pdays==-1,14]=0



logitMod <- glm(y ~age+job+marital+education+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+NPC+previous+poutcome-1, data = bank_full, family = "binomial")
confint(logitMod)

#Model selection <<data dredging>>
# cross validation


predict.glm(logitMod,bank_full[1,1:16],type='response')



# Ikke tilgjengelig:
# 1. Tidligere kjoept over tlf eller enda bedre #kjoep/#ringt.
# 2. Lønn (gjennomsnittlig maanedlig intekt, banken har vel tilgang på det?)
# 3. Hvem var selgeren (% sukse rate)
# 4. Når på dagen ble det ringt
# 5. ukedag/hellig dag




################################################################
############# Oppgave 2                            #############
################################################################
# Tenke gjennom og komme med konkrete anbefalinger for hvordan 
# du ville anvendt modellen i praksis?
# 
# 1. Hvem skal man bruke tid og ressurser på for å ringe.
# 2. Når man skal ringe (bedre hvis mer data var tilgjengelig her
# som klokkeslett og uke/heligdag)
# 4. Hvis vi hadde selgeren % sukse rate, Hvem skal ringe.
# 3. Hvis banken tilbød flere producter, kunne slik statistikk
# fortelle hva kunden sannsynligvis ville være mest interessert i, 
# og for anbefaling.



# X. Hvor naar er man ferdig med å snakke med kunde som ikke er interessert?

