#This script is a test script to explore adding ages beyond dat$Nages in
#calculating a plus group. 
#This script is not self contained. Need to run the data loading first. 
#Ultimately we do not use this given that switching to a normal distribution
#automatically adds plus group dynamics. 


#test length comps to see if there is a plus group
temp <- pivot_longer(dat$lencomp, cols = l10:l200, names_to = "length")
temp$length_num = as.numeric(substr(temp$length,2,4))

ggplot(filter(temp,FltSvy == 3), aes(x=Yr, y=length_num, size = value)) +
  geom_point(alpha=0.7)
#dont see any lengths much above 150 so not aggregated into plus group for lengths


##
#Generate ALK with added ages to calculate plus groups
##
plusA = 20 #how many ages to go beyond plus group
#couldnt figure out estimating VonB parameters, so calculate the change in growth from age to age
newlen=rep(NA,plusA)
for(i in 1:plusA){
  if(i==1)  newlen[i] = Latage[dat$Nages]  + 0.455*(145 - Latage[dat$Nages])   
  if(i!=1) newlen[i] = newlen[i-1] + 0.455*(145-newlen[i-1])
}
alk=matrix(NA,nrow=length(Latage)+plusA,ncol=(length(lengths)-1),dimnames = list(paste0("a",seq(1:(length(Latage)+plusA))),paste0("l",(lengths[1:(length(lengths)-1)]))))
for(a in 1:(length(Latage)+plusA)){ #loop over 28 ages and the length bins and calculate probability for each bin
  for(l in 1:(length(lengths)-1)){
    alk[a,l]=getprob(L1=lengths[l],L2=lengths[l+1],meanL=ifelse(!is.na(Latage[a]),Latage[a],newlen[a-dat$Nages]),sd=0.1)
    #alk[a,l]=getprob(L1=lengths[l],L2=lengths[l+1],meanL=log(ifelse(!is.na(Latage[a]),Latage[a],newlen[a-dat$Nages])),sd=0.1)
    }
}
#alk=round(alk,digits=2)
plot(alk[dat$Nages+plusA,],x=seq(10,200,by=5),ylab="Prob is oldest age by length",xlab = "Length bin (lower edge)")
noentry = which(colSums(alk)==0) #determine which lengths dont have any entries
#alk[1,noentry[1]]=1 #set the first column to be 1 for the first age 1
alk[1,noentry[c(1,2)]]=1 #set the first column to be 1 for the first age 1
#alk[dat$Nages+plusA,noentry[-1]]=1 #set the last columns to be 1 for the last age
alk[dat$Nages+plusA,noentry[-c(1,2)]]=1 #set the last columns to be 1 for the last age
alk=t(t(alk)/colSums(alk)) #divided by column sums to produce P(A|L); i.e. column sums among ages = 1
#alk=round(alk,digits=2)
plusGroup = colSums(alk[dat$Nages:(dat$Nages+plusA),])
alk=alk[1:dat$Nages,]
alk[dat$Nages,]=plusGroup

pluss = round(plusGroup,2)
pluss = rbind(pluss,round(plusGroup,2))

#Plots to add figure as shown in github issue #52
matplot(t(pluss),x=seq(10,200,by=5),type="l",xlab="length",ylab = "Probably length is of age 28+ fish", col=c(1,2,3,4,5),lty=c(1,2,3,4,5))
legend("topleft",c("age28","age29","age33","age38","age48"),col=c(1,2,3,4,5),lty=c(1,2,3,4,5))
