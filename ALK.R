rm(list = ls())
#parameters of the growth equation, which I don't use below because I don't understand the equation either
Linf=145
Lmin=22
kbase=0.455
k_a=c(0.5,
      0.75,
      1,
      1,
      1,
      1.8,
      1.8,
      1.2,
      1.2,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1
)

#CV around mean length at age
cv=0.1
variance=log(cv^2+1)

#Mean length at age as reported in table A1 (we should be able to replicate these with equation 1, but maybe we don't really have to)
Latage=c(22,
         35.2865,
         41.384,
         45.7348,
         49.904,
         53.8991,
         57.7273,
         64.2198,
         74.721,
         85.5489,
         96.3811,
         105.259,
         112.534,
         118.496,
         123.383,
         127.387,
         130.669,
         133.359,
         135.563,
         137.369,
         138.85,
         140.063,
         141.058,
         141.872,
         142.54,
         143.088,
         143.536,
         144.168
)

#length bins for which I want a probability
lengths=seq(9.5,201.5,by=1)

#function to calculate probability in a given length interval
getprob=function(L1=NULL,L2=NULL,meanL=NULL,sd=NULL){
prob=pnorm(L2,mean=meanL,sd=sd)-pnorm(L1,mean=meanL,sd=sd)
return(prob)
}

#matrix to hold alk
alk=matrix(NA,nrow=length(Latage),ncol=(length(lengths)-2),dimnames = list(seq(1:length(Latage)),(lengths[2:(length(lengths)-1)]-0.5)))
#loop over 28 ages and the length bins and calculate probility for each bin
for(a in 1:length(Latage)){
  for(l in 1:(length(lengths)-2)){
  alk[a,l]=getprob(L1=lengths[l],L2=lengths[l+1],meanL=Latage[a],sd=sqrt(variance))
  }
}
alk=round(alk,digits=2) #round probs to 2 digits
alk=t(t(alk)/colSums(alk)) #divided by column sums to produce P(A|L); i.e. column sums among ages = 1
write.csv(alk,file=paste("C:\\Spatial_SPASAM_2021_Sim","alk.csv",sep="\\")) #save it if needed.
