rm(list=ls())
gsindex=read.table("~/Desktop/u_Jags/GSI_Annual_text.txt",header=T)
head(gsindex)
ice=read.table("~/Desktop/u_Jags/IceLow_text.txt",skip=1,header=T)
head(ice)
iceyearly <- tapply(ice$Pressure,ice$Year,FUN=mean)
ice_data_full <- data.frame(Year=names(iceyearly),mean=iceyearly)
ice_datapm2=ice_data_full[19:63,] # selected for 1966 to 2010
ice_datapm1=ice_data_full[20:63,]
ice_datap=ice_data_full[21:63,]
#
azr=read.table("~/Desktop/u_Jags/Azr_High_text.txt",skip=1,header=T)
azryearly <- tapply(azr$Pressure,azr$Year,FUN=mean)
azr_data_full <- data.frame(Year=names(azryearly),mean=azryearly)
azr_datapm2= azr_data_full[19:63,] # selected for 1966 to 2010
azr_datapm1= azr_data_full[20:63,]
azr_datap= azr_data_full[21:63,]
#names(gs.data)=c("Year","GS path","Iceland","Azores")
#Arrange data frame in ascending order of population size.
#This becomes important for plotting
#credible intervals later.
#pop.data = pop.data[order(pop.data$"Population Size"),]
# I am not sure what those data files will be for me
# possibilities: GS path, Icelandic Low and Azores High 
# time-series of annual values
gsdatap=gsindex$Ann[3:45]
gsdatapm1=gsindex$Ann[2:45]
gsdatapm2=gsindex$Ann[1:45]

# Lets keep the same numbers for the Markov Chain below
inits=list(
  list( alpha=1, beta=1, ILPr=1000, tau=.01),# , #chain 1
  list( alpha=2, beta=1, ILPr=1005, tau=.5),#, #chain 2
  list( alpha=3, beta=1, ILPr= 995, tau=.01) #chain 3
)

data1=list(
  n=43,
  yp=as.double(gsdatap),
  ypm1=as.double(gsdatapm1),
  ypm2=as.double(gsdatapm2),
  AHp=as.double(azr_datap$mean),
  ILp=as.double(ice_datap$mean),
  AHpm1=as.double(azr_datapm1$mean),
  ILpm1=as.double(ice_datapm1$mean),
  AHpm2=as.double(azr_datapm2$mean),
  ILpm2=as.double(ice_datapm2$mean)
)
library(rjags)
n.adapt=500
n.update=1000
n.iter=1000
##call to JAGS
set.seed(1)
jm=jags.model("~/Desktop/u_Jags/GSNAO_Complex01.txt",data=data1, inits,
              n.chains=length(inits), n.adapt = 500)
update(jm, n.iter=n.update)
zm=coda.samples(jm,variable.names=c("ILPr", "alpha", "beta", "sigma", "tau"),
                n.iter=n.iter, n.thin=1)

summary(zm)

plot(zm)
par(mfrow=c(2,2))
traceplot(zm)
densplot(zm)
gelman.diag(zm)

heidel.diag(zm)
raftery.diag(zm)



