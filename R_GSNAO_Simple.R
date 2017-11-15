rm(list=ls())
gsindex=read.table("GSI_Annual_text.txt",header=T)
head(gsindex)
ice=read.table("IceLow_text.txt",skip=1,header=T)
head(ice)
iceyearly <- tapply(ice$Pressure,ice$Year,FUN=mean)
ice_data_full <- data.frame(Year=names(iceyearly),mean=iceyearly)
ice_data=ice_data_full[19:63,] # selected for 1966 to 2010
#
azr=read.table("Azr_High_text.txt",skip=1,header=T)
azryearly <- tapply(azr$Pressure,azr$Year,FUN=mean)
azr_data_full <- data.frame(Year=names(azryearly),mean=azryearly)
azr_data= azr_data_full[19:63,] # selected for 1966 to 2010
azr_data
#
gs_data=gsindex$Ann
ice_data1=ice_data$mean
azr_data1=azr_data$mean
# Lets keep the same numbers for the Markov Chain below
inits=list(
  list( alpha=1, beta=1, tau=.01),# , #chain 1
  list( alpha=2, beta=1, tau=.5),#, #chain 2
  list( alpha=3, beta=1, tau=.01) #chain 3
)

data1=list(
  n=45,
  y=as.double(gsindex$Ann),
  AH=as.double(ice_data1),
  IL=as.double(azr_data1)
)
library(rjags)
n.adapt=5000
n.update=10000
n.iter=10000
##call to JAGS
set.seed(1)
jm=jags.model("GSNAO_Simple_model.txt",data=data1, inits,
              n.chains=length(inits), n.adapt = 5000)
update(jm, n.iter=n.update)
zm=coda.samples(jm,variable.names=c("alpha", "beta", "sigma", "tau"),
                n.iter=n.iter, n.thin=1)

summary(zm)

plot(zm)
par(mfrow=c(2,2))
traceplot(zm)
densplot(zm)
gelman.diag(zm)

heidel.diag(zm)
raftery.diag(zm)



