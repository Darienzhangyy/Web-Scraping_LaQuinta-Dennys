library(googleVis)
library(stringr)

load(LaQuinta)
load(dennys_info)

#Number of La Quinta in every state
dataLQ <- data.frame(LaQuinta)
Statecount=as.data.frame(lapply(dataLQ, table)$State)
names(Statecount)=c("State","LaQuinta")
Lq = gvisGeoChart(Statecount, locationvar='State', colorvar='LaQuinta',
                  options=list(region='US',displayMode="regions",resolution="provinces",
                               colorAxis="{colors: ['yellow','red']}", backgroundColor="cornflowerblue",
                               width=700, height=430))
plot(Lq)


#Number of Denny's in every state
dataDen = data.frame(dennys_info)
#get the last split in address, unlist it to get a vector
DenState=unlist(lapply(str_split(dataDen$address, ", "), function(x) x[length(x)]))
#split the blanks
DenState=unlist(lapply(str_split(DenState, " "), function(x) x[1]))
#get the state count of Denny's
DenState=as.data.frame(lapply(as.data.frame(DenState), table))
names(DenState)=c("State","Dennys")
#latlong<-paste(dataDen$latitude, dataDen$longitude, sep=":")
Den = gvisGeoChart(DenState, locationvar='State', colorvar='Dennys',
                     options=list(region="US",displayMode="regions",resolution="provinces",
                                  colorAxis="{colors: ['yellow','red']}", backgroundColor="cornflowerblue",
                                  width=700, height=430))
plot(Den)

