library(MASS)
library(remotes)
library(readr)
library(stringr)
library(dplyr)
source('Queueing.R')
options(scipen = 999)

objStationName<-c("nanshao","shagao","shahe","gonghua","zhuxin","shengming","xier")
objAccSet<-c("151019037","151019039","151019041","151019043","151019045","151019047","151019049")
columnNames<-c("id","type","exittime","entertime","exitacc","enteracc","etd","ett","exd","ext")
file_list <- list.files(path="/Users/vannadal/Documents/yili/filterdata/",full.names = TRUE)
lowerBound<-strptime("050000", "%H%M%S")
upperBound<-strptime("100000", "%H%M%S")

#length(file_list)
#enteracc is the same as the exitacc.The original data has to be re-processed.
dataset<-data.frame(matrix(ncol = 10, nrow = 0))
colnames(dataset)<-columnNames
#freqs=5 defines the frequent pax who travels 5 trips in selected dates
freqs<-5 
#selected datefiles in file_list
dateNum<-5 

for(i in 1:dateNum){
  print(file_list[i])
  data<-read.csv(file_list[i],header = FALSE)
  colnames(data) <- c("id","type","exittime","entertime","exitacc","enteracc")
  objData<-subset(data, enteracc %in% objAccSet)
  unique(objData[c(5)])
  
  #split enter/exit data/time
  objData<-cbind(objData,substr(objData$entertime,1,8))
  objData<-cbind(objData,substr(objData$entertime,9,14))
  objData<-cbind(objData,substr(objData$exittime,1,8))
  objData<-cbind(objData,substr(objData$exittime,9,14))
  colnames(objData) <- columnNames
  
  #filter entertime falling in [05:00:00,10:00:00]
  objData$ett <- as.character(objData$ett)
  objData$ext <- as.character(objData$ext)
  objData<-subset(objData,(strptime(objData$ett,"%H%M%S")<upperBound)&(strptime(objData$ett,"%H%M%S")>lowerBound))
  dataset<-rbind(dataset,objData)
#  nrow(dataset)
#  unique(data[c(5)])
} 
dataset$ett<-as.numeric(strptime(dataset$ett,"%H%M%S"))
dataset$ext<-as.numeric(strptime(dataset$ext,"%H%M%S"))
head(dataset)
#idset:each individual's departure 3sigma para,[id,enteracc,amount,mean,sd]
idset<-dataset %>% 
  group_by(id) %>% 
  summarise(amount=n(),meantime=as.integer(mean(ett)), sdValue=as.integer(sd(ett)))
head(idset)

#find out frequent individuals with amount>=10 and write into freqIdSet
#freqIdSet["id","amount","meantime","sdValue","mindepart","maxdepart"]
freqIdSet<-subset(idset,idset$amount>=freqs) #*********#change amount when the 
freqDataset<-subset(dataset, id %in% freqIdSet$id)
len<-nrow(freqIdSet)
minmax <- data.frame(matrix(ncol = 2, nrow = len))
colnames(minmax)<-c("mindept","maxdept")
for(j in 1:len){
    meanValue<-freqIdSet[j,3]
    if(freqIdSet[j,2]==1){
      minmax[j,1]<-meanValue
      minmax[j,2]<-meanValue
    }
    else
    {
      sdValue<-freqIdSet[j,4]
      lowerSigma<-(meanValue-3*sdValue)
      upperSigma<-(meanValue+3*sdValue)
      tmp<-with(freqDataset, ett[ (id == as.integer(freqIdSet[j,1])) & 
                                (ett >= as.integer(lowerSigma)) & 
                                (ett <= as.integer(upperSigma)) ])
      minmax[j,1]<-min(tmp)
      minmax[j,2]<-max(tmp)
    }
}
freqIdSet<-cbind(freqIdSet,minmax)

#figure 5
dtfi<- freqIdSet$maxdept-freqIdSet$mindept
fig5<-hist(dtfi,breaks=20,freq = FALSE)
summary(dtfi)

#define reward ratio and reward interval
#!!!!!must run the whole code to keep strptime consistent with the system time
rwdLowerBoundSet<-as.numeric(strptime(c("080000","073000","070000","080000","080000","080000"),"%H%M%S"))
rwdUpperBoundSet<-as.numeric(strptime(c("083000","083000","083000","090000","083000","083000"),"%H%M%S"))
centralDTFI<-c(0,0,0,0,0,0)
shoulderDTFI<-c(0,0,0,0,0,0)
lowpeakDTFI<-c(0,0,0,0,0,0)
highpeakDTFI<--c(0,0,0,0,0,0)
rwRatio<-c(0.6,0.6,0.6,0.6,0.3,0.8)

#table3:different types of passengers under 6 scenarios
for(i in 1:length(rwdLowerBoundSet)){
  for(j in 1:nrow(freqIdSet)){
    if((freqIdSet[j,"mindept"]<rwdLowerBoundSet[i]) & (freqIdSet[j,"maxdept"]>rwdLowerBoundSet[i]))
      lowpeakDTFI[i]<-lowpeakDTFI[i]+1
    else if((freqIdSet[j,"mindept"]<rwdUpperBoundSet[i]) & (freqIdSet[j,"maxdept"]>rwdUpperBoundSet[i]))
      highpeakDTFI[i]<-highpeakDTFI[i]+1
    else if((freqIdSet[j,"mindept"]>=rwdLowerBoundSet[i]) & (freqIdSet[j,"maxdept"]<=rwdUpperBoundSet[i]))
      centralDTFI[i]<-centralDTFI[i]+1
    else
      shoulderDTFI[i]<-shoulderDTFI[i]+1
  }
}
#table3
paxtypes<-rbind(centralDTFI,shoulderDTFI, lowpeakDTFI,highpeakDTFI)
table3_paxtypes<-mat/ colSums(mat)

#RevSc named table4:revenue changes under proposed scenarios
dateGroup=unique(dataset$etd)
RevSc<-data.frame(matrix(ncol=6,nrow=2))
colnames(RevSc)<-c("Scenario1","Scenario2","Scenario3","Scenario4","Scenario5","Scenario6")
for(i in 1:length(rwdLowerBoundSet)){
  #filter objective pax and put into subDataset
  subDataset<-subset(freqDataset,(freqDataset$ett>= rwdLowerBoundSet[i])&(freqDataset$ett<= rwdUpperBoundSet[i]))
  RevSc[1,i]<-as.integer(nrow(subDataset)/length(dateGroup))
  RevSc[2,i]<-3*RevSc[1,i]
}
#simulatequeue(subDataset)















