library(dequer)

#subdataset is the objective pax set, filtered in reward.r
#each pax' queueing time are writed into result(line 101)

#return resultBoardingSet:col("id","waitingtime","exittime","boardingtime"),queueing time=boardingtime-waitingtime
simulatequeue<-function(subdataset){
    dataset<-subset(subDataset,subDataset$enteracc<=subDataset$exitacc)#filter the whole downstream pax taking changping line 
    walktimeSet<-read.csv("/Users/vannadal/Documents/yili/walktime.csv",header  = TRUE)
    timetable<-read.csv("/Users/vannadal/Documents/yili/timetable201706.csv",header  = TRUE)
    
    #convert  timetable to timetableSet: [cabinNum,stations]
    timetableSet<-NULL
    for(i in 2:ncol(timetable)){
      tmp<-as.numeric(strptime(timetable[,i],"%H:%M:%S"))
      timetableSet<- rbind(timetableSet,tmp)
    }
    timetableSet<-t(timetableSet)
    
    queueTimeset<-data.frame(matrix(nrow = length(dataset),ncol=3))
    colnames(queueTimeset)<-c("id","entertime","queuetime")
    cabinNum<-ncol(timetableSet)
    #initialist two queue lists :cabinQueue and stationQueue
    stationnameSet<-timetable[,1]
    Capacity<-matrix(1460,ncol = ncol(timetableSet)+1,nrow = nrow(timetableSet)+1)

    #create cabinQueue[i] to carry pax
    cabinQueue<-list()
    for(i in 1:cabinNum){
      cabinQueue[i]<-list()
    }
    
    #create stationQueue[i] to simulate the waiting pax at station i
    stationQueue<-list()
    for(i in 1:length(stationnameSet)){
      stationQueue[i]<-list()
    }
    
    #push each pax(id,stationname,waitingtime,exittime) to stationQueue[i] by waitingtime in ascending order
    for(i in 1:length(stationnameSet)){
      stationPaxSet<-subset(dataset,dataset$enteracc==stationnameSet[i])
      stationPaxSet<-stationPaxSet[order(stationPaxSet$ett),]
      # pop the initialized value:null 
      pp<-as.queue(stationQueue[i])
      pop(pp)
      if(nrow(stationPaxSet)>=1){
        for(j in 1:nrow(stationPaxSet)){
          element<-c(stationPaxSet[j,"id"],stationPaxSet[j,"ett"]+walktimeSet[i,3],stationPaxSet[j,"ext"])
          pushback(pp,element)
        }
        stationQueue[i]<-list(pp)
      }
    }
    #pop the firstelement which is null initializedly.
    for (i in 1:length(stationnameSet)) {
      queue<-stationQueue[[i]]
      if (length(queue)) {
        element<-pop(queue)
        print(element)
      }
    }

    resultBoardingSet<-NULL
    for(i in 1:cabinNum){
      for(j in 1:length(stationnameSet)){
        if(!is.null(stationQueue[[j]]))
        {
          if(i==1){
            lastExitime<-timetableSet[j,1]          
          } else{
            lastExitime<-timetableSet[j,i-1]
          }
          currentExitime<-timetableSet[j,i]
          exitPaxSet<-subset(dataset,(dataset$exitacc==stationnameSet[j]) &
                               (dataset$ext>=lastExitime) & (dataset$ext<=currentExitime))
          Capacity[j,i]<-Capacity[j,i]+nrow(exitPaxSet)
          # if(Capacity[j,i]>=1460)
          #   Capacity[j,i]<-1460
        
          sq<-stationQueue[[j]]
          if(length(sq)>0){
            cq<-as.queue(cabinQueue[i])
            sqList<-as.list(sq)
  
            count=1
            elementList<-as.vector(unlist(sqList[count]))
            while ((!is.null(elementList)) & (Capacity[j,i]>0) & (elementList[2]<timetableSet[j,i]))
            {
              popElem<-pop(sq)
              element<-c(popElem[1],popElem[2],popElem[3],timetableSet[j,i])
              resultBoardingSet<-rbind(resultBoardingSet,c(popElem[1],popElem[2],popElem[3],timetableSet[j,i]))
              pushback(cq,element)
              print(element)

              count<-count+1
              Capacity[j,i]<-Capacity[j,i]-1
              elementList<-as.vector(unlist(sqList[count]))
              if(is.null(elementList)) 
                break
           #   print(elementList)
             # vec<-c(i,j,Capacity[j,i],elementList[2],timetableSet[j,i])
              #print(vec)
            }
            cabinQueue[i]<-list(cq)
            stationQueue[j]<-list(sq)
            Capacity[j+1,i]<-Capacity[j,i]            
          } 
        }
      }
    }
  #resultBoardingSet col("id","waitingtime","exittime","boardingtime")    
  colnames(resultBoardingSet)<-c("id","waitingtime","exittime","boardingtime")
   return (resultBoardingSet)
}
    