
#filter record("id","type","exittime","entertime","exitacc","enteracc") 
# where enterstation in objAccSet from the original dataset

dir_list <- list.files(path="/Users/vannadal/Documents/yili/smartcarddata",full.names = TRUE)
columnNames<-c("id","type","exittime","entertime","exitacc","enteracc")
objAccSet<-c("151019037","151019039","151019041","151019043","151019045","151019047","151019049")

for(i in 1:length(dir_list)) {
  len<-nchar(dir_list[i])
  date<-substr(dir_list[i],len-7,len)
  print(dir_list[i])
  file_list <- list.files(path=dir_list[i],full.names = TRUE)
  for (j in 1:length(file_list)) {
    data <- read.csv(file_list[j],header = FALSE)
    subdata <- data[c(7,8,4,14,23,24)]
    colnames(subdata)<-columnNames
    objData<-subset(subdata, enteracc %in% objAccSet)
    filename<-paste("/Users/vannadal/Documents/yili/preprocess/",date,".csv",sep = "")
    #when writing csv file, using write.table instead of writing.csv. cause append is not avaiable for .csv
    write.table(objData,file=filename,append = T,row.names = FALSE,sep=",",col.names = FALSE)
  }
}