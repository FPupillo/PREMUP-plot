## file to analyse phase 2

cd<-getwd()
setwd("merged_phases")
phase3files<-list.files(pattern = ".*csv")
setwd(cd)
#initialise matrix
Perf<-matrix(NA, nrow = length(phase3files),ncol = 8) 
colnames(Perf)<-c("PartNum","perfscene4lowPE", "perfscene4highPE","perfscene5lowPE", "perfscene5highPE",
                  "perfscene6lowPE","perfscene6lhighPE", "perfmediumPE")
                
# compute recognition performance
for (j in 1 :  length(phase3files)){
  # initialise the count for the column
  coln<-2
  file<-read.csv(paste("merged_phases/",phase3files[j], sep=""))
  # select only rec_session 1 and eliminate the fillers
  file<-file[file$pred_Cond!=4 & file$rec_session==1,]
  for (s in 4:6){ # we are looping through scenes
    #subset only trials for scene s and only for old item (so we are looking only at correct recogntion, not rejection)
    # the variable indicating the scene is "pred_Cont"
    fileSub<-file[file$rec_trialType=="old" & file$pred_Cont==s,]
    #loop through the three conditions (low, medium, high PE)
    cond<-c(2,3)
    for (c in 1:2){
      filePE<-fileSub[fileSub$pred_Cond==cond[c],]
      acc<-mean(filePE$rec_acc, na.rm=T)
      Perf[j, c(1,coln)]<-c(round(as.numeric(substr(phase3files[j],5,6)),1), acc)
      coln<-coln+1
    }
  }
  #now medium PE
  filePE<-file[(file$rec_trialType=="old")& (file$pred_Cont==1 |file$pred_Cont==2|file$pred_Cont==3),]
  acc<-mean(filePE$rec_acc, na.rm=T)
  Perf[j, c(1,coln)]<-c(round(as.numeric(substr(phase3files[j],5,6)),1), acc)
}
#save it
dataframe<-data.frame(Perf)
if (!file.exists("Results Phase3.csv")){
  write.csv(dataframe, "Results Phase3.csv",row.names = F)
}
rm(list=ls())
