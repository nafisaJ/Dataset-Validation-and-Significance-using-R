library(gdata)
library(ggplot2)
library(gridExtra)
library(grid)

options("max.print" = 999999)

checkIfInRange <- function(FilenameFormat,FinalfileName) {
  #print(FilenameFormat)
  FinalfileData=read.xls(FinalfileName, header=TRUE, stringsAsFactors=FALSE)
  if(FilenameFormat=="res"){
    if(ncol(FinalfileData)>8)
      FinalfileData=FinalfileData[, 1:8]
    
    if(ncol(FinalfileData)==8)
      FinalfileData=FinalfileData[,-7]
    colnames(FinalfileData)=c("Frame","Time","Speed",	"Acceleration","Braking","Steering","Lane Position")
    
  }
  FinalfileData = as.data.frame(na.omit(sapply(FinalfileData, as.numeric) )  )
   
  switch(FilenameFormat, 
         "BR"={
           errorResults=FinalfileData[FinalfileData[, "X.1"] < 4 | FinalfileData[, "X.1"] > 70,]
           if(nrow(errorResults)>0){
             print(getwd())
             print(FinalfileName)
             print(errorResults)
             return("-1")
           }
         },
         "HR"={
           errorResults=FinalfileData[FinalfileData[, "X.1"] < 40 | FinalfileData[, "X.1"] > 140,]
           if(nrow(errorResults)>0){
             print(getwd())
             print(FinalfileName)
             print(errorResults)
             return("-1")
           }
         },
         "peda"={
           errorResults=FinalfileData[FinalfileData[, "X.1"] < 10 | FinalfileData[, "X.1"] > 4700,]
           if(nrow(errorResults)>0){
             print(getwd())
             print(FinalfileName)
             print(errorResults)
             return("-1")
           }
         },
         "res"={
           errorResults=FinalfileData[FinalfileData[, "Acceleration"] < 0 | (FinalfileData[, "Speed"] >= -0.1 & FinalfileData[, "Speed"] <= 0.1 )  | FinalfileData[, "Braking"] >300,]
           
           
           
           temp=FinalfileData[ "Acceleration"]
           if(nrow(FinalfileData[FinalfileData[, "Acceleration"] < 0,]["Acceleration"])>0){
             
             temp[temp["Acceleration"]<0]=NA
           }
           colnames(temp)="NR Accelaration"
           FinalfileData=cbind(FinalfileData,temp)
           
           temp=FinalfileData[ "Speed"]
           if((nrow(FinalfileData[FinalfileData[, "Speed"] >= -0.1 & FinalfileData[, "Speed"] <= 0.1 ,]["Speed"])>0)) 
             temp[temp[ "Speed"] >= -0.1 & temp[ "Speed"] <= 0.1 ,]=0
           
           if(nrow(FinalfileData[FinalfileData[, "Speed"] < -0.1,]["Speed"])>0)
             temp[temp[ "Speed"] < -0.1,]=NA
           colnames(temp)="NR Speed"
           FinalfileData=cbind(FinalfileData,temp)
           temp=FinalfileData[ "Braking"]
           if(nrow(FinalfileData[FinalfileData[, "Braking"] >300,]["Braking"])>0)
             temp[temp["Braking"]>300,]=300
           colnames(temp)="NR Braking"
           FinalfileData=cbind(FinalfileData,temp)
           write.csv(FinalfileData, file = FinalfileName, row.names=FALSE, sep=",")
           if(nrow(errorResults)>0){
             setwd(workingDir)
             setwd("updatedRESFiles")
             write.csv(FinalfileData, file = FinalfileName, row.names=FALSE)
           }
           return("1")
         }
         
         #}
  )
  return("1")
}


getGGPlot <- function(chan,folder) {
  sessionPlot=list()
  if(!is.na(folder) & !folder=="")
  {
    print(folder)
    title=""
    lab_y=""
    lab_x="Time [sec]"
    switch(as.character(dataChannels[chan]), 
           "BR"={
             title="Breathing Rate"
             lab_y="BR[bpm]"
           },
           "HR"={
             title="Heart Rate"
             lab_y="HR[bpm]"
           },
           "peda"={
             title="Palm EDA"
             lab_y="EDA[K Ohm]"
           },
           "res"={
             title="Accelaration Signals"
           },
           "pp"={
             title="Perinasal Perspiration"
             lab_y="PP[*Celcius^2]"
           },
           {
             #print('default')
           }
    )
    numOfRows=0;
    
    #code for Raw Data 
    # !dataChannels[chan]=="performance..res." & 
    if(!dataChannels[chan]=="res"){
      # metaDataSetrelVal=metaDataSet[is.na(metaDataSet[dataChannels[chan]])==FALSE & !metaDataSet[dataChannels[chan]]==0 &  (grep(as.character(folder),metaDataSet$Session)) ,]
      metaDataSetrelVal=metaDataSet[is.na(metaDataSet[dataChannels[chan]])==FALSE & !metaDataSet[dataChannels[chan]]==0 ,]
      metaDataSetrelVal=metaDataSetrelVal[grep(as.character(folder),metaDataSetrelVal$Session),]
      
    }else{
      #metaDataSetrelVal=metaDataSet[is.na(metaDataSet["performance..res."])==FALSE & !metaDataSet["performance..res."]==0 &  metaDataSet["Session"]==(pattern = paste(as.character(folder))),]
      metaDataSetrelVal=metaDataSet[is.na(metaDataSet["performance..res."])==FALSE & !metaDataSet["performance..res."]==0 ,]
      metaDataSetrelVal=metaDataSetrelVal[grep(as.character(folder),metaDataSetrelVal$Session),]
      
    }
    
    numOfRows=length(unique(metaDataSetrelVal$Subject))
    if(!nrow(metaDataSetrelVal)==0)
    {
      drawPath=cbind(rep(str,each=nrow(metaDataSetrelVal)),'/',metaDataSetrelVal["Subject"],'/')
      colnames(drawPath) <- c("ParentDir","FstSlash","Subject","SecSlash")
      drawPath=paste(drawPath$ParentDir,drawPath$FstSlash,drawPath$Subject,drawPath$SecSlash,sep = "")
      for(i in 1:length(drawPath)) 
      {
        print(drawPath[i])
        setwd(drawPath[i])
        tempArr = strsplit(drawPath[i], "/")
        folderT = lapply(tempArr, tail, 1) 
        if(folderT == list.files())
          setwd(as.character(folderT))
        FinalFolder=list.files(pattern = paste(as.character(folder)))
        setwd(as.character(FinalFolder))
        FinalfileName=list.files(pattern = paste(dataChannels[chan]))
       
        print(FinalfileName)
        if(!length(FinalfileName)==0)
        {
          if(!dataChannels[chan]=="res")
            FinalfileData=read.xls(FinalfileName, stringsAsFactors=FALSE)
          else
            FinalfileData=read.csv(FinalfileName, stringsAsFactors=FALSE)
          # FinalFileHeaders=FinalfileData[1,]
          # FinalfileData=FinalfileData[c(2:nrow(FinalfileData)),]
          FinalfileData = sapply(FinalfileData, as.numeric)
          FinalfileData = data.frame(FinalfileData)
          #verify for pp
          FinalfileData=FinalfileData[colSums(!is.na(FinalfileData)) > 0]
          if(as.character(dataChannels[chan])=="pp"){
            FinalfileData=FinalfileData[, 1:4]
            colnames(FinalfileData) <- c("Perinasal.Perspiration","X","X.1","X.2")
          }
          
          temp1 = rep(as.character(folderT), times = nrow(FinalfileData))
          FinalfileData=cbind(FinalfileData, temp1 )
          if(i==1)
          {
            combinedData = FinalfileData
          }
          if(i!=1)
          {
            combinedData = rbind(combinedData,FinalfileData)
          }
        }
      }
      combinedData = sapply(combinedData, as.numeric)
      combinedData=data.frame(combinedData)
      if(as.character(dataChannels[chan])=="res" || as.character(dataChannels[chan])=="performance..res."){
        #cleanData=combinedData[combinedData[, "Acceleration"] > 0,]
        p1 <- ggplot(combinedData, aes((combinedData)["Time"], (combinedData)["Speed"], group=temp1, color=temp1)) + geom_line()+geom_point()+xlab(lab_x)+ylab("Speed [Km/hr]")+ ggtitle(paste("Speed (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[3][!is.na(combinedData[3]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        p1 <- ggplot(combinedData, aes((combinedData)["Time"], (combinedData)["Acceleration"], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab("Accelaration [Km/hr^2]")+ ggtitle(paste("Accelaration (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[4][!is.na(combinedData[4]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        p1 <- ggplot(combinedData, aes((combinedData)["Time"], (combinedData)["Braking"], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab("Braking [N]")+ ggtitle(paste("Braking (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[5][!is.na(combinedData[5]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        p1 <- ggplot(combinedData, aes((combinedData)["Time"], (combinedData)["Steering"], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab("Steering [rad]")+ ggtitle(paste("Steering (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[6][!is.na(combinedData[6]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        p1 <- ggplot(combinedData, aes((combinedData)["Time"], (combinedData)["Lane.Position"], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab("Lane Position [m]")+  ggtitle(paste("Lane Position (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[7][!is.na(combinedData[7]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        
        p1 <- ggplot(combinedData, aes((combinedData)["Time"], (combinedData)["NR.Speed"], group=temp1, color=temp1)) + geom_line()+geom_point()+xlab(lab_x)+ylab("Speed [Km/hr]")+ ggtitle(paste("NR Speed (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[3][!is.na(combinedData[3]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        p1 <- ggplot(combinedData, aes((combinedData)["Time"], (combinedData)["NR.Accelaration"], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab("Accelaration [Km/hr^2]")+ ggtitle(paste("NR Accelaration (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[4][!is.na(combinedData[4]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        p1 <- ggplot(combinedData, aes((combinedData)["Time"], (combinedData)["NR.Braking"], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab("Braking [N]")+ ggtitle(paste("NR Braking (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[5][!is.na(combinedData[5]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        
      }
      else if(as.character(dataChannels[chan])=="pp"){
        # # make one col
        p1 <- ggplot(combinedData, aes((combinedData)[2], (combinedData)[4], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab(lab_y)+ ggtitle(paste(title," Clean Signal (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[4][!is.na(combinedData[4]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
        # sessionPlot[[length(sessionPlot)+1]] <- p1
      }
      else{
        p1 <- ggplot(combinedData, aes((combinedData)[2], (combinedData)[3], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab(lab_y)+ ggtitle(paste(title," Raw Signal (Session:",folder,")",sep=""))+annotate("text", label = paste("n=",numOfRows,sep=""), x = max(combinedData[2][!is.na(combinedData[2]),]) , y = max(combinedData[3][!is.na(combinedData[3]),]) , size = 3, colour = "red")
        sessionPlot[[length(sessionPlot)+1]] <- p1
      }
      setwd("C:/Users/nafis/Desktop/ANA Files/output")
      if(!as.character(dataChannels[chan])=="res" & !as.character(dataChannels[chan])=="performance..res." & !as.character(dataChannels[chan])=="pp"){
        # find signals which have all values in range
        #metaDataSetrelVal2=metaDataSet[is.na(metaDataSet[dataChannels[chan]])==FALSE & metaDataSet[dataChannels[chan]]==1 &  metaDataSet["Session"]==folder,]
        metaDataSetrelVal2=metaDataSet[is.na(metaDataSet[dataChannels[chan]])==FALSE & metaDataSet[dataChannels[chan]]==1 ,]
        metaDataSetrelVal2=metaDataSetrelVal2[grep(as.character(folder),metaDataSetrelVal2$Session),]
        numOfRows=length(unique(metaDataSetrelVal2$Subject))
        
        if(!nrow(metaDataSetrelVal2)==0)
        {
          drawPath=cbind(rep(str,each=nrow(metaDataSetrelVal2)),'/',metaDataSetrelVal2["Subject"],'/')
          colnames(drawPath) <- c("ParentDir","FstSlash","Subject","SecSlash")
          
          drawPath=paste(drawPath$ParentDir,drawPath$FstSlash,drawPath$Subject,drawPath$SecSlash,sep = "")
          
          for(i in 1:length(drawPath)) 
          {
            print(drawPath[i])
            setwd(drawPath[i])
            tempArr = strsplit(drawPath[i], "/")
            folderT = lapply(tempArr, tail, 1) 
            if(folderT == list.files())
              setwd(as.character(folderT))
            FinalFolder=list.files(pattern = paste(as.character(folder)))
            setwd(as.character(FinalFolder))
            FinalfileName=list.files(pattern = paste(dataChannels[chan]))
            print(FinalfileName)
            if(!length(FinalfileName)==0)
            {
              FinalfileData=read.xls(FinalfileName, stringsAsFactors=FALSE)
              FinalFileHeaders=FinalfileData[1,]
              FinalfileData=FinalfileData[c(2:nrow(FinalfileData)),]
              FinalfileData = sapply(FinalfileData, as.numeric)       
              FinalfileData = data.frame(FinalfileData)
              FinalfileData=FinalfileData[colSums(!is.na(FinalfileData)) > 0]
              temp1 = rep(as.character(folderT), times = nrow(FinalfileData))
              FinalfileData=cbind(FinalfileData, temp1 )
              if(i==1)
              {
                cleanData = FinalfileData
              }
              if(i!=1)
              {
                cleanData = rbind(cleanData,FinalfileData)
              }
            }
          }
          cleanData = sapply(cleanData, as.numeric)
          cleanData=data.frame(cleanData)
          #temp=(cleanData)[2]
          
          p1 <- ggplot(cleanData, aes((cleanData)[2], (cleanData)[3], group=temp1, color=temp1)) + geom_line()+xlab(lab_x)+ylab(lab_y)+ ggtitle(paste(title," Clean Signal (Session:",folder,")",sep="")) +annotate("text", label = paste("n=",numOfRows,sep=""), x = max(cleanData[2][!is.na(cleanData[2]),]) , y = max(cleanData[3][!is.na(cleanData[3]),]) , size = 3, colour = "red")
          sessionPlot[[length(sessionPlot)+1]] <- p1
          
        }
      } 
    }
  }
  return(sessionPlot)
}


dataPath="C:/Users/nafis/Desktop/data"

workingDir="C:/Users/nafis/Desktop/Project"
setwd(workingDir)
outputFile = read.xls(paste(workingDir,"/Dataset-Table-Index_output.xlsx",sep=""),header=TRUE, stringsAsFactors=FALSE)
#dataChannels=c("res")

setwd("output")
dataChannels=c("BR","HR","peda","pp","res")
sink("outputFinal.txt")

setwd(dataPath)


for(i in 1:nrow(outputFile))
{
  if(!substr(outputFile[i,1],0,1)=="T"){
    break
  }
  setwd(as.character(outputFile$Subject[i]))
  if(as.character(outputFile$Subject[i])==list.files())
    setwd(as.character(outputFile$Subject[i]))
  finalSession=list.files(pattern = paste(as.character(outputFile$Session[i])))
  if(length(finalSession)==0)
  {
    for(j in 1:length(dataChannels) )
    {
      if(!dataChannels[j]=="res")
      {
        if(!is.na(outputFile[dataChannels[j]][i,]))
        {
          outputFile[dataChannels[j]][i,]="0"
        }
      }
      else
      {
        if(!is.na(outputFile["performance..res."][i,]))
        {
          outputFile["performance..res."][i,]="0"
        }
      }
      
    }
  }
  else
  {
    setwd(as.character(finalSession))
    for(j in 1:length(dataChannels) )
    {
      if(!dataChannels[j]=="res")
      {
        if(!is.na(outputFile[dataChannels[j]][i,]))
        {
          fileFound = list.files(pattern = paste(as.character(dataChannels[j])))
          if(length(fileFound)==0)
            outputFile[dataChannels[j]][i,]="0"
          else{
            outputFile[dataChannels[j]][i,]=checkIfInRange(as.character(dataChannels[j]),fileFound )
          }
        }
      }
      else
      {
        if(!is.na(outputFile["performance..res."][i,]))
        {
          fileFound = list.files(pattern = paste(as.character(dataChannels[j])))
          if(length(fileFound)==0)
            outputFile["performance..res."][i,]="0"
          else
          {
            outputFile["performance..res."][i,]=checkIfInRange(as.character(dataChannels[j]),fileFound )
          }
        }
      }
    }
  }
  setwd(dataPath)
  
}

setwd(workingDir)
setwd("output")

sink()
write.csv(outputFile, file = "UpdatedIndex.csv")




metaDataSet=outputFile
#metaDataSet=metaDataSet=read.xls("C:/Users/nafis/Desktop/ANA Files/Statistical methods in Research/UpdatedIndex.xls",perl="C:/Strawberry/perl/bin/perl.exe", header=TRUE, stringsAsFactors=FALSE)
str=dataPath
#folderNames = unique(metaDataSet$Session)
folderNames = c("BL" , "CD" , "ED" , "MD",  "ND" , "PD" , "RD" , "FD")
setwd(dataPath)


for(chan in 1:length(dataChannels))
{
  #chan=4
  print(paste("chan: ",chan))
  if(!dataChannels[chan]=="res")
    pdf( paste(workingDir,"/output/OUTPUT2",dataChannels[chan],".pdf",sep=""),width = 10,height = 10)
  for(folder in folderNames)
  {
    #folder="MD"
    if(!is.na(folder) & !folder=="")
    {
      sessionPlot<- getGGPlot(chan,folder)
      if(!dataChannels[chan]=="res" & !dataChannels[chan]=="pp"){
        if(!length(sessionPlot)==0)
          do.call(grid.arrange, c(sessionPlot, list(ncol=2)))
      }
      else if(dataChannels[chan]=="pp"){
        if(!length(sessionPlot)==0)
          do.call(grid.arrange, c(sessionPlot, list(ncol=1)))
      }
      else{
        if(!length(sessionPlot)==0){
          png( paste(workingDir,"/output/OUTPUTGraphs_",dataChannels[chan],"_",folder,".png",sep=""),width = 20,height = 10, units="in", res=200)
          
          do.call(grid.arrange, c(sessionPlot, list(ncol=5, nrow=2)))
          dev.off()
        }
        
      }
    }
    
  }
   dev.off()
 
  
  
}




