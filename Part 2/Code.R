library(gdata)

### function to check Normality of data 

checkNormality <- function(data,printstmt){
  print(printstmt)
  qqnorm(data)
  qqline(data, col = 2)
  print(shapiro.test(data))
  ks.test(data, "pnorm", alternative="two.sided")
}

## the path where data is stored
dataPath="C:/Users/nafis/Desktop/data"

workingDir="C:/Users/nafis/Desktop/Project"
setwd(workingDir)
dataChannels=c("BR","HR","peda","pp")
#dataChannels=c("BR")

### read the updated index file - the values in updated file are 1 if the data is in range

metaDataSet=read.xls("C:/Users/nafis/Desktop/Project/output2/output/UpdatedIndex.xls",perl="C:/Strawberry/perl/bin/perl.exe", header=TRUE, stringsAsFactors=FALSE)
str=dataPath
folderNames = c("CD" ,  "ED" , "MD")
setwd(dataPath)

cnt=0
### for each data channel say HR
for(chan in 1:length(dataChannels))
{
  ### for each folder say CD
  
  for(folder in folderNames)
  {
    if(!is.na(folder) & !folder=="")
      {
        
        ### metaDataSetrelVal2 will contain all cells where values are ==1 , metaDataSetrelVal will contain the rows of the corresponding folder(subject) and Session(folder) eg CD
        if(!dataChannels[chan]=="res"){
           metaDataSetrelVal2=metaDataSet[is.na(metaDataSet[dataChannels[chan]])==FALSE & metaDataSet[dataChannels[chan]]==1 ,]
          metaDataSetrelVal=metaDataSetrelVal2[grep(as.character(folder),metaDataSetrelVal2$Session),]
          
        }else{
          metaDataSetrelVal2=metaDataSet[is.na(metaDataSet["performance..res."])==FALSE & metaDataSet["performance..res."]==1 ,]
          metaDataSetrelVal=metaDataSetrelVal2[grep(as.character(folder),metaDataSetrelVal2$Session),]
          
        }
        
        ### the number of rows for which the subject, folder combination was 1 -- number of times the iteration needs to be repeated
        numOfRows=length(unique(metaDataSetrelVal$Subject))
        if(!nrow(metaDataSetrelVal)==0)
        {
          ### construct the path of the folder containing the file -- in drawPath variable
          drawPath=cbind(rep(str,each=nrow(metaDataSetrelVal)),'/',metaDataSetrelVal["Subject"],'/')
          colnames(drawPath) <- c("ParentDir","FstSlash","Subject","SecSlash")
          drawPath=paste(drawPath$ParentDir,drawPath$FstSlash,drawPath$Subject,drawPath$SecSlash,sep = "")
          
          ### for each path of the file
          for(i in 1:length(drawPath)) 
          {
            #i=1
            print(drawPath[i])
            setwd(drawPath[i])
            tempArr = strsplit(drawPath[i], "/")
            folderT = lapply(tempArr, tail, 1) 
            
            ### if the value for that particular subject and ND combination exists in metaDataSetrelVal2 -- which contains only values=1
            if(nrow(metaDataSetrelVal2[metaDataSetrelVal2$Subject==folderT & metaDataSetrelVal2$Session=="ND",])>=1){
              cnt=cnt+1
              print("in if")
              if(folderT == list.files())
                setwd(as.character(folderT))
              
              ### find the final folder containing the file CD
              FinalFolder=list.files(pattern = paste(as.character(folder)))
              setwd(as.character(FinalFolder))
              
              ### find final file name
              FinalfileName=list.files(pattern = paste(dataChannels[chan]))
              
              ### find file with .stm extension
              FinalfileNameStm = list.files(pattern = paste(".stm"))
              
              print(FinalfileName)
              print(FinalfileNameStm)
              
              
              ### if there exists a .stm file in the folder find the start and end values of 2 time slices
              if(!length(FinalfileNameStm)==0)
              {
                FinalfileDataStm=read.xls(FinalfileNameStm, stringsAsFactors=FALSE)
                st1=FinalfileDataStm$X[9]
                st1=sapply(st1, as.numeric)
                en1=FinalfileDataStm$X.1[9]
                en1=sapply(en1, as.numeric)
                
                st2=FinalfileDataStm$X[10]
                st2=sapply(st2, as.numeric)
                en2=FinalfileDataStm$X.1[10]
                en2=sapply(en2, as.numeric)
              }
              
              
              if(!length(FinalfileName)==0)
              {
                ### read the final file using appropriate format -- the res files get updated when part 1 of prog is run, removing the out-of-range values and are stored as .cssv else all files are in .xls format
                if(!dataChannels[chan]=="res")
                  FinalfileData=read.xls(FinalfileName, stringsAsFactors=FALSE)
                else
                  FinalfileData=read.csv(FinalfileName, stringsAsFactors=FALSE)
                
                ### reset datapath to fetch ND files
                setwd(dataPath)
                setwd(drawPath[i])
                
                ### find final folder for ND files
                FinalFolder2=list.files(pattern = paste(as.character("ND")))
                setwd(as.character(FinalFolder2))
                
                
                ### find final filename for ND file and read in FinalfileDataND
                FinalfileNameND=list.files(pattern = paste(dataChannels[chan]))
                
                if(!dataChannels[chan]=="res")
                  FinalfileDataND=read.xls(FinalfileNameND, stringsAsFactors=FALSE)
                else
                  FinalfileDataND=read.csv(FinalfileNameND, stringsAsFactors=FALSE)
                
                
                ### convert both files of the particular session ( folder ) eg CD and of session ND to numeric format
                FinalfileData = sapply(FinalfileData, as.numeric)
                FinalfileData = data.frame(FinalfileData)
                FinalfileDataND = sapply(FinalfileDataND, as.numeric)
                FinalfileDataND = data.frame(FinalfileDataND)
                FinalfileDataND=FinalfileDataND[colSums(!is.na(FinalfileDataND)) > 0]
                FinalfileData=FinalfileData[colSums(!is.na(FinalfileData)) > 0]
                
                ### this block eliminates the issue caused by multiple columns in pp file
                if(as.character(dataChannels[chan])=="pp"){
                  FinalfileData=FinalfileData[, 1:4]
                  colnames(FinalfileData) <- c("Perinasal.Perspiration","X","X.1","X.2")
                  FinalfileDataND=FinalfileDataND[, 1:4]
                  colnames(FinalfileDataND) <- c("Perinasal.Perspiration","X","X.1","X.2")
                  
                  
                }
                
                ### omit NA files in both files (CD and ND)
                FinalfileData=data.frame(na.omit(FinalfileData))
                FinalfileDataND=data.frame(na.omit(FinalfileDataND))
                
                ### divide the file data into phases based on time slice values
                FinalfileDataPH1= FinalfileData[FinalfileData$X<st1,]
                FinalfileDataPH2= FinalfileData[FinalfileData$X>=st1 & FinalfileData$X<en1 ,]
                FinalfileDataPH3= FinalfileData[FinalfileData$X>=en1 & FinalfileData$X<st2 ,]
                FinalfileDataPH4= FinalfileData[FinalfileData$X>=st2 & FinalfileData$X<en2 ,]
                FinalfileDataPH5= FinalfileData[FinalfileData$X>=en2,]
                
                
                
                ### divide the ND file data into phases based on time slice values
                FinalfileDataNDPH1= FinalfileDataND[FinalfileDataND$X<st1,]
                FinalfileDataNDPH2= FinalfileDataND[FinalfileDataND$X>=st1 & FinalfileDataND$X<en1 ,]
                FinalfileDataNDPH3= FinalfileDataND[FinalfileDataND$X>=en1 & FinalfileDataND$X<st2 ,]
                FinalfileDataNDPH4= FinalfileDataND[FinalfileDataND$X>=st2 & FinalfileDataND$X<en2 ,]
                FinalfileDataNDPH5= FinalfileDataND[FinalfileDataND$X>=en2,]
                
                ### eliminate the channel if the data in any phase has 0 rows
                if(nrow(FinalfileDataPH1)==0 || nrow(FinalfileDataPH2)==0 || nrow(FinalfileDataPH3)==0 || nrow(FinalfileDataPH4)==0 || nrow(FinalfileDataPH5)==0 || nrow(FinalfileDataNDPH1)==0 || nrow(FinalfileDataNDPH2)==0 || nrow(FinalfileDataNDPH3)==0 || nrow(FinalfileDataNDPH4)==0  || nrow(FinalfileDataNDPH5)==0   )
                  break
        
                ### assign or append the means to vectors combinedDataPh1, combinedDataPh2, etc to denote means of file data and combinedDataNDPh1, combinedDataNDPh2, etc to denote means of ND file data
                if(i==1)
                {
                  combinedDataPh1=mean(sapply(FinalfileDataPH1$X.1, as.numeric))
                  combinedDataPh2=mean(sapply(FinalfileDataPH2$X.1, as.numeric))
                  combinedDataPh3=mean(sapply(FinalfileDataPH3$X.1, as.numeric))
                  combinedDataPh4=mean(sapply(FinalfileDataPH4$X.1, as.numeric))
                  combinedDataPh5=mean(sapply(FinalfileDataPH5$X.1, as.numeric))
                  
                  combinedDataNDPh1=mean(sapply(FinalfileDataNDPH1$X.1, as.numeric))
                  combinedDataNDPh2=mean(sapply(FinalfileDataNDPH2$X.1, as.numeric))
                  combinedDataNDPh3=mean(sapply(FinalfileDataNDPH3$X.1, as.numeric))
                  combinedDataNDPh4=mean(sapply(FinalfileDataNDPH4$X.1, as.numeric))
                  combinedDataNDPh5=mean(sapply(FinalfileDataNDPH5$X.1, as.numeric))
                  
                }
                if(i!=1)
                {
                  combinedDataPh1 = rbind(combinedDataPh1,mean(sapply(FinalfileDataPH1$X.1, as.numeric)))
                  combinedDataPh2 = rbind(combinedDataPh2,mean(sapply(FinalfileDataPH2$X.1, as.numeric)))
                  combinedDataPh3 = rbind(combinedDataPh3,mean(sapply(FinalfileDataPH3$X.1, as.numeric)))
                  combinedDataPh4 = rbind(combinedDataPh4,mean(sapply(FinalfileDataPH4$X.1, as.numeric)))
                  combinedDataPh5 = rbind(combinedDataPh5,mean(sapply(FinalfileDataPH5$X.1, as.numeric)))
                  
                  
                  combinedDataNDPh1 = rbind(combinedDataNDPh1,mean(sapply(FinalfileDataNDPH1$X.1, as.numeric)))
                  combinedDataNDPh2 = rbind(combinedDataNDPh2,mean(sapply(FinalfileDataNDPH2$X.1, as.numeric)))
                  combinedDataNDPh3 = rbind(combinedDataNDPh3,mean(sapply(FinalfileDataNDPH3$X.1, as.numeric)))
                  combinedDataNDPh4 = rbind(combinedDataNDPh4,mean(sapply(FinalfileDataNDPH4$X.1, as.numeric)))
                  combinedDataNDPh5 = rbind(combinedDataNDPh5,mean(sapply(FinalfileDataNDPH5$X.1, as.numeric)))
                  
                }
              }
            }
              
            
            
          }
          
          
          
          ### convert all values to numerica data frame
          #combinedData = sapply(combinedData, as.numeric)
          #combinedData = data.frame(combinedData)
          combinedDataPh1 = sapply(combinedDataPh1, as.numeric)
          combinedDataPh1 = data.frame(combinedDataPh1)
          combinedDataPh2 = sapply(combinedDataPh2, as.numeric)
          combinedDataPh2 = data.frame(combinedDataPh2)
          combinedDataPh3 = sapply(combinedDataPh3, as.numeric)
          combinedDataPh3 = data.frame(combinedDataPh3)
          combinedDataPh4 = sapply(combinedDataPh4, as.numeric)
          combinedDataPh4 = data.frame(combinedDataPh4)
          combinedDataPh5 = sapply(combinedDataPh5, as.numeric)
          combinedDataPh5 = data.frame(combinedDataPh5)
          
          #combinedDataND = sapply(combinedDataND, as.numeric)
          #combinedDataND = data.frame(combinedDataND)
          combinedDataNDPh1 = sapply(combinedDataNDPh1, as.numeric)
          combinedDataNDPh1 = data.frame(combinedDataNDPh1)
          combinedDataNDPh2 = sapply(combinedDataNDPh2, as.numeric)
          combinedDataNDPh2 = data.frame(combinedDataNDPh2)
          combinedDataNDPh3 = sapply(combinedDataNDPh3, as.numeric)
          combinedDataNDPh3 = data.frame(combinedDataNDPh3)
          combinedDataNDPh4 = sapply(combinedDataNDPh4, as.numeric)
          combinedDataNDPh4 = data.frame(combinedDataNDPh4)
          combinedDataNDPh5 = sapply(combinedDataNDPh5, as.numeric)
          combinedDataNDPh5 = data.frame(combinedDataNDPh5)
          
          
        }
        
      }
    
    ### assign the mean values to appropriate vectors 
    
    ### naming convention used : <datachannel eg HR>.<Session eg CD>.<Phase eg Ph1>
    ### and for vectors containing difference of the session from ND (eg CD-ND)
    ### <datachannel eg HR>.<Session eg CD>.ND.<Phase eg Ph1>
    
    if(as.character(dataChannels[chan])=="HR"){
      HR.ND.Ph1=combinedDataNDPh1
      HR.ND.Ph2=combinedDataNDPh2
      HR.ND.Ph3=combinedDataNDPh3
      HR.ND.Ph4=combinedDataNDPh4
      HR.ND.Ph5=combinedDataNDPh5
      
      
      
      if(folder=="CD"){
        HR.CD.Ph1=combinedDataPh1
        HR.CD.Ph2=combinedDataPh2
        HR.CD.Ph3=combinedDataPh3
        HR.CD.Ph4=combinedDataPh4
        HR.CD.Ph5=combinedDataPh5
        
        
        HR.CD.ND.Ph1=HR.CD.Ph1-HR.ND.Ph1
        HR.CD.ND.Ph2=HR.CD.Ph2-HR.ND.Ph2
        HR.CD.ND.Ph3=HR.CD.Ph3-HR.ND.Ph3
        HR.CD.ND.Ph4=HR.CD.Ph4-HR.ND.Ph4
        HR.CD.ND.Ph5=HR.CD.Ph5-HR.ND.Ph5
        
        
        HR.CD=cbind(HR.CD.ND.Ph1,"Phase 1")
        colnames(HR.CD) <- c("Rate","Phase" )
        temp=cbind(HR.CD.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        HR.CD=rbind(HR.CD,temp)
        temp=cbind(HR.CD.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        HR.CD=rbind(HR.CD,temp)
        temp=cbind(HR.CD.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        HR.CD=rbind(HR.CD,temp)
        temp=cbind(HR.CD.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        HR.CD=rbind(HR.CD,temp)
        
      }
      
      
      
      if(folder=="ED"){
        HR.ED.Ph1=combinedDataPh1
        HR.ED.Ph2=combinedDataPh2
        HR.ED.Ph3=combinedDataPh3
        HR.ED.Ph4=combinedDataPh4
        HR.ED.Ph5=combinedDataPh5
        
        
        HR.ED.ND.Ph1=HR.ED.Ph1-HR.ND.Ph1
        HR.ED.ND.Ph2=HR.ED.Ph2-HR.ND.Ph2
        HR.ED.ND.Ph3=HR.ED.Ph3-HR.ND.Ph3
        HR.ED.ND.Ph4=HR.ED.Ph4-HR.ND.Ph4
        HR.ED.ND.Ph5=HR.ED.Ph5-HR.ND.Ph5
        
        HR.ED=cbind(HR.ED.ND.Ph1,"Phase 1")
        colnames(HR.ED) <- c("Rate","Phase" )
        temp=cbind(HR.ED.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        HR.ED=rbind(HR.ED,temp)
        temp=cbind(HR.ED.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        HR.ED=rbind(HR.ED,temp)
        temp=cbind(HR.ED.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        HR.ED=rbind(HR.ED,temp)
        temp=cbind(HR.ED.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        HR.ED=rbind(HR.ED,temp)
        
        
      }
      
      if(folder=="MD"){
        HR.MD.Ph1=combinedDataPh1
        HR.MD.Ph2=combinedDataPh2
        HR.MD.Ph3=combinedDataPh3
        HR.MD.Ph4=combinedDataPh4
        HR.MD.Ph5=combinedDataPh5
        
        HR.MD.ND.Ph1=HR.MD.Ph1-HR.ND.Ph1
        HR.MD.ND.Ph2=HR.MD.Ph2-HR.ND.Ph2
        HR.MD.ND.Ph3=HR.MD.Ph3-HR.ND.Ph3
        HR.MD.ND.Ph4=HR.MD.Ph4-HR.ND.Ph4
        HR.MD.ND.Ph5=HR.MD.Ph5-HR.ND.Ph5
        
        HR.MD=cbind(HR.MD.ND.Ph1,"Phase 1")
        colnames(HR.MD) <- c("Rate","Phase" )
        temp=cbind(HR.MD.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        HR.MD=rbind(HR.MD,temp)
        temp=cbind(HR.MD.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        HR.MD=rbind(HR.MD,temp)
        temp=cbind(HR.MD.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        HR.MD=rbind(HR.MD,temp)
        temp=cbind(HR.MD.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        HR.MD=rbind(HR.MD,temp)
        
      }
      
      
      
    }
    
    if(as.character(dataChannels[chan])=="BR"){
     BR.ND.Ph1=combinedDataNDPh1
      BR.ND.Ph2=combinedDataNDPh2
      BR.ND.Ph3=combinedDataNDPh3
      BR.ND.Ph4=combinedDataNDPh4
      BR.ND.Ph5=combinedDataNDPh5
      
     
      
      if(folder=="CD"){
        BR.CD.Ph1=combinedDataPh1
        BR.CD.Ph2=combinedDataPh2
        BR.CD.Ph3=combinedDataPh3
        BR.CD.Ph4=combinedDataPh4
        BR.CD.Ph5=combinedDataPh5
        
        BR.CD.ND.Ph1=BR.CD.Ph1-BR.ND.Ph1
        BR.CD.ND.Ph2=BR.CD.Ph2-BR.ND.Ph2
        BR.CD.ND.Ph3=BR.CD.Ph3-BR.ND.Ph3
        BR.CD.ND.Ph4=BR.CD.Ph4-BR.ND.Ph4
        BR.CD.ND.Ph5=BR.CD.Ph5-BR.ND.Ph5
        
        BR.CD=cbind(BR.CD.ND.Ph1,"Phase 1")
        colnames(BR.CD) <- c("Rate","Phase" )
        temp=cbind(BR.CD.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        BR.CD=rbind(BR.CD,temp)
        temp=cbind(BR.CD.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        BR.CD=rbind(BR.CD,temp)
        temp=cbind(BR.CD.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        BR.CD=rbind(BR.CD,temp)
        temp=cbind(BR.CD.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        BR.CD=rbind(BR.CD,temp)
        
      }
      
      
      
      if(folder=="ED"){
        BR.ED.Ph1=combinedDataPh1
        BR.ED.Ph2=combinedDataPh2
        BR.ED.Ph3=combinedDataPh3
        BR.ED.Ph4=combinedDataPh4
        BR.ED.Ph5=combinedDataPh5
        
        BR.ED.ND.Ph1=BR.ED.Ph1-BR.ND.Ph1
        BR.ED.ND.Ph2=BR.ED.Ph2-BR.ND.Ph2
        BR.ED.ND.Ph3=BR.ED.Ph3-BR.ND.Ph3
        BR.ED.ND.Ph4=BR.ED.Ph4-BR.ND.Ph4
        BR.ED.ND.Ph5=BR.ED.Ph5-BR.ND.Ph5
        
        BR.ED=cbind(BR.ED.ND.Ph1,"Phase 1")
        colnames(BR.CD) <- c("Rate","Phase" )
        temp=cbind(BR.ED.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        colnames(BR.ED) <- c("Rate","Phase" )
        BR.ED=rbind(BR.ED,temp)
        temp=cbind(BR.ED.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        BR.ED=rbind(BR.ED,temp)
        temp=cbind(BR.ED.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        BR.ED=rbind(BR.ED,temp)
        temp=cbind(BR.ED.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        BR.ED=rbind(BR.ED,temp)
        
      }
      
      if(folder=="MD"){
        BR.MD.Ph1=combinedDataPh1
        BR.MD.Ph2=combinedDataPh2
        BR.MD.Ph3=combinedDataPh3
        BR.MD.Ph4=combinedDataPh4
        BR.MD.Ph5=combinedDataPh5
        
        BR.MD.ND.Ph1=BR.MD.Ph1-BR.ND.Ph1
        BR.MD.ND.Ph2=BR.MD.Ph2-BR.ND.Ph2
        BR.MD.ND.Ph3=BR.MD.Ph3-BR.ND.Ph3
        BR.MD.ND.Ph4=BR.MD.Ph4-BR.ND.Ph4
        BR.MD.ND.Ph5=BR.MD.Ph5-BR.ND.Ph5
        
        BR.MD=cbind(BR.MD.ND.Ph1,"Phase 1")
        colnames(BR.MD) <- c("Rate","Phase" )
        temp=cbind(BR.MD.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        BR.MD=rbind(BR.MD,temp)
        temp=cbind(BR.MD.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        BR.MD=rbind(BR.MD,temp)
        temp=cbind(BR.MD.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        BR.MD=rbind(BR.MD,temp)
        temp=cbind(BR.MD.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        BR.MD=rbind(BR.MD,temp)
        
      }
      
         
    }
    
    if(as.character(dataChannels[chan])=="peda"){
       peda.ND.Ph1=combinedDataNDPh1
      peda.ND.Ph2=combinedDataNDPh2
      peda.ND.Ph3=combinedDataNDPh3
      peda.ND.Ph4=combinedDataNDPh4
      peda.ND.Ph5=combinedDataNDPh5
      
      
      
      if(folder=="CD"){
        peda.CD.Ph1=combinedDataPh1
        peda.CD.Ph2=combinedDataPh2
        peda.CD.Ph3=combinedDataPh3
        peda.CD.Ph4=combinedDataPh4
        peda.CD.Ph5=combinedDataPh5
        
        
        peda.CD.ND.Ph1=peda.CD.Ph1-peda.ND.Ph1
        peda.CD.ND.Ph2=peda.CD.Ph2-peda.ND.Ph2
        peda.CD.ND.Ph3=peda.CD.Ph3-peda.ND.Ph3
        peda.CD.ND.Ph4=peda.CD.Ph4-peda.ND.Ph4
        peda.CD.ND.Ph5=peda.CD.Ph5-peda.ND.Ph5
        
        peda.CD=cbind(peda.CD.ND.Ph1,"Phase 1")
        colnames(peda.CD) <- c("Rate","Phase" )
        temp=cbind(peda.CD.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        peda.CD=rbind(peda.CD,temp)
        temp=cbind(peda.CD.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        peda.CD=rbind(peda.CD,temp)
        temp=cbind(peda.CD.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        peda.CD=rbind(peda.CD,temp)
        temp=cbind(peda.CD.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        peda.CD=rbind(peda.CD,temp)
      }
      
      
      
      if(folder=="ED"){
        peda.ED.Ph1=combinedDataPh1
        peda.ED.Ph2=combinedDataPh2
        peda.ED.Ph3=combinedDataPh3
        peda.ED.Ph4=combinedDataPh4
        peda.ED.Ph5=combinedDataPh5
        
        
        peda.ED.ND.Ph1=peda.ED.Ph1-peda.ND.Ph1
        peda.ED.ND.Ph2=peda.ED.Ph2-peda.ND.Ph2
        peda.ED.ND.Ph3=peda.ED.Ph3-peda.ND.Ph3
        peda.ED.ND.Ph4=peda.ED.Ph4-peda.ND.Ph4
        peda.ED.ND.Ph5=peda.ED.Ph5-peda.ND.Ph5
        
        peda.ED=cbind(peda.ED.ND.Ph1,"Phase 1")
        colnames(peda.ED) <- c("Rate","Phase" )
        temp=cbind(peda.ED.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        peda.ED=rbind(peda.ED,temp)
        temp=cbind(peda.ED.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        peda.ED=rbind(peda.ED,temp)
        temp=cbind(peda.ED.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        peda.ED=rbind(peda.ED,temp)
        temp=cbind(peda.ED.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        peda.ED=rbind(peda.ED,temp)
        
        
      }
      
      if(folder=="MD"){
        peda.MD.Ph1=combinedDataPh1
        peda.MD.Ph2=combinedDataPh2
        peda.MD.Ph3=combinedDataPh3
        peda.MD.Ph4=combinedDataPh4
        peda.MD.Ph5=combinedDataPh5
        
        peda.MD.ND.Ph1=peda.MD.Ph1-peda.ND.Ph1
        peda.MD.ND.Ph2=peda.MD.Ph2-peda.ND.Ph2
        peda.MD.ND.Ph3=peda.MD.Ph3-peda.ND.Ph3
        peda.MD.ND.Ph4=peda.MD.Ph4-peda.ND.Ph4
        peda.MD.ND.Ph5=peda.MD.Ph5-peda.ND.Ph5
        
        peda.MD=cbind(peda.MD.ND.Ph1,"Phase 1")
        colnames(peda.MD) <- c("Rate","Phase" )
        temp=cbind(peda.MD.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        peda.MD=rbind(peda.MD,temp)
        temp=cbind(peda.MD.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        peda.MD=rbind(peda.MD,temp)
        temp=cbind(peda.MD.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        peda.MD=rbind(peda.MD,temp)
        temp=cbind(peda.MD.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        peda.MD=rbind(peda.MD,temp)
        
      }
      
      
    }
    
    if(as.character(dataChannels[chan])=="pp"){
      pp.ND.Ph1=combinedDataNDPh1
      pp.ND.Ph2=combinedDataNDPh2
      pp.ND.Ph3=combinedDataNDPh3
      pp.ND.Ph4=combinedDataNDPh4
      pp.ND.Ph5=combinedDataNDPh5
      
      
      
      if(folder=="CD"){
        pp.CD.Ph1=combinedDataPh1
        pp.CD.Ph2=combinedDataPh2
        pp.CD.Ph3=combinedDataPh3
        pp.CD.Ph4=combinedDataPh4
        pp.CD.Ph5=combinedDataPh5
        
        pp.CD.ND.Ph1=pp.CD.Ph1-pp.ND.Ph1
        pp.CD.ND.Ph2=pp.CD.Ph2-pp.ND.Ph2
        pp.CD.ND.Ph3=pp.CD.Ph3-pp.ND.Ph3
        pp.CD.ND.Ph4=pp.CD.Ph4-pp.ND.Ph4
        pp.CD.ND.Ph5=pp.CD.Ph5-pp.ND.Ph5
        
        pp.CD=cbind(pp.CD.ND.Ph1,"Phase 1")
        colnames(pp.CD) <- c("Rate","Phase" )
        temp=cbind(pp.CD.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        pp.CD=rbind(pp.CD,temp)
        temp=cbind(pp.CD.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        pp.CD=rbind(pp.CD,temp)
        temp=cbind(pp.CD.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        pp.CD=rbind(pp.CD,temp)
        temp=cbind(pp.CD.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        pp.CD=rbind(pp.CD,temp)
        
        
      }
      
      
      
      if(folder=="ED"){
         pp.ED.Ph1=combinedDataPh1
        pp.ED.Ph2=combinedDataPh2
        pp.ED.Ph3=combinedDataPh3
        pp.ED.Ph4=combinedDataPh4
        pp.ED.Ph5=combinedDataPh5
        
        pp.ED.ND.Ph1=pp.ED.Ph1-pp.ND.Ph1
        pp.ED.ND.Ph2=pp.ED.Ph2-pp.ND.Ph2
        pp.ED.ND.Ph3=pp.ED.Ph3-pp.ND.Ph3
        pp.ED.ND.Ph4=pp.ED.Ph4-pp.ND.Ph4
        pp.ED.ND.Ph5=pp.ED.Ph5-pp.ND.Ph5
        
        pp.ED=cbind(pp.ED.ND.Ph1,"Phase 1")
        colnames(pp.ED) <- c("Rate","Phase" )
        temp=cbind(pp.ED.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        pp.ED=rbind(pp.ED,temp)
        temp=cbind(pp.ED.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        pp.ED=rbind(pp.ED,temp)
        temp=cbind(pp.ED.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        pp.ED=rbind(pp.ED,temp)
        temp=cbind(pp.ED.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        pp.ED=rbind(pp.ED,temp)
        
        
      }
      
      if(folder=="MD"){
        pp.MD.Ph1=combinedDataPh1
        pp.MD.Ph2=combinedDataPh2
        pp.MD.Ph3=combinedDataPh3
        pp.MD.Ph4=combinedDataPh4
        pp.MD.Ph5=combinedDataPh5
        
        pp.MD.ND.Ph1=pp.MD.Ph1-pp.ND.Ph1
        pp.MD.ND.Ph2=pp.MD.Ph2-pp.ND.Ph2
        pp.MD.ND.Ph3=pp.MD.Ph3-pp.ND.Ph3
        pp.MD.ND.Ph4=pp.MD.Ph4-pp.ND.Ph4
        pp.MD.ND.Ph5=pp.MD.Ph5-pp.ND.Ph5
        
        pp.MD=cbind(pp.MD.ND.Ph1,"Phase 1")
        colnames(pp.MD) <- c("Rate","Phase" )
        temp=cbind(pp.MD.ND.Ph2,"Phase 2")
        colnames(temp) <- c("Rate","Phase" )
        pp.MD=rbind(pp.MD,temp)
        temp=cbind(pp.MD.ND.Ph3,"Phase 3")
        colnames(temp) <- c("Rate","Phase" )
        pp.MD=rbind(pp.MD,temp)
        temp=cbind(pp.MD.ND.Ph4,"Phase 4")
        colnames(temp) <- c("Rate","Phase" )
        pp.MD=rbind(pp.MD,temp)
        temp=cbind(pp.MD.ND.Ph5,"Phase 5")
        colnames(temp) <- c("Rate","Phase" )
        pp.MD=rbind(pp.MD,temp)
      }
      
      
    }
    
    
  }

}

# Test for BR-CD-Phase1
#Normality tests
checkNormality(BR.CD.ND.Ph1[,1],'Normality Test for BR-CD-Phase1')

t.test(BR.CD.ND.Ph1[,1])

# Test for BR-CD-Phase2
#Normality tests

checkNormality(BR.CD.ND.Ph2[,1],'Normality Test for BR-CD-Phase2')

t.test(BR.CD.ND.Ph2[,1])



# Test for BR-CD-Phase3
#Normality tests
checkNormality(BR.CD.ND.Ph3[,1],'Normality Test for BR-CD-Phase3')

t.test(BR.CD.ND.Ph3[,1])




# Test for BR-CD-Phase4
#Normality tests
checkNormality(BR.CD.ND.Ph4[,1],'Normality Test for BR-CD-Phase4')
t.test(BR.CD.ND.Ph4[,1])


# Test for BR-CD-Phase5
#Normality tests
checkNormality(BR.CD.ND.Ph5[,1],'Normality Test for BR-CD-Phase5')

t.test(BR.CD.ND.Ph5[,1])


# Test for BR-ED-Phase1
#Normality tests
checkNormality(BR.ED.ND.Ph1[,1],'Normality Test for BR-ED-Phase1')
t.test(BR.ED.ND.Ph1[,1])
# Test for BR-ED-Phase2
#Normality tests
checkNormality(BR.ED.ND.Ph2[,1],'Normality Test for BR-ED-Phase2')
t.test(BR.ED.ND.Ph2[,1])

# Test for BR-ED-Phase3
#Normality tests
checkNormality(BR.ED.ND.Ph3[,1],'Normality Test for BR-ED-Phase3')
t.test(BR.ED.ND.Ph3[,1])


# Test for BR-ED-Phase4
#Normality tests
checkNormality(BR.ED.ND.Ph4[,1],'Normality Test for BR-ED-Phase4')

t.test(BR.ED.ND.Ph4[,1])
# Test for BR-ED-Phase5
#Normality tests
checkNormality(BR.ED.ND.Ph5[,1],'Normality Test for BR-ED-Phase5')
t.test(BR.ED.ND.Ph5[,1])
# Test for BR-MD-Phase1
#Normality tests
checkNormality(BR.MD.ND.Ph1[,1],'Normality Test for BR-MD-Phase1')
t.test(BR.MD.ND.Ph1[,1])

# Test for BR-MD-Phase2
#Normality tests

checkNormality(BR.MD.ND.Ph2[,1],'Normality Test for BR-MD-Phase2')

t.test(BR.MD.ND.Ph2[,1])
# Test for BR-MD-Phase3
#Normality tests

checkNormality(BR.MD.ND.Ph3[,1],'Normality Test for BR-MD-Phase3')
t.test(BR.MD.ND.Ph3[,1])
# Test for BR-MD-Phase4
#Normality tests
checkNormality(BR.MD.ND.Ph4[,1],'Normality Test for BR-MD-Phase4')
t.test(BR.MD.ND.Ph4[,1])


# Test for BR-MD-Phase5
#Normality tests
checkNormality(BR.MD.ND.Ph5[,1],'Normality Test for BR-MD-Phase5')

t.test(BR.MD.ND.Ph5[,1])


# Test for HR-CD-Phase1
checkNormality(HR.CD.ND.Ph1[,1],'Normality Test for HR-CD-Phase1')
t.test(HR.CD.ND.Ph1[,1])

# Test for HR-CD-Phase2
checkNormality(HR.CD.ND.Ph2[,1],'Normality Test for HR-CD-Phase2')
t.test(HR.CD.ND.Ph2[,1])



# Test for HR-CD-Phase3
checkNormality(HR.CD.ND.Ph3[,1],'Normality Test for HR-CD-Phase3')
t.test(HR.CD.ND.Ph3[,1])

# Test for HR-CD-Phase4
checkNormality(HR.CD.ND.Ph4[,1],'Normality Test for HR-CD-Phase4')
t.test(HR.CD.ND.Ph4[,1])
# Test for HR-CD-Phase5
checkNormality(HR.CD.ND.Ph5[,1],'Normality Test for HR-CD-Phase5')
t.test(HR.CD.ND.Ph5[,1])

# Test for HR-ED-Phase1
checkNormality(HR.ED.ND.Ph1[,1],'Normality Test for HR-ED-Phase1')

t.test(HR.ED.ND.Ph1[,1])

# Test for HR-ED-Phase2
checkNormality(HR.ED.ND.Ph2[,1],'Normality Test for HR-ED-Phase2')
t.test(HR.ED.ND.Ph2[,1])

# Test for HR-ED-Phase3
checkNormality(HR.ED.ND.Ph3[,1],'Normality Test for HR-ED-Phase3')
t.test(HR.ED.ND.Ph3[,1])

# Test for HR-ED-Phase4
checkNormality(HR.ED.ND.Ph4[,1],'Normality Test for HR-ED-Phase4')
t.test(HR.ED.ND.Ph4[,1])
# Test for HR-ED-Phase5
checkNormality(HR.ED.ND.Ph5[,1],'Normality Test for HR-ED-Phase5')
t.test(HR.ED.ND.Ph5[,1])



# Test for HR-MD-Phase1
checkNormality(HR.MD.ND.Ph1[,1],'Normality Test for HR-MD-Phase1')
t.test(HR.MD.ND.Ph1[,1])

# Test for HR-MD-Phase2
checkNormality(HR.MD.ND.Ph2[,1],'Normality Test for HR-MD-Phase2')
t.test(HR.MD.ND.Ph2[,1])

# Test for HR-MD-Phase3
checkNormality(HR.MD.ND.Ph3[,1],'Normality Test for HR-MD-Phase3')
t.test(HR.MD.ND.Ph3[,1])

# Test for HR-MD-Phase4
checkNormality(HR.MD.ND.Ph4[,1],'Normality Test for HR-MD-Phase4')
t.test(HR.MD.ND.Ph4[,1])


# Test for HR-MD-Phase5
checkNormality(HR.MD.ND.Ph5[,1],'Normality Test for HR-MD-Phase5')
t.test(HR.MD.ND.Ph5[,1])


### Code to test Normality of Peda and Apply T-Test

checkNormality(peda.CD.ND.Ph1[,1],'Normality Test for peda-CD-Phase1')
t.test(peda.CD.ND.Ph1[,1])

checkNormality(peda.CD.ND.Ph2[,1],'Normality Test for peda-CD-Phase2')
t.test(peda.CD.ND.Ph2[,1])

checkNormality(peda.CD.ND.Ph3[,1],'Normality Test for peda-CD-Phase3')
peda.CD.ND.Ph3.pos=peda.CD.ND.Ph3[,1]+1-min(peda.CD.ND.Ph3[,1])
checkNormality(log(peda.CD.ND.Ph3.pos),'Normality test after log for peda-CD-Phase3')
t.test(log(peda.CD.ND.Ph3.pos),mu=log(-min(peda.CD.ND.Ph3[,1])+1))


checkNormality(peda.CD.ND.Ph4[,1],'Normality Test for peda-CD-Phase4')
peda.CD.ND.Ph4.pos=peda.CD.ND.Ph4[,1]+1-min(peda.CD.ND.Ph4[,1])
hist(log(peda.CD.ND.Ph4.pos))
checkNormality(log(peda.CD.ND.Ph4.pos),'Normality test after log for peda-CD-Phase4')
t.test(log(peda.CD.ND.Ph4.pos),mu=log(-min(peda.CD.ND.Ph4[,1])+1))

checkNormality(peda.CD.ND.Ph5[,1],'Normality Test for peda-CD-Phase5')
peda.CD.ND.Ph5.pos=peda.CD.ND.Ph5[,1]+1-min(peda.CD.ND.Ph5[,1])
checkNormality(1/sqrt(peda.CD.ND.Ph5.pos),'Normality test after Inverse of square root of positive numbers of peda-CD-Phase5')
t.test(1/sqrt(peda.CD.ND.Ph5.pos),mu=1/sqrt(-min(peda.CD.ND.Ph5[,1])+1))


checkNormality(peda.ED.ND.Ph1[,1],'Normality Test for peda-ED-Phase1')
peda.ED.ND.Ph1.pos=peda.ED.ND.Ph1[,1]+1-min(peda.ED.ND.Ph1[,1])
checkNormality(log(peda.ED.ND.Ph1.pos),'Normality Test after log for for peda-ED-Phase1')
t.test(log(peda.ED.ND.Ph1.pos),mu=log(-min(peda.ED.ND.Ph1[,1])+1))


checkNormality(peda.ED.ND.Ph2[,1],'Normality Test for peda-ED-Phase2')
peda.ED.ND.Ph2.pos=peda.ED.ND.Ph2[,1]+1-min(peda.ED.ND.Ph2[,1])
checkNormality(log(peda.ED.ND.Ph2.pos),'Normality Test after log for for peda-ED-Phase2')
t.test(log(peda.ED.ND.Ph2.pos),mu=log(-min(peda.ED.ND.Ph2[,1])+1))


checkNormality(peda.ED.ND.Ph3[,1],'Normality Test for peda-ED-Phase3')
peda.ED.ND.Ph3.pos=peda.ED.ND.Ph3[,1]+1-min(peda.ED.ND.Ph3[,1])
checkNormality(log(peda.ED.ND.Ph3.pos),'Normality Test after log for for peda-ED-Phase3')
t.test(log(peda.ED.ND.Ph3.pos),mu=log(-min(peda.ED.ND.Ph3[,1])+1))



checkNormality(peda.ED.ND.Ph4[,1],'Normality Test for peda-ED-Phase4')
peda.ED.ND.Ph4.pos=peda.ED.ND.Ph4[,1]+1-min(peda.ED.ND.Ph4[,1])
checkNormality(log(peda.ED.ND.Ph4.pos),'Normality Test after log for for peda-ED-Phase4')
t.test(log(peda.ED.ND.Ph4.pos),mu=log(-min(peda.ED.ND.Ph4[,1])+1))


checkNormality(peda.ED.ND.Ph5[,1],'Normality Test for peda-ED-Phase5')
peda.ED.ND.Ph5.pos=peda.ED.ND.Ph5[,1]+1-min(peda.ED.ND.Ph5[,1])
checkNormality(log(peda.ED.ND.Ph5.pos),'Normality Test after log for for peda-ED-Phase5')
t.test(log(peda.ED.ND.Ph5.pos),mu=log(-min(peda.ED.ND.Ph5[,1])+1))


checkNormality(peda.MD.ND.Ph1[,1],'Normality Test for peda-MD-Phase1')
peda.MD.ND.Ph1.pos=peda.MD.ND.Ph1[,1]+1-min(peda.MD.ND.Ph1[,1])
checkNormality(log(peda.MD.ND.Ph1.pos),'Normality Test after log for for peda-MD-Phase1')
t.test(log(peda.MD.ND.Ph1.pos),mu=log(-min(peda.MD.ND.Ph1[,1])+1))


checkNormality(peda.MD.ND.Ph2[,1],'Normality Test for peda-MD-Phase2')
peda.MD.ND.Ph2.pos=peda.MD.ND.Ph2[,1]+1-min(peda.MD.ND.Ph2[,1])
checkNormality(log(peda.MD.ND.Ph2.pos),'Normality Test after log for for peda-MD-Phase2')
t.test(log(peda.MD.ND.Ph2.pos),mu=log(-min(peda.MD.ND.Ph2[,1])+1))


checkNormality(peda.MD.ND.Ph3[,1],'Normality Test for peda-MD-Phase3')
peda.MD.ND.Ph3.pos=peda.MD.ND.Ph3[,1]+1-min(peda.MD.ND.Ph3[,1])
checkNormality(log(peda.MD.ND.Ph3.pos),'Normality Test after log for for peda-MD-Phase3')
t.test(log(peda.MD.ND.Ph3.pos),mu=log(-min(peda.MD.ND.Ph3[,1])+1))


checkNormality(peda.MD.ND.Ph4[,1],'Normality Test for peda-MD-Phase4')
peda.MD.ND.Ph4.pos=peda.MD.ND.Ph4[,1]+1-min(peda.MD.ND.Ph4[,1])
checkNormality(log(peda.MD.ND.Ph4.pos),'Normality Test after log for for peda-MD-Phase4')
t.test(log(peda.MD.ND.Ph4.pos),mu=log(-min(peda.MD.ND.Ph4[,1])+1))

checkNormality(peda.MD.ND.Ph5[,1],'Normality Test for peda-MD-Phase5')
peda.MD.ND.Ph5.pos=peda.MD.ND.Ph5[,1]+1-min(peda.MD.ND.Ph5[,1])
checkNormality(log(peda.MD.ND.Ph5.pos),'Normality Test after log for for peda-MD-Phase5')
t.test(log(peda.MD.ND.Ph5.pos),mu=log(-min(peda.MD.ND.Ph5[,1])+1))


### Code to test Normality of PP and Apply T-Test

checkNormality(pp.CD.ND.Ph1[,1],'Normality Test for pp-CD-Phase1')
t.test(pp.CD.ND.Ph1[,1])


checkNormality(pp.CD.ND.Ph2[,1],'Normality Test for pp-CD-Phase2')
t.test(pp.CD.ND.Ph2[,1])

checkNormality(pp.CD.ND.Ph3[,1],'Normality Test for pp-CD-Phase3')
t.test(pp.CD.ND.Ph3[,1])


checkNormality(pp.CD.ND.Ph4[,1],'Normality Test for pp-CD-Phase4')
t.test(pp.CD.ND.Ph4[,1])

checkNormality(pp.CD.ND.Ph5[,1],'Normality Test for pp-CD-Phase5')
t.test(pp.CD.ND.Ph5[,1])

checkNormality(pp.ED.ND.Ph1[,1],'Normality Test for pp-ED-Phase1')
t.test(pp.ED.ND.Ph1[,1])

checkNormality(pp.ED.ND.Ph2[,1],'Normality Test for pp-ED-Phase2')
t.test(pp.ED.ND.Ph2[,1])

checkNormality(pp.ED.ND.Ph3[,1],'Normality Test for pp-ED-Phase3')
t.test(pp.ED.ND.Ph3[,1])


checkNormality(pp.ED.ND.Ph4[,1],'Normality Test for pp-ED-Phase4')
t.test(pp.ED.ND.Ph4[,1])



checkNormality(pp.ED.ND.Ph5[,1],'Normality Test for pp-ED-Phase5')
t.test(pp.ED.ND.Ph5[,1])

checkNormality(pp.MD.ND.Ph1[,1],'Normality Test for pp-MD-Phase1')
t.test(pp.MD.ND.Ph1[,1])

checkNormality(pp.MD.ND.Ph2[,1],'Normality Test for pp-MD-Phase2')
t.test(pp.MD.ND.Ph2[,1])


checkNormality(pp.MD.ND.Ph3[,1],'Normality Test for pp-MD-Phase3')
t.test(pp.MD.ND.Ph3[,1])


checkNormality(pp.MD.ND.Ph4[,1],'Normality Test for pp-MD-Phase4')
t.test(pp.MD.ND.Ph4[,1])


checkNormality(pp.MD.ND.Ph5[,1],'Normality Test for pp-MD-Phase5')
t.test(pp.MD.ND.Ph5[,1])




### boxplot for HR

par(mfrow=c(3,1))

boxplot(data=HR.CD, Rate~Phase, col="grey90", ylab=expression(bold(paste(Delta, "H [bpm]"))), ylim=c(-10, 15))
mtext(side=4, line=1, expression(bold("CD")))
abline(0,0)
mtext(c("","***","","***",""), side = 3,0, at=1:5,cex=1.5)
mtext(c(paste("n=",c(nrow(HR.CD.ND.Ph1)),sep=""),paste("n=",c(nrow(HR.CD.ND.Ph2)),sep=""), paste("n=",c(nrow(HR.CD.ND.Ph3)),sep=""), paste("n=",c(nrow(HR.CD.ND.Ph4)),sep=""), paste("n=",c(nrow(HR.CD.ND.Ph5)),sep="")), side = 3,2, at=1:5,cex=0.75)
mtext("Validation of Heart Rate Channel", side = 3,3)


boxplot(data=HR.ED, Rate~Phase, col="palevioletred1" , ylab=expression(bold(paste(Delta, "H [bpm]"))), ylim=c(-10, 15))
mtext(side=4, line=1, expression(bold("ED")))
abline(0,0)
mtext(c("","***","","*","*"), side = 3,0, at=1:5,cex=1.5)
mtext(c(paste("n=",c(nrow(HR.ED.ND.Ph1)),sep=""),paste("n=",c(nrow(HR.ED.ND.Ph2)),sep=""), paste("n=",c(nrow(HR.ED.ND.Ph3)),sep=""), paste("n=",c(nrow(HR.ED.ND.Ph4)),sep=""), paste("n=",c(nrow(HR.ED.ND.Ph5)),sep="")), side = 3,2, at=1:5,cex=0.75)



boxplot(data=HR.MD, Rate~Phase, col="gold" , ylab=expression(bold(paste(Delta, "H [bpm]"))), ylim=c(-10, 15))
mtext(side=4, line=1, expression(bold("MD")))
abline(0,0)
mtext(c("***","**","","***",""), side = 3,0, at=1:5,cex=1.5)
mtext(c(paste("n=",c(nrow(HR.MD.ND.Ph1)),sep=""),paste("n=",c(nrow(HR.MD.ND.Ph2)),sep=""), paste("n=",c(nrow(HR.MD.ND.Ph3)),sep=""), paste("n=",c(nrow(HR.MD.ND.Ph4)),sep=""), paste("n=",c(nrow(HR.MD.ND.Ph5)),sep="")), side = 3,2, at=1:5, cex=0.75)



### boxplot for BR

par(mfrow=c(3,1))

boxplot(data=BR.CD, Rate~Phase, col="grey90", ylab=expression(bold(paste(Delta, "B [bpm]"))), ylim=c(-10, 15))
mtext(side=4, line=1, expression(bold("CD")))
abline(0,0)
#mtext(c("","***","","***",""), side = 3,0, at=1:5)
mtext(c(paste("n=",c(nrow(BR.CD.ND.Ph1)),sep=""),paste("n=",c(nrow(BR.CD.ND.Ph2)),sep=""), paste("n=",c(nrow(BR.CD.ND.Ph3)),sep=""), paste("n=",c(nrow(BR.CD.ND.Ph4)),sep=""), paste("n=",c(nrow(BR.CD.ND.Ph5)),sep="")), side = 3,2, at=1:5,cex=0.75)
mtext("Validation of Breathing Rate Channel", side = 3,3)




boxplot(data=BR.ED, Rate~Phase, col="palevioletred1" , ylab=expression(bold(paste(Delta, "B [bpm]"))), ylim=c(-10, 15))
mtext(side=4, line=1, expression(bold("ED")))
abline(0,0)
mtext(c("","","**","",""), side = 3,0, at=1:5)
mtext(c(paste("n=",c(nrow(BR.ED.ND.Ph1)),sep=""),paste("n=",c(nrow(BR.ED.ND.Ph2)),sep=""), paste("n=",c(nrow(BR.ED.ND.Ph3)),sep=""), paste("n=",c(nrow(BR.ED.ND.Ph4)),sep=""), paste("n=",c(nrow(BR.ED.ND.Ph5)),sep="")), side = 3,2, at=1:5,cex=0.75)


boxplot(data=BR.MD, Rate~Phase, col="gold" , ylab=expression(bold(paste(Delta, "B [bpm]"))), ylim=c(-10, 15))
mtext(side=4, line=1, expression(bold("MD")))
abline(0,0)
mtext(c("","***","","***","**"), side = 3,0, at=1:5)
mtext(c(paste("n=",c(nrow(BR.MD.ND.Ph1)),sep=""),paste("n=",c(nrow(BR.MD.ND.Ph2)),sep=""), paste("n=",c(nrow(BR.MD.ND.Ph3)),sep=""), paste("n=",c(nrow(BR.MD.ND.Ph4)),sep=""), paste("n=",c(nrow(BR.MD.ND.Ph5)),sep="")), side = 3,2, at=1:5, cex=0.75)



### boxplot for Peda
par(mfrow=c(3,1))

boxplot(data=peda.CD, Rate~Phase, col="grey90", ylab=expression(bold(paste("PEDA [K",Omega,"]"))), ylim=c(-60,60))
mtext(side=4, line=1, expression(bold("CD")))
abline(0,0)
mtext(c(paste("n=",c(nrow(peda.CD.ND.Ph1)),sep=""),paste("n=",c(nrow(peda.CD.ND.Ph2)),sep=""), paste("n=",c(nrow(peda.CD.ND.Ph3)),sep=""), paste("n=",c(nrow(peda.CD.ND.Ph4)),sep=""), paste("n=",c(nrow(peda.CD.ND.Ph5)),sep="")), side = 3,2, at=1:5,cex=0.75)
mtext("Validation of Palm EDA Channel", side = 3,3)



boxplot(data=peda.ED, Rate~Phase, col="palevioletred1" , ylab=expression(bold(paste("PEDA [K",Omega,"]"))), ylim=c(-60,60))
mtext(side=4, line=1, expression(bold("ED")))
abline(0,0)
mtext(c(paste("n=",c(nrow(peda.ED.ND.Ph1)),sep=""),paste("n=",c(nrow(peda.ED.ND.Ph2)),sep=""), paste("n=",c(nrow(peda.ED.ND.Ph3)),sep=""), paste("n=",c(nrow(peda.ED.ND.Ph4)),sep=""), paste("n=",c(nrow(peda.ED.ND.Ph5)),sep="")), side = 3,2, at=1:5,cex=0.75)


boxplot(data=peda.MD, Rate~Phase, col="gold" , ylab=expression(bold(paste("PEDA [K",Omega,"]"))), ylim=c(-60,60))
mtext(side=4, line=1, expression(bold("MD")))
abline(0,0)
mtext(c(paste("n=",c(nrow(peda.MD.ND.Ph1)),sep=""),paste("n=",c(nrow(peda.MD.ND.Ph2)),sep=""), paste("n=",c(nrow(peda.MD.ND.Ph3)),sep=""), paste("n=",c(nrow(peda.MD.ND.Ph4)),sep=""), paste("n=",c(nrow(peda.MD.ND.Ph5)),sep="")), side = 3,2, at=1:5, cex=0.75)




### Boxplot for PP
par(mfrow=c(3,1))

boxplot(data=pp.CD, Rate~Phase, col="grey90", ylab=expression(bold(paste("PP [K",~degree~C,"]"))), ylim=c(-1.5e-3,1.5e-3))
mtext(side=4, line=1, expression(bold("CD")))
abline(0,0)
mtext(c("","***","","***",""), side = 3,0, at=1:5)
mtext(c(paste("n=",c(nrow(pp.CD.ND.Ph1)),sep=""),paste("n=",c(nrow(pp.CD.ND.Ph2)),sep=""), paste("n=",c(nrow(pp.CD.ND.Ph3)),sep=""), paste("n=",c(nrow(pp.CD.ND.Ph4)),sep=""), paste("n=",c(nrow(pp.CD.ND.Ph5)),sep="")), side = 3,2, at=1:5,cex=0.75)
mtext("Validation of Perinasal Perspiration Channel", side = 3,3)


boxplot(data=pp.ED, Rate~Phase, col="palevioletred1" , ylab=expression(bold(paste("PP [K",Omega,"]"))), ylim=c(-1.5e-3,1.5e-3))
mtext(side=4, line=1, expression(bold("ED")))
abline(0,0)
mtext(c("","***","","***",""), side = 3,0, at=1:5)
mtext(c(paste("n=",c(nrow(pp.ED.ND.Ph1)),sep=""),paste("n=",c(nrow(pp.ED.ND.Ph2)),sep=""), paste("n=",c(nrow(pp.ED.ND.Ph3)),sep=""), paste("n=",c(nrow(pp.ED.ND.Ph4)),sep=""), paste("n=",c(nrow(pp.ED.ND.Ph5)),sep="")), side = 3,2, at=1:5,cex=0.75)


boxplot(data=pp.MD, Rate~Phase, col="gold" , ylab=expression(bold(paste("PP [K",Omega,"]"))), ylim=c(-1.5e-3,1.5e-3))
mtext(side=4, line=1, expression(bold("MD")))
abline(0,0)
mtext(c("","***","","**",""), side = 3,0, at=1:5)
mtext(c(paste("n=",c(nrow(pp.MD.ND.Ph1)),sep=""),paste("n=",c(nrow(pp.MD.ND.Ph2)),sep=""), paste("n=",c(nrow(pp.MD.ND.Ph3)),sep=""), paste("n=",c(nrow(pp.MD.ND.Ph4)),sep=""), paste("n=",c(nrow(pp.MD.ND.Ph5)),sep="")), side = 3,2, at=1:5, cex=0.75)










