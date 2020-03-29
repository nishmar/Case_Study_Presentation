# Nishi Cestero - 2/1/2019
# sensitive_ write_features.R: 
# Parse info and read files with experiment features into df for sensitive questions
# feature analysis

#each file in format
# p382p383-part1_ch1:434.186830:434.846830:q20:n1:F.txt

#all are in interviewee role 
#determine interviewee ->look at part and ch
#get question num and T/F values
#read feature labels from all_feature_names.csv 

# information will be output in a table in this format
# participant | qnum | T/F | sensitive | features_labels..

parse_files<- function  (num_turns, feature){
  
  feature_name <- feature 
  
  #get all files under feature directory 
  feature<- feature_labels[which(feature_labels$V1==feature),]
  feature<- feature[feature!=""]
  feature<- feature[(2:length(feature))]
  
  #files_complexity = files_feature
  directory = sprintf("questions/%s/%s_segments/", num_turns, feature_name)
  #print(directory)
  files_feature<- list.files(directory)
  
  header <- c('Participant','Qnum', 'Sensitive', 'T/F', feature)
  
  #print(header)
  #print(length(header))
  
  df <- data.frame(matrix(ncol=length(header),nrow=0))
  colnames(df)<-header
  
  #loop over files
  #parse out relevant information from filename
  #delimited by :
  
  for (i in (1:length(files_feature))){
    
    s<- unlist(strsplit(files_feature[i],':')) #parse filename
    p<- unlist(strsplit(s[1],'-')) #participants and part
    
    participants <- unlist(strsplit(p[1],"p"))
    turn = unlist(strsplit(p[2],"_"))[1] #part
    qnum<- s[4]
    T_F <- gsub(".txt","", s[6])
    
    if (turn == "part1") {
      ee <- participants[3]
    } else{
      ee<- participants[2]
    }
  
    #open files to get part, qnum, T/F, features
    
    if(feature_name == "IS09"){
      values <- npyLoad(paste(directory,files_feature[i], sep=""))
    }
    
    else if (feature_name=="liwc"){
      #delimiter is "," transcript is present before values and followed by comma
      #discard transcript and store all values
      values <- read.csv(paste(directory,files_feature[i], sep=""), 
                           stringsAsFactor = FALSE,
                           header=FALSE)
      
      values<- values[(2:length(values))]

    }
    
    else if(feature_name=="lexical"){
      print(sprintf("filename: %s", files_feature[i]))
      
      data<- readLines(paste(directory,files_feature[i], sep=""))
      data<- strsplit(data,'\t')
      data<- unlist(data)
      
      header<-data[1]
      header<-strsplit(header,",")
      header<-unlist(header)
      header<-header[-1]
      header <- c('Participant','Qnum', 'Sensitive', 'T/F', header)
      
      
      data<- data[-1:-2]
      
      values<- data.frame(rbind(data))
      print(sprintf("length values: %f -- length header: %f",length(values), length(header)))
      #print(values)
      
    }
    
    else if(num_turns=="multiple_turns" & feature_name=="complexity"){
      values<- read.csv(paste(directory,files_feature[i], sep=""),
                        stringsAsFactors = FALSE,
                        header=FALSE)
    }
    
    else{
      values <- read.table(paste(directory,files_feature[i], sep=""), stringsAsFactor = FALSE,
                           header=FALSE,
                           text= readLines(file.choose(), warn= FALSE))
    }
    
    #add info to df
    row<- data.frame(ee, qnum, FALSE,T_F, values)
    
    #if finds extra values in file, print out length and filename
    if(length(row) != length(header)){
      print(sprintf("filename: %s", files_feature[i]))
      print(sprintf("length row: %f -- length expected: %f",length(row), length(header)))
      print(row)
    }
    
    #otherwise bind values in file to existing df
    else{
      colnames(row)<-header
      df<- rbind(df,row)
    }
    
  }
  
  return(df)
    
}

########## MAIN ###########

#install.packages('RcppCNPy')
#library(RcppCNPy)

setwd("/local/users/deception/full_corpus/")

turns <- c('first_turn','multiple_turns')
features <- c('lexical')#, 'complexity','praat15', 'liwc', 'IS09') #_segments in files

feature_labels <- read.csv("all_feature_names.csv", stringsAsFactors=F, header=F)

#sensitive q numbers:
# We followed the criteria in Tourangeau and Yan (2007) to define sensitive questions in our corpus.  These questions are related to money (16), parental or romantic relationships (5, 14, 15), mortality (23), and socially undesirable behaviors or experiences (12, 13, 24).
# 
# Tourangeau, R., & Yan, T. (2007). Sensitive questions in surveys. Psychological bulletin, 133(5), 859.
sensitive_q <- c("q16","q5","q14","q15","q23","q12","q13","q13","q24")

#set output directory
output_dir = "questions/sensitive_analyses/"

#loop through file path
#loop features
#call parse files


for (i in 1:length(turns)){
  for (j in 1:length(features)){
    df <- parse_files(turns[i], features[j])
    
    #save df as file with feature label
    output_file<- paste(output_dir,sprintf("%s_%s.csv",features[j],turns[i]), sep="")
    
    #Mark  sensitive questions 
    for (k in 1:length(sensitive_q)){
      df$Sensitive[df$Qnum==sensitive_q[k]] <- TRUE
    }
    
    write.csv(df, output_file)
  }
}

## Read/write numpy files w/ R for opensmile features
#npmat <- npyLoad("questions/first_turn/IS09_segments/p564p565-part2_ch2:987.840871:988.712871:q23:n1:T.npy")
#another alternative to reading numpy
#install.packages('reticulate')
#library('reticulate')
# 




