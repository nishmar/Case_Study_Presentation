### Visualize Interactions
#install.packages('effects')
library(effects)


turns <- c('first_turn','multiple_turns')
features <- c('complexity','praat15', 'liwc','lexical')#, 'IS09') #_segments in files

# ONE-WAY MANOVA on each feature group
#perform mshapiro.test() - part of mvnormtest package, 
#to ensure dependent variables are normally distribute within groups
#levene's test for homogeneity of variance, p value less than .05 indicates violation of assumption
#linearity of dependent variables- are any of our feature sets have linear relationships?

#Post-hoc ANOVAs with posthoc TukeyHSD
#1 for each feature
#Only prints significant results

##ANALYSIS FUNCTION
manova_posthoc <- function  (num_turns, feature){
  
  #load df
  if(feature=="lexical"){
    file = sprintf("questions/sensitive_analyses/%s_%s_num.csv", feature, num_turns)
  }
  else{
    file = sprintf("questions/sensitive_analyses/%s_%s.csv", feature, num_turns)
  }
  df <- read.csv(file) 
  
  df$T.F[df$T.F=="22"] <- "TRUE"
  df$T.F<- factor(df$T.F) #refactor levels
  
  Y<- unlist(df[,6])
  
  for (i in (7:length(df))){
    Y<- cbind(Y,unlist(df[,i]))
  }
  
  res.man<- manova(Y ~ df$Sensitive*df$T.F)
  print(summary(res.man))
  
  siglist<- c()
  df_res<-c()
  F_s<-c()
  F_d<- c()
  F_i<- c()
  p_s<- c()
  p_d<- c()
  p_i<- c()
  
  for (i in (6:length(df))){
    
    dv <- as.numeric(unlist(df[i]))
    
    df$Sensitive[df$Sensitive==TRUE]<- "SENSITIVE"
    df$Sensitive[df$Sensitive==FALSE]<- "NEUTRAL"
    
    Question<- factor(df$Sensitive)
    
    levels(df$T.F)[2] <-"TRUTHFUL"
    levels(df$T.F)[1] <-"DECEPTIVE"
    Answer<- factor(df$T.F)
    
    feature_name<- colnames(df[i])
    new<- data.frame("Question"=Question,"Answer"= Answer, feature_name = dv)
    
    fit <- aov(feature_name ~ Answer*Question, data=new)
    result<- anova(fit)
    
    if(result$Pr[2]< 0.05 | result$Pr[3]< 0.05){
      siglist<- c(siglist, colnames(df[i]))
  
      df_res<-c(df_res, result$Df[4])
      F_s<-c(F_s, result$F[2])
      F_d<- c(F_d, result$F[1])
      F_i<- c(F_i, result$F[3])
      
      p_s<- c(p_s, result$Pr[2]) 
      p_d<- c(p_d, result$Pr[1]) 
      p_i<- c(p_i, result$Pr[3])
      
      print(colnames(df[i]))
      print(result)
      
      #posthoc<- TukeyHSD(fit)
      #print(posthoc)
      
      #Plotting effects
      jpeg(sprintf("questions/sensitive_analyses/plots/%s_%s_%s.jpeg",feature,feature_name,num_turns))
      plot(allEffects(fit), multiline=TRUE, ci.style="bars", ylab=feature_name) ##
      dev.off()
      }
  }
  print(siglist)
  
  p_s_adj<- p.adjust(p_s,method="BH") 
  p_d_adj<- p.adjust(p_d,method="BH")
  p_i_adj<- p.adjust(p_i,method="BH")
  
  results<- data.frame(siglist, F_s,F_d,F_i, df_res, p_s_adj, p_d_adj, p_i_adj)
  out_file = sprintf("questions/sensitive_analyses/%s.csv",feature)
  write.csv(results,out_file)
}

##ANALYSIS 1a
siglist <- c('')
for (i in (6:length(df_c))){
  fit <- aov(unlist(df_c[i]) ~ factor(df_c$Sensitive)*factor(df_c$T.F))
  result<- anova(fit)
  
  if(result$Pr[1]< 0.05){
    siglist<- c(siglist, colnames(df_c[i]))
    print(colnames(df_c[i]))
    print(result)

    posthoc<- TukeyHSD(fit)
    print(posthoc)
  }
}

manova_posthoc('first_turn', 'complexity')
manova_posthoc('multiple_turns','complexity')


## ANALYSIS 2a: praat single turn
manova_posthoc('first_turn','praat15')
manova_posthoc('multiple_turns','praat15')

#ANALYSIS 3a: lexical features - LDI
#create separate dfs for categorical vs num features
file = sprintf("questions/sensitive_analyses/%s_%s.csv", 'lexical', 'first_turn')
df <- read.csv(file)
#The non-binary features here: numCuePhrases, numHedgePhrases, 
#numLaugh, complexity, DAL-wc, DAL-pleasant, DAL-activate, DAL-imagery, 
#specScores
num_feat <- c("Participant","Qnum","Sensitive","T.F",
              "numFilledPauses","numCuePhrases", "numHedgePhrases",
              "complexity", "numLaugh", "DAL.wc", "DAL.pleasant", "DAL.activate",
              "DAL.imagenary","specScores")
new<- df[num_feat]
write.csv(new, "questions/sensitive_analyses/lexical_first_turn_num.csv")
file = sprintf("questions/sensitive_analyses/%s_%s.csv", 'lexical', 'multiple_turns')
df <- read.csv(file)
new<- df[num_feat]
write.csv(new, "questions/sensitive_analyses/lexical_multiple_turns_num.csv")

manova_posthoc('first_turn','lexical')
manova_posthoc('multiple_turns','lexical')

##LIST OF CAT FEATURES
cat_feat <- c("Participant","Qnum","Sensitive","T.F", "hasAbsolutelyReally",
              "hasContraction", "hasI", "hasWe", "hasYes", "hasNAposT",
              "hasNo", "hasNot", "isJustYes", "isJustNo", "noYesOrNo",
              "specificDenial", "thirdPersonPronouns", "hasFalseStart",
              "hasFilledPause", "hasCuePhrase", "hasHedgePhrase", "hasLaugh")


#ANALYSES LIWC: subset liwc features
#word_count <- c('WC')
summary_lang <- c('WC','Analytic','Clout','Authentic','Tone','Sixltr','Dic')
ling_dim <- c('function.','pronoun','ppron','i','we','you','shehe','they','ipron','article',
              'prep', 'auxverb', 'adverb', 'conj', 'negate')
other_grammar <- c('verb', 'adj', 'compare', 'interrog', 'number', 'quant')
affect <- c('affect','posemo', 'negemo','anx','anger','sad')
social <- c('social', 'family','friend', 'female','male')
cog <- c('cogproc', 'insight', 'cause', 'discrep', 'tentat',
         'certain', 'differ')
percept <- c('percept', 'see', 'hear', 'feel')
biological<- c('bio', 'body','health','sexual','ingest')
drives<- c('drives','affiliation','achieve','power','reward','risk')
time<- c('focuspast','focuspresent','focusfuture')
relativity<- c('relativ','motion','space','time')
personal <- c('work','leisure','home','money','relig','death')
informal <- c('informal','swear','netspeak','assent','nonflu','filler')

liwc_subsets<- c('summary_lang','ling_dim', 'other_grammar',
                 'affect', 'social', 'cog', 'percept', 'biological', 'drives',
                 'time', 'relativity', 'personal', 'informal') 

liwc_analysis <- function (subset, num_turns){
  feature<- "liwc"
  file = sprintf("questions/sensitive_analyses/liwc_%s.csv", num_turns)
  df <- read.csv(file) 
  
  #start with first feature in subset
  Y<- unlist(df[,which(names(df)==subset[1])])
  
  #add the rest of the features in subset to Y matrix
  for (i in (2:length(subset))){
    Y<- cbind(Y,unlist(df[,which(names(df)==subset[i])]))
  }
  res.man<- manova(Y ~ df$Sensitive*df$T.F)
  print(summary(res.man))
  
  siglist<- c()
  df_res<-c()
  F_s<-c()
  F_d<- c()
  F_i<- c()
  p_s<- c()
  p_d<- c()
  p_i<- c()
  
  #for each feature in subset
  for (i in (1:length(subset))){
    
    dv <- as.numeric(unlist(df[, which(names(df)==subset[i])]))
    df$Sensitive[df$Sensitive==TRUE]<- "SENSITIVE"
    df$Sensitive[df$Sensitive==FALSE]<- "NEUTRAL"
    
    Question<- factor(df$Sensitive)
    
    df$T.F[df$T.F==TRUE] <- "TRUTHFUL"
    df$T.F[df$T.F==FALSE] <- "DECEPTIVE"
    Answer<- factor(df$T.F)
    
    feature_name<- subset[i]
    new<- data.frame("Question"=Question,"Answer"= Answer, "dv" = dv)
    
    fit <- aov(dv ~ Answer*Question, data=new)
    result<- anova(fit)
    
    #print(result)
    
    if(result$Pr[2]< 0.05 | result$Pr[3]< 0.05){
      siglist<- c(siglist, subset[i])
      
      df_res<-c(df_res, result$Df[4])
      F_s<-c(F_s, result$F[2])
      F_d<- c(F_d, result$F[1])
      F_i<- c(F_i, result$F[3])
      
      p_s<- c(p_s, result$Pr[2]) 
      p_d<- c(p_d, result$Pr[1]) 
      p_i<- c(p_i, result$Pr[3]) 
      
      print(subset[i])
      print(result)
      
      posthoc<- TukeyHSD(fit)
      print(posthoc)
      
      #Plotting effects
      jpeg(sprintf("questions/sensitive_analyses/plots/%s_%s_%s.jpeg",feature,feature_name,num_turns))
      plot(allEffects(fit), multiline=TRUE, ci.style="bars", ylab=feature_name) ##
      dev.off()
    }
  }
  print(siglist)
  p_s_adj<- p.adjust(p_s,method="BH") 
  p_d_adj<- p.adjust(p_d,method="BH")
  p_i_adj<- p.adjust(p_i,method="BH")
  
  results<- data.frame(siglist, F_s,F_d,F_i, df_res, p_s_adj, p_d_adj, p_i_adj)
  out_file = sprintf("questions/sensitive_analyses/liwc_%s.csv", subset[1])
  write.csv(results,out_file)
  
}

liwc_analysis(summary_lang,'first_turn')
liwc_analysis(ling_dim,'first_turn')
liwc_analysis(other_grammar,'first_turn')
liwc_analysis(affect,'first_turn')
liwc_analysis(social,'first_turn')
liwc_analysis(cog,'first_turn')
liwc_analysis(percept,'first_turn') #NS
liwc_analysis(biological,'first_turn')
liwc_analysis(drives,'first_turn')
liwc_analysis(time,'first_turn')
liwc_analysis(relativity,'first_turn')
liwc_analysis(personal,'first_turn')
liwc_analysis(informal,'first_turn')

liwc_analysis(summary_lang,'multiple_turns')
liwc_analysis(ling_dim,'multiple_turns')
liwc_analysis(other_grammar,'multiple_turns')
liwc_analysis(affect,'multiple_turns')
liwc_analysis(social,'multiple_turns')
liwc_analysis(cog,'multiple_turns')
liwc_analysis(percept,'multiple_turns') #NS
liwc_analysis(biological,'multiple_turns')
liwc_analysis(drives,'multiple_turns')
liwc_analysis(time,'multiple_turns')
liwc_analysis(relativity,'multiple_turns')
liwc_analysis(personal,'multiple_turns')
liwc_analysis(informal,'multiple_turns')

#-----
#Discriminant analysis 

##Find missing values
sapply(df, function(x) sum(is.na(x)))




