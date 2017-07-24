library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
require(zoo)

wd <- "F:/Documents/Data/P20_2013/meta"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)


weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

####Run function####
# set our working directory, change this if using on another machine
wd <- "F:/Documents/Data/DHSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

dataList <- list()
dataIndex <- 1

# Loop through every dir
for(i in 2:length(dirs)){
  dir <- dirs[i]
  # Pull some coded info out of the dir name
  country <- tolower(substr(basename(dir),1,2))
  recode <- tolower(substr(basename(dir),3,4))
  phase <- as.integer(substr(basename(dir),5,5))
  # For this analysis, we're only interested in individual member recodes, or "hr"
  if(basename(dir) %in% povcalcuts$filename){
    message(basename(dir))
    hrwd <- dir
    if(!file_test(op="-d", hrwd)){next;}
    
    hrBase <- basename(hrwd)
    iso2 <- toupper(substr(hrBase,1,2))
    phase <- substr(hrBase,5,6)
    
    prwd <- paste0("F:/Documents/Data/DHSauto/",tolower(iso2),"pr",phase,"dt/")
    if(!file_test(op="-d", prwd)){next;}
    
    pr <- read.csv(paste0(prwd,iso2,"PR",phase,"FL.csv")
                   ,na.strings="",as.is=TRUE,check.names=FALSE)
    
    names(pr)[which(names(pr)=="hv271")] <- "wealth"
    pr$wealth <- pr$wealth/100000
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    #Rename urban var
    names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
    
    #Rename educ var
    names(pr)[which(names(pr)=="hv109")] <- "educ"
    recode.educ <- function(x){
      if(is.na(x)){return(NA)}
      else if(tolower(x)=="dk" | tolower(x)=="don't know" | tolower(x)=="missing" | x==8 | x==9){return(NA)}
      else if(x==0 | x==1 | tolower(x)=="no education, preschool" | tolower(x)=="no education" | tolower(x)=="incomplete primary"){return("No education, preschool")}
      else if(x==2 | x==3 | tolower(x)=="complete primary" | tolower(x)=="incomplete secondary"){return("Primary")}
      else if(x==4 | tolower(x)=="complete secondary"){return("Secondary")}
      else if(x==5 | tolower(x)=="higher"){return("Higher")}
      else{return(NA)}
    }
    pr$educ <- sapply(pr$educ,recode.educ)
    
    #Rename age var
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    #Rename sex var
    names(pr)[which(names(pr)=="hv104")] <- "sex"
    
    #Rename cluster/hh var
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hv024")] <- "region"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    names(pr)[which(names(pr)=="hv112")] <- "mother.line"
    pr$mother.line[which(pr$mother.line==99)] <- NA
    
    #Head vars
    names(pr)[which(names(pr)=="hv219")] <- "head.sex"
    names(pr)[which(names(pr)=="hv220")] <- "head.age"
    
    #reg?
    names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
    
    #nutrition
    names(pr)[which(names(pr)=="ha40")] <- "woman.bmi"
    if(typeof(pr$woman.bmi)!="NULL"){
      pr$woman.bmi <- pr$woman.bmi/100 
    }else{
      pr$woman.bmi <- NA
    }
    names(pr)[which(names(pr)=="hb40")] <- "man.bmi"
    if(typeof(pr$man.bmi)!="NULL"){
      pr$man.bmi <- pr$man.bmi/100 
    }else{
      pr$man.bmi <- NA
    }
    names(pr)[which(names(pr)=="hc1")] <- "age.months"
    names(pr)[which(names(pr)=="hc2")] <- "weight.kg"
    names(pr)[which(names(pr)=="hc3")] <- "height.cm"
    names(pr)[which(names(pr)=="hc15")] <- "standing.lying"
    names(pr)[which(names(pr)=="hc5")] <- "child.height.age"
    names(pr)[which(names(pr)=="hc11")] <- "child.weight.height"
    if(typeof(pr$child.height.age)=="NULL"){
      pr$child.height.age <- NA
    }else{
      pr$child.height.age <- pr$child.height.age/100
    }
    if(typeof(pr$child.weight.height)=="NULL"){
      pr$child.weight.height <- NA
    }else{
      pr$child.weight.height <- pr$child.weight.height/100
    }
    pr$child.weights <- pr$weights
    
    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    np20cut <- 0.2
    extcut <- subset(povcalcuts,filename==hrBase)$extreme
    cuts <- c(povcalcut,np20cut,extcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
    
    pr$p20 <- (pr$wealth < povperc[1])
    pr$np20 <- (pr$wealth < povperc[2])
    pr$ext <- (pr$wealth < povperc[3])
    
    mothers <- unique(pr[c("cluster","household","line","woman.bmi")])
    mothers <- mothers[complete.cases(mothers),]
    names(mothers) <- c("cluster","household","mother.line","mother.bmi")
    pr <- join(
      pr
      ,mothers
      ,by=c("cluster","household","mother.line")
    )
    
    keep <- c("wealth","weights","urban.rural","region","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","np20","ext","child.weight.height"
    )
    prNames <- names(pr)
    namesDiff <- setdiff(keep,prNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        pr[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      } 
    }
    data <- pr[keep]
    data$filename <- hrBase
    dataList[[dataIndex]] <- data
    dataIndex <- dataIndex + 1
  }
}

wd <- "C:/Users/Zach/Documents/GNR 2017"
setwd(wd)

data.total <- rbindlist(dataList)

recode.urban <- function(x){
  if(is.na(x)){return(NA)}
  else if(x==0 | tolower(x)=="rural"){return(0)}
  else if(x==1 | tolower(x)=="urban"){return(1)}
  else{return(NA)}
}
data.total$urban <- sapply(data.total$urban.rural,recode.urban)

recode.educ <- function(x){
  if(is.na(x)){return(NA)}
  else if(tolower(x)=="dk" | tolower(x)=="don't know"){return(NA)}
  else if(x==0 | tolower(x)=="no education, preschool"){return("No education, preschool")}
  else if(x==1 | tolower(x)=="primary"){return("Primary")}
  else if(x==2 | tolower(x)=="secondary"){return("Secondary")}
  else if(x==3 | tolower(x)=="higher"){return("Higher")}
  else{return(NA)}
}
data.total$educ <- sapply(data.total$educ,recode.educ)
data.total$educ <- factor(data.total$educ
                          ,levels = c("No education, preschool","Primary","Secondary","Higher")
)

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

data.total$ageCategory <- vapply(data.total$age,codeAgeCat,character(1))
data.total$ageCategory <- factor(data.total$ageCategory,
                                 levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                            ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                            ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                            ,"95+","missing")                          
)

data.total$head.ageCategory <- vapply(data.total$head.age,codeAgeCat,character(1))
data.total$head.ageCategory <- factor(data.total$head.ageCategory,
                                      levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                                 ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                                 ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                                 ,"95+","missing")                          
)

sex.missing = c(NA,"missing",9)
sex.male = c(1,"male","masculin","hombre")
sex.female = c(2, "female","feminin","mujer")
data.total$sex[which(tolower(data.total$sex) %in% sex.missing)] <- NA
data.total$sex[which(tolower(data.total$sex) %in% sex.male)] <- "Male"
data.total$sex[which(tolower(data.total$sex) %in% sex.female)] <- "Female"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.missing)] <- NA
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.male)] <- "Male"
data.total$head.sex[which(tolower(data.total$head.sex) %in% sex.female)] <- "Female"

#0 - neither certificate or registered
#1 - has certificate
#2 - registered
#8 - dk
birth.cert.missing <- c(NA,"dk","don't know",8,9,"missing","nsp","manquant","no sabe")
birth.cert.no <- c("registered",0,2,"neither certificate or registered","no","non","has only hospital card")
birth.cert.yes <- setdiff(unique(tolower(data.total$birth.cert)),c(birth.cert.no,birth.cert.missing))

birth.reg.missing <- c(NA,"dk","missing","nsp","manquant")
birth.reg.no <- c("no","non")
birth.reg.yes <- c("yes","oui","sí")
#count registrations if birth.cert var reveals it to be so
birth.cert.registered <- c(2,"registered","has only hospital card",birth.cert.yes)
birth.cert.not.registered <- c(0,"neither certificate or registered","no","non")
data.total$birth.reg.coded <- unfactor(data.total$birth.reg)
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.registered)] <- "Yes"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.not.registered)] <- "No"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & grepl("visto",data.total$birth.cert))] <- "Yes"

data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.missing)] <- NA
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.no)] <- 0
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.yes)] <- 1
data.total$birth.reg.coded[which(substr(data.total$birth.reg.coded,1,1)=="S")] <- 1

data.total$birth.reg <- data.total$birth.reg.coded

data.total$birth.cert <- unfactor(data.total$birth.cert)
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.missing)] <- NA
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.no)] <- 0
data.total$birth.cert[which(tolower(data.total$birth.cert) %in% birth.cert.yes)] <- 1
data.total$birth.cert[which(grepl("visto",data.total$birth.cert))] <- 1

data.total$woman.bmi[which(data.total$woman.bmi>80)] <- NA
data.total$woman.bmi.class <- NA
data.total$woman.bmi.class[which(data.total$woman.bmi<16)] <- "Severe thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=16 & data.total$woman.bmi<17)] <- "Moderate thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=17 & data.total$woman.bmi<18.5)] <- "Mild thinness"
data.total$woman.bmi.class[which(data.total$woman.bmi>=18.5 & data.total$woman.bmi<25)] <- "Normal range"
data.total$woman.bmi.class[which(data.total$woman.bmi>=25 & data.total$woman.bmi<30)] <- "Pre-obese"
data.total$woman.bmi.class[which(data.total$woman.bmi>=30 & data.total$woman.bmi<35)] <- "Obese class I"
data.total$woman.bmi.class[which(data.total$woman.bmi>=35 & data.total$woman.bmi<40)] <- "Obese class II"
data.total$woman.bmi.class[which(data.total$woman.bmi>=40)] <- "Obese class III"

data.total$woman.bmi.class <- factor(data.total$woman.bmi.class
                                     ,levels=c(
                                       "Severe thinness"
                                       ,"Moderate thinness"
                                       ,"Mild thinness"
                                       ,"Normal range"
                                       ,"Pre-obese"
                                       ,"Obese class I"
                                       ,"Obese class II"
                                       ,"Obese class III"
                                     ))

data.total$man.bmi[which(data.total$man.bmi>80)] <- NA
data.total$man.bmi.class <- NA
data.total$man.bmi.class[which(data.total$man.bmi<16)] <- "Severe thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=16 & data.total$man.bmi<17)] <- "Moderate thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=17 & data.total$man.bmi<18.5)] <- "Mild thinness"
data.total$man.bmi.class[which(data.total$man.bmi>=18.5 & data.total$man.bmi<25)] <- "Normal range"
data.total$man.bmi.class[which(data.total$man.bmi>=25 & data.total$man.bmi<30)] <- "Pre-obese"
data.total$man.bmi.class[which(data.total$man.bmi>=30 & data.total$man.bmi<35)] <- "Obese class I"
data.total$man.bmi.class[which(data.total$man.bmi>=35 & data.total$man.bmi<40)] <- "Obese class II"
data.total$man.bmi.class[which(data.total$man.bmi>=40)] <- "Obese class III"

data.total$man.bmi.class <- factor(data.total$man.bmi.class
                                   ,levels=c(
                                     "Severe thinness"
                                     ,"Moderate thinness"
                                     ,"Mild thinness"
                                     ,"Normal range"
                                     ,"Pre-obese"
                                     ,"Obese class I"
                                     ,"Obese class II"
                                     ,"Obese class III"
                                   ))

data.total$mother.bmi[which(data.total$mother.bmi>80)] <- NA
data.total$mother.bmi.class <- NA
data.total$mother.bmi.class[which(data.total$mother.bmi<16)] <- "Severe thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=16 & data.total$mother.bmi<17)] <- "Moderate thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=17 & data.total$mother.bmi<18.5)] <- "Mild thinness"
data.total$mother.bmi.class[which(data.total$mother.bmi>=18.5 & data.total$mother.bmi<25)] <- "Normal range"
data.total$mother.bmi.class[which(data.total$mother.bmi>=25 & data.total$mother.bmi<30)] <- "Pre-obese"
data.total$mother.bmi.class[which(data.total$mother.bmi>=30 & data.total$mother.bmi<35)] <- "Obese class I"
data.total$mother.bmi.class[which(data.total$mother.bmi>=35 & data.total$mother.bmi<40)] <- "Obese class II"
data.total$mother.bmi.class[which(data.total$mother.bmi>=40)] <- "Obese class III"

data.total$mother.bmi.class <- factor(data.total$mother.bmi.class
                                      ,levels=c(
                                        "Severe thinness"
                                        ,"Moderate thinness"
                                        ,"Mild thinness"
                                        ,"Normal range"
                                        ,"Pre-obese"
                                        ,"Obese class I"
                                        ,"Obese class II"
                                        ,"Obese class III"
                                      ))

data.total$child.height.age[which(data.total$child.height.age>80)] <- NA
data.total$wasting <- NA
# data.total$stunting[which(data.total$child.height.age<= (-6))] <- "Implausibly low"
data.total$stunting[which(data.total$child.height.age > (-6) & data.total$child.height.age<= (-3))] <- "Severely stunted"
data.total$stunting[which(data.total$child.height.age > (-3) & data.total$child.height.age<= (-2))] <- "Stunted, but not severely"
data.total$stunting[which(data.total$child.height.age > (-2) & data.total$child.height.age< (6))] <- "Not stunted"
# data.total$stunting[which(data.total$child.height.age>= (6))] <- "Implausibly high"

data.total$stunting <- factor(data.total$stunting
                              ,levels=c(
                                "Implausibly low"
                                ,"Severely stunted"
                                ,"Stunted, but not severely"
                                ,"Not stunted"
                                ,"Implausibly high"
                              ))
#Wasting
data.total$child.weight.height[which(data.total$child.weight.height>80)] <- NA
data.total$wasting <- NA
# data.total$wasting[which(data.total$child.weight.height<= (-6))] <- "Implausibly low"
data.total$wasting[which(data.total$child.weight.height > (-6) & data.total$child.weight.height<= (-3))] <- "Severely wasted"
data.total$wasting[which(data.total$child.weight.height > (-3) & data.total$child.weight.height<= (-2))] <- "wasted, but not severely"
data.total$wasting[which(data.total$child.weight.height > (-2) & data.total$child.weight.height< (6))] <- "Not wasted"
# data.total$wasting[which(data.total$child.weight.height>= (6))] <- "Implausibly high"

data.total$wasting <- factor(data.total$wasting
                              ,levels=c(
                                "Implausibly low"
                                ,"Severely wasted"
                                ,"wasted, but not severely"
                                ,"Not wasted"
                                ,"Implausibly high"
                              ))


save(data.total,file="total_triple.RData")