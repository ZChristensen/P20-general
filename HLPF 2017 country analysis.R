library(readr)
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
library(survey)
library(WDI)
require(zoo)
# code for bycountry_tabs is here https://github.com/akmiller01/alexm-util/blob/master/DevInit/R/P20/2013_tab_data.R an
# and https://github.com/akmiller01/alexm-util/blob/master/DevInit/R/P20/crosstabs_2013.R  ***
####Function and setup####
# library(Hmisc)
# library(plyr)
# library(foreign)
# library(data.table)
# library(WDI)
# library(varhandle)
# require(zoo)
# 
wd <- "F:/Documents/Data/P20_2013/meta"
setwd(wd)

povcalcuts <- read.csv("headcounts.csv",as.is=TRUE)
povcalcuts$filename[which(povcalcuts$filename=="bdhr70dt")] <- "bdhr61dt"
povcalcuts$filename[which(povcalcuts$filename=="bfhr70dt")] <- "bfhr62dt"
povcalcuts$filename[which(povcalcuts$filename=="idhr63dt")] <- "idhr51dt"
povcalcuts$filename[which(povcalcuts$filename=="kehr7hdt")] <- "kehr70dt"
povcalcuts$filename[which(povcalcuts$filename=="mdhr6hdt")] <- "mdhr51dt"
povcalcuts$filename[which(povcalcuts$filename=="mwhr71dt")] <- "mwhr7hdt"
povcalcuts$filename[which(povcalcuts$filename=="phhr61dt")] <- "phhr52dt"
# povcalcuts$filename[which(povcalcuts$filename=="tzhr6adt")] <- "tzhr63dt"
povcalcuts$filename[which(povcalcuts$filename=="ughr72dt")] <- "ughr60dt"
povcalcuts= subset(povcalcuts, iso3=="BGD"|iso3=="NPL"|iso3=="NGA"|iso3=="KEN"|iso3=="BRA") 

indicator <- "SI.POV.NAHC"

dat <- WDI(country = "all",
           indicator = indicator,
           start = 1990,
           end = 2016,
           extra = TRUE
           #cache = new_cache
)

dat <- dat[c("iso3c","year","SI.POV.NAHC")]
names(dat) <- c("iso3","year","hc")
dat$hc <- dat$hc/100
dat <- dat[order(dat$year),]
dat <- dat[order(dat$iso3),]
colname <- "hc"
dat <- ddply(dat,.(iso3),function(x)
{
  naLen <- nrow(x[which(is.na(x[,colname])),])
  allLen <- nrow(x)
  valueLen <- allLen-naLen
  ival <- x[,colname]
  x[,paste("original",colname,sep="-")] <- ival
  if(valueLen>=2)
  {
    interpVals <- na.approx(x[,colname],na.rm=FALSE,rule=2)
  }
  else if(valueLen==1){
    interpVals <- rep(sum(x[,colname],na.rm=TRUE),allLen)
  }
  else{
    interpVals <- rep(NA,allLen)
  }
  x[,colname] <- interpVals
  return(x)
}
)
names(dat) <- c("iso3","year","pl.hc","pl.hc.original")
povcalcuts <- join(povcalcuts,dat,by=c("iso3","year"))

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

    pr <- read.dta(paste0(prwd,iso2,"PR",phase,"FL.dta"))
                 

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
    if(typeof(pr$child.height.age)=="NULL"){
      pr$child.height.age <- NA
    }else{
      pr$child.height.age <- pr$child.height.age/100
    }
    pr$child.weights <- pr$weights

    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    np20cut <- 0.2
    nplcut <- subset(povcalcuts,filename==hrBase)$pl.hc
    extcut <- subset(povcalcuts,filename==hrBase)$extreme
    cuts <- c(povcalcut,np20cut,nplcut,extcut)
    povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)

    pr$p20 <- (pr$wealth < povperc[1])
    pr$np20 <- (pr$wealth < povperc[2])
    pr$npl <- (pr$wealth < povperc[3])
    pr$ext <- (pr$wealth < povperc[4])

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
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","np20","npl","ext"
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

setwd("F:/Documents/Data/MICSmeta")
varNames <- read.csv("mics_meta_vars_complete.csv",as.is=TRUE,na.strings="")
classes <- read.csv("global_mics_classes.csv",as.is=TRUE,na.strings="NAN")

wd <- "F:/Documents/Data/MICSauto/"
setwd(wd)

# List out all the directories in our wd, this is where our data is contained
dirs <- list.dirs(wd,full.names=TRUE)

# dir <- "D:/Documents/Data/MICSauto/Somalia MICS 2006 SPSS Datasets"
# dir <- "D:/Documents/Data/MICSauto/Algeria_MICS4_Datasets"
# dir <- "F:/Documents/Data/MICSauto/Nepal_MICS5_Datasets"

for(i in 2:length(dirs)){
  dir <- dirs[i]
  hrBase <- basename(dir)
  if(hrBase %in% povcalcuts$filename){

    message(hrBase)
    if(exists("hh")){rm(hh)}
    if(exists("hl")){rm(hl)}
    if(exists("ch")){rm(ch)}
    if(exists("wm")){rm(wm)}
    load(paste0(dir,"/","hh.RData"))
    load(paste0(dir,"/","hl.RData"))
    load(paste0(dir,"/","ch.RData"))
    load(paste0(dir,"/","wm.RData"))
    hh <- data.frame(hh,as.is=TRUE,check.names=FALSE)
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    ch <- data.frame(ch,as.is=TRUE,check.names=FALSE)
    wm <- data.frame(wm,as.is=TRUE,check.names=FALSE)
    names(hh) <- tolower(names(hh))
    names(hl) <- tolower(names(hl))
    names(ch) <- tolower(names(ch))
    names(wm) <- tolower(names(wm))

    file.varName <- subset(varNames,filename==hrBase)

    attendedVar <- subset(file.varName,match=="attended")$varName
    gradeVar <- subset(file.varName,match=="grade")$varName
    schoolVar <- subset(file.varName,match=="school")$varName

    ynm.classes <- subset(classes,filename==hrBase & type=="ynm")
    attended.classes <- subset(classes,filename==hrBase & type=="attended")
    urban.rural.classes <- subset(classes,filename==hrBase & type=="urban.rural")
    school.classes <- subset(classes,filename==hrBase & type=="school")

    missing.vals <- subset(ynm.classes,is.na(ynm))$value
    no.vals <- subset(ynm.classes,ynm==0)$value
    yes.vals <- subset(ynm.classes,ynm==1)$value

    missing.attended <- subset(attended.classes,is.na(attended))$value
    no.attended <- subset(attended.classes,attended==0)$value
    yes.attended <- subset(attended.classes,attended==1)$value

    missing.level <- subset(school.classes,is.na(level))$value
    none.level <- subset(school.classes,level=="none")$value
    preschool.level <- subset(school.classes,level=="preschool")$value
    primary.level <- subset(school.classes,level=="primary")$value
    secondary.level <- subset(school.classes,level=="secondary")$value
    higher.level <- subset(school.classes,level=="higher")$value

    #Rename wealth var
    if(typeof(hh$wlthscor)=="NULL" | typeof(hh$wlthscor)=="logical" | length(hh$wlthscor[which(!is.na(hh$wlthscor))])==0){
      if(typeof(hh$wscore)=="NULL" | typeof(hh$wscore)=="logical" | length(hh$wscore[which(!is.na(hh$wscore))])==0){
        message("Wealth missing!");return(NA)
      }else{
        names(hh)[which(names(hh)=="wscore")] <- "wealth"
      }
    }else{
      names(hh)[which(names(hh)=="wlthscor")] <- "wealth"
    }

    #Rename sample.weights var
    names(hh)[which(names(hh)=="hhweight")] <- "weights"

    #Rename urban var
    names(hh)[which(names(hh)=="hh6")] <- "urban.rural"
    if(typeof(hh$urban.rural)=="NULL"){message("No urban.rural!");hh$urban.rural<-NA;urban.missing<-TRUE}else{urban.missing<-FALSE}

    #Rename educ var
    names(hl)[which(names(hl)==attendedVar)] <- "attended"
    names(hl)[which(names(hl)==schoolVar)] <- "school"
    names(hl)[which(names(hl)==gradeVar)] <- "grade"

    #Rename age var
    names(hl)[which(names(hl)=="hl6")] <- "age"

    #Rename sex var
    names(hl)[which(names(hl)=="hl4")] <- "sex"

    #Rename head var
    hl$head <- tolower(substr(hl$hl3,1,4)) %in% c("chef","head")

    #Rename child vars
    names(ch)[which(names(ch)=="br1")] <- "birth.cert"
    names(ch)[which(names(ch)=="br2")] <- "birth.reg"
    names(ch)[which(names(ch)=="cage")] <- "age.months"
    names(ch)[which(names(ch)=="chweight")] <- "child.weights"
    names(ch)[which(names(ch)=="an3")] <- "weight.kg"
    names(ch)[which(names(ch)=="an4a")] <- "standing.lying"
    names(ch)[which(names(ch)=="haz2")] <- "child.height.age"

    #code female bmi
    if(typeof(wm$anw4)!="NULL" & typeof(wm$anw5)!="NULL"){
      wm$anw4[which(wm$anw4==99.9)] <- NA
      wm$anw5[which(wm$anw5==999.9)] <- NA
      wm$anw5 <- wm$anw5/100
      wm$woman.bmi <- wm$anw4/(wm$anw5*wm$anw5)
    }

    #Rename cluster/hh var
    names(hl)[which(names(hl)=="hh1")] <- "cluster"
    names(hl)[which(names(hl)=="hh2")] <- "household"
    names(hl)[which(names(hl)=="hl1")] <- "line"
    names(hl)[which(names(hl)=="ln")] <- "line"
    names(hh)[which(names(hh)=="hh1")] <- "cluster"
    names(hh)[which(names(hh)=="hh2")] <- "household"
    names(hh)[which(names(hh)=="hh7")] <- "region"
    names(ch)[which(names(ch)=="hh1")] <- "cluster"
    names(ch)[which(names(ch)=="hh2")] <- "household"
    names(ch)[which(names(ch)=="ln")] <- "line"
    names(ch)[which(names(ch)=="uf6")] <- "mother.line"
    names(wm)[which(names(wm)=="hh1")] <- "cluster"
    names(wm)[which(names(wm)=="hh2")] <- "household"
    names(wm)[which(names(wm)=="ln")] <- "line"

    recode.educ <- function(attendedV,schoolV,gradeV){
      educV <- c()
      for(i in 1:length(attendedV)){
        attended <- tolower(attendedV[i])
        school <- tolower(schoolV[i])
        if(length(school)<=0){
          school <- NA
        }
        grade <- gradeV[i]
        ###Ignore factor grades for now... We need to code these out in the metavars
        if(is.factor(grade)){
          grade <- NA
        }
        if(!is.na(grade)){
          if(grade>90){grade<-NA}
        }
        if(attended %in% missing.attended){
          if(school %in% missing.level){
            if(is.na(grade)){
              #missing all three
              educ <- NA
            }else{
              #missing attended and level, but not grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }else{
            #missing attended, but not level
            if(is.na(grade)){
              #has level, but not grade
              if(school %in% preschool.level | school %in% none.level){
                educ <- 0
              }else if(school %in% primary.level){
                educ <- 1
              }else if(school %in% secondary.level){
                educ <- 2
              }else if(school %in% higher.level){
                educ <- 3
              }else{
                educ <- NA
              }
            }else{
              #missing attended and level, but not grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }
        }else if(attended %in% no.attended){
          #No education
          educ <- 0
        }else{
          if(school %in% missing.level){
            if(is.na(grade)){
              #has attended, but has no level or grade
              educ <- NA
            }else{
              #has attended, missing level, but not missing grade
              if(grade>=5 & grade<7){
                educ <- 1
              }else if(grade>=7 & grade<9){
                educ <- 2
              }else if(grade>9){
                educ <- 3
              }else{
                educ <- 0
              }
            }
          }else if(school %in% preschool.level | school %in% none.level){
            if(is.na(grade)){
              educ <- 0
            }else if(grade>=5){
              #Complete primary
              educ <- 1
            }else{
              educ <- 0
            }
          } else if(school %in% primary.level){
            if(is.na(grade)){
              educ <- 0
            }else if(grade<5){
              #Incomplete primary
              educ <- 0
            }else if(grade>=5){
              #Complete primary
              educ <- 1
            }else{
              educ <- NA
            }
          } else if(school %in% secondary.level){
            #(in)complete secondary
            educ <- 2
          } else if(school %in% higher.level){
            #(in)complete higher
            educ <- 3
          }else if(grade>=5 & grade<7){
            educ <- 1
          }else if(grade>=7 & grade<9){
            educ <- 2
          }else if(grade>9){
            educ <- 3
          }else if(grade<5){
            #not at least 5 years of some other schooling
            educ <- 0
          } else{
            #missing grade with preschool, primary, or other
            educ <- NA
          }
        }
        educV <- c(educV,educ)
      }
      return(educV)
    }

    hl$educ <- recode.educ(hl$attended,hl$school,hl$grade)

    head <- subset(hl,head==1)
    names(head)[which(names(head)=="sex")] <- "head.sex"
    names(head)[which(names(head)=="age")] <- "head.age"
    keep <- c("cluster","household","head.sex","head.age")
    head <- head[keep]
    hh <- join(
      hh
      ,head
      ,by=c("cluster","household")
    )

    recode.urban.rural <- function(x){
      item <- subset(urban.rural.classes,value==tolower(x))
      if(nrow(item)==0){return(NA)}
      else{item$urban[1]}
    }
    hh$urban.rural <- sapply(hh$urban.rural,recode.urban.rural)

    povcalcut <- subset(povcalcuts,filename==hrBase)$hc
    np20cut <- 0.2
    nplcut <- subset(povcalcuts,filename==hrBase)$pl.hc
    extcut <- subset(povcalcuts,filename==hrBase)$extreme
    cuts <- c(povcalcut,np20cut,nplcut,extcut)
    povperc <- weighted.percentile(hh$wealth,hh$weights,prob=cuts)

    hh$p20 <- (hh$wealth < povperc[1])
    hh$np20 <- (hh$wealth < povperc[2])
    hh$npl <- (hh$wealth < povperc[3])
    hh$ext <- (hh$wealth < povperc[4])

    wmkeep <- c("household","cluster","line","woman.bmi")
    wmNames <- names(wm)
    namesDiff <- setdiff(wmkeep,wmNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        wm[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    wm <- wm[wmkeep]

    hl <- join(
      hl
      ,wm
      ,by=c("cluster","household","line")
    )

    names(wm) <- c("household","cluster","mother.line","mother.bmi")

    ch <- join(
      ch
      ,wm
      ,by=c("cluster","household","mother.line")
    )

    chkeep <- c("household","cluster","line","birth.cert","birth.reg","age.months","child.weights","weight.kg","standing.lying"
                ,"child.height.age","mother.bmi")
    chNames <- names(ch)
    namesDiff <- setdiff(chkeep,chNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        ch[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    ch <- ch[chkeep]

    hl <- join(
      hl
      ,ch
      ,by=c("cluster","household","line")
    )


    hhkeep <- c("wealth","weights","urban.rural","region","cluster","household","head.sex","head.age","p20","np20","npl","ext")
    hhNames <- names(hh)
    namesDiff <- setdiff(hhkeep,hhNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hh[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    hh <- hh[hhkeep]
    hl <- join(
      hl
      ,hh
      ,by=c("cluster","household")
    )
    hl <- data.frame(hl,as.is=TRUE,check.names=FALSE)
    keep <- c("wealth","weights","urban.rural","region","educ","age","sex","cluster","household","head.sex","head.age","p20"
              ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
              ,"woman.bmi","man.bmi","child.weights","mother.bmi","np20","npl","ext"
    )
    hlNames <- names(hl)
    namesDiff <- setdiff(keep,hlNames)
    if(length(namesDiff)>0){
      for(y in 1:length(namesDiff)){
        hl[namesDiff[y]] <- NA
        message(paste("Missing variable",namesDiff[y]))
      }
    }
    hl <- hl[keep]
    hl$filename <- hrBase
    dataList[[dataIndex]] <- hl
    dataIndex <- dataIndex + 1
  }
}

wd <- "F:/Documents/Data/MICSmeta"
setwd(wd)
# 
data.total <- rbindlist(dataList)
save(data.total,file="dataintermediate.RData")
load("dataintermediate.RData")
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
data.total$birth.reg.coded <- as.character(data.total$birth.reg)
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.registered)] <- "Yes"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & tolower(data.total$birth.cert) %in% birth.cert.not.registered)] <- "No"
data.total$birth.reg.coded[which(is.na(data.total$birth.reg.coded) & grepl("visto",data.total$birth.cert))] <- "Yes"

data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.missing)] <- NA
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.no)] <- 0
data.total$birth.reg.coded[which(tolower(data.total$birth.reg.coded) %in% birth.reg.yes)] <- 1
data.total$birth.reg.coded[which(substr(data.total$birth.reg.coded,1,1)=="S")] <- 1

data.total$birth.reg <- data.total$birth.reg.coded

data.total$birth.cert <- as.character(data.total$birth.cert)
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
data.total$stunting <- NA
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

# wd <- "D:/Documents/Data/BrazilSurvey/spss"
# setwd(wd)
# 
# load("PNDS2006_BR_DOM_PESS.RData")
# pr <- data.frame(dat,as.is=TRUE,check.names=FALSE)
# load("PNDS2006_BR_FILHOS.RData")
# ch <- data.frame(dat,as.is=TRUE,check.names=FALSE)
# 
# load("wealth.RData")
# dat <- dat[c("DOMICILIO_ID","wealth")]
# 
# pr <- pr[order(pr$DOMICILIO_ID),]
# dat <- dat[order(dat$DOMICILIO_ID),]
# wealth <- dat$wealth
# 
# pr <- cbind(pr,wealth)
# 
# names(pr)[which(names(pr)=="P000_NQUE")] <- "line"
# names(pr)[which(names(pr)=="CD002_CONG")] <- "cluster"
# names(pr)[which(names(pr)=="DOMICILIO_ID")] <- "household"
# 
# names(pr)[which(names(pr)=="CD008_SITU")] <- "urban.rural"
# pr$urban <- NA
# pr$urban[which(pr$urban.rural=="Urbano")] <- 1
# pr$urban[which(pr$urban.rural=="Rural")] <- 0
# 
# names(pr)[which(names(pr)=="XP999_PESO")] <- "weights"
# pr$weights <- pr$weights/10000
# 
# names(pr)[which(names(pr)=="P004_SEXO")] <- "sex"
# pr$gender <- NA
# pr$gender[which(pr$sex=="Masculino")] <- "Male"
# pr$gender[which(pr$sex=="Feminino")] <- "Female"
# pr$sex <- pr$gender
# pr$gender <- NULL
# names(pr)[which(names(pr)=="XP010_MELH")] <- "age"
# names(pr)[which(names(pr)=="P011B_GRAU")] <- "educ"
# 
# names(pr)[which(names(pr)=="P003_PARE")] <- "head"
# head <- subset(pr,head=="Responsável")
# names(head)[which(names(head)=="age")] <- "head.age"
# names(head)[which(names(head)=="sex")] <- "head.sex"
# keep <- c("household","head.age","head.sex")
# head <- head[keep]
# 
# pr <- join(
#   pr
#   ,head
#   ,by="household"
# )
# 
# povcalcut <- subset(povcalcuts,filename=="Brazil")$hc
# np20cut <- 0.2
# nplcut <- subset(povcalcuts,filename=="Brazil")$pl.hc
# extcut <- subset(povcalcuts,filename==hrBase)$extreme
# cuts <- c(povcalcut,np20cut,nplcut,extcut)
# povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
# 
# pr$p20 <- (pr$wealth < povperc[1])
# pr$np20 <- (pr$wealth < povperc[2])
# pr$npl <- (pr$wealth < povperc[3])
# pr$ext <- (pr$wealth < povperc[4])
# 
# codeAgeCat <- function(x){
#   startAge <- 0
#   ageDiff <- 4
#   endAge <- 4
#   if(is.na(x)){
#     return("missing")
#   }
#   while(startAge<95){
#     endAge <- startAge+ageDiff
#     if(x>=startAge & x<=endAge){
#       return(
#         paste0(startAge,"-",endAge)  
#       )
#     }
#     startAge <- endAge + 1
#   }
#   if(x>=95){
#     return("95+")
#   }
#   return("missing")
# }
# 
# pr$ageCategory <- vapply(pr$age,codeAgeCat,character(1))
# pr$ageCategory <- factor(pr$ageCategory,
#                          levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
#                                     ,"35-39","40-44","45-49","50-54","55-59","60-64"
#                                     ,"65-69","70-74","75-79","80-84","85-89","90-94"
#                                     ,"95+","missing")                          
# )
# 
# pr$head.ageCategory <- vapply(pr$head.age,codeAgeCat,character(1))
# pr$head.ageCategory <- factor(pr$head.ageCategory,
#                               levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
#                                          ,"35-39","40-44","45-49","50-54","55-59","60-64"
#                                          ,"65-69","70-74","75-79","80-84","85-89","90-94"
#                                          ,"95+","missing")                          
# )
# 
# names(ch)[which(names(ch)=="M241_LINH")] <- "ch.line"
# names(ch)[which(names(ch)=="M241Z_NQUE")] <- "line"
# names(ch)[which(names(ch)=="MULHER_ID")] <- "mother.id"
# ch$household <- substr(ch$mother.id,1,nchar(ch$mother.id)-2)
# names(ch)[which(names(ch)=="CM002_CONG")] <- "cluster"
# names(ch)[which(names(ch)=="XF110_PESO")] <- "weight.kg"
# ch$weight.kg[which(ch$weight.kg>=998)] <- NA
# names(ch)[which(names(ch)=="XF120_ALTU")] <- "height.cm"
# ch$height.cm[which(ch$height.cm>=998)] <- NA
# names(ch)[which(names(ch)=="XF100_MESE")] <- "age.months"
# ch$height.cm[which(ch$height.cm>=998)] <- NA
# names(ch)[which(names(ch)=="M847_DEIT")] <- "standing.lying"
# names(ch)[which(names(ch)=="XF310_INDI")] <- "child.height.age"
# names(ch)[which(names(ch)=="XF320_INDI")] <- "child.weight.age"
# 
# chKeep <- c("line","cluster","household","weight.kg"
#             ,"height.cm","age.months","standing.lying"
#             ,"child.height.age","child.weight.age")
# ch <- ch[chKeep]
# 
# pr <- cbind(pr,ch[match(pr$line,ch$line),])
# 
# pr$stunting <- NA
# # pr$stunting[which(pr$child.height.age<= (-6))] <- "Implausibly low"
# pr$stunting[which(pr$child.height.age > (-6) & pr$child.height.age<= (-3))] <- "Severely stunted"
# pr$stunting[which(pr$child.height.age > (-3) & pr$child.height.age<= (-2))] <- "Stunted, but not severely"
# pr$stunting[which(pr$child.height.age > (-2) & pr$child.height.age< (6))] <- "Not stunted"
# # pr$stunting[which(pr$child.height.age>= (6))] <- "Implausibly high"
# 
# pr$stunting <- factor(pr$stunting
#                       ,levels=c(
#                         "Implausibly low"
#                         ,"Severely stunted"
#                         ,"Stunted, but not severely"
#                         ,"Not stunted"
#                         ,"Implausibly high"
#                       ))
# 
# recode.educ <- function(x){
#   if(is.na(x)){return(NA)}
#   else if(x=="Sem resposta" | x=="Não sabe"){return(NA)}
#   else if(x=="Nenhum" | x=="Creche (não seriado)" | x=="Pré-escola (não seriado)"){return("No education, preschool")}
#   else if(x=="CA / Alfab adultos (não seriado)" | x=="EJA (não seriado)" | x=="Ensino fundamental (seriado)" | x=="Crianças especiais"){return("Primary")}
#   else if(x=="Supletivo ensino fundamental" | x=="Ensino médio"){return("Secondary")}
#   else{return("Higher")}
# }
# pr$educ <- sapply(pr$educ,recode.educ)
# pr$educ <- factor(pr$educ
#                   ,levels = c("No education, preschool","Primary","Secondary","Higher")
# )
# 
# keep <- c("wealth","weights","urban.rural","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
#           ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age","child.weight.age"
#           ,"woman.bmi","man.bmi","ageCategory","head.ageCategory","stunting","npl","np20","ext"
# )
# prNames <- names(pr)
# namesDiff <- setdiff(keep,prNames)
# if(length(namesDiff)>0){
#   for(y in 1:length(namesDiff)){
#     pr[namesDiff[y]] <- NA
#     message(paste("Missing variable",namesDiff[y]))
#   } 
# }
# data <- pr[keep]
# data$filename <- "Brazil"
# 
# brazil.data.total <- data
# data.total <- rbind(brazil.data.total,data.total,fill=TRUE)
# 
# wd <- "D:/Documents/Data/ChinaSurvey/"
# setwd(wd)
# 
# load("dat2012.RData")
# load("wealth.RData")
# 
# hr <- dat
# ir <- famros
# ch <- data.frame(child,as.is=TRUE,check.names=FALSE)
# 
# #Rename sample.weights var
# names(hr)[which(names(hr)=="fswt_natcs12")] <- "sample.weights"
# hr$weights <- hr$sample.weights/100000
# 
# #Rename urban var
# names(hr)[which(names(hr)=="urban12")] <- "urban.rural"
# recode.urban.rural <- function(x){
#   if(is.null(x)){return(NA)}
#   else if(is.na(x)){return(NA)}
#   else if(tolower(x)=="urban" | x==1){return(1)}
#   else if(tolower(x)=="rural" | x==2){return(0)}
#   else{return(NA)}
# }
# hr$urban <- sapply(hr$urban.rural,recode.urban.rural)
# 
# #Rename educ var
# names(ir)[which(names(ir)=="tb4_a12_p")] <- "educ"
# recode.educ <- function(x){
#   if(is.na(x)){return(NA)}
#   else if(tolower(x)=="na" | tolower(x)=="unknown"){return(NA)}
#   else if(tolower(x)=="illiterate/semi-literate"){return("No education, preschool")}
#   else if(tolower(x)=="primary school"){return("Primary")}
#   else if(tolower(x)=="junior high school"){return("Secondary")}
#   else{return("Higher")}
# }
# ir$educ <- sapply(ir$educ,recode.educ)
# ir$educ <- factor(ir$educ
#                   ,levels = c("No education, preschool","Primary","Secondary","Higher")
# )
# 
# #Rename age/sex var
# names(ir)[which(names(ir)=="tb1b_a_p")] <- "age"
# ir$age[which(ir$age<0)] <- NA
# names(ir)[which(names(ir)=="tb2_a_p")] <- "sex"
# ir$sex[which(ir$sex=="NA")] <- NA
# 
# #Registration
# names(ir)[which(names(ir)=="qa301_a12_p")] <- "birth.reg.raw"
# registered <- c("Agricultural","non-agricultural","Non-Chinese nationality")
# nonregistered <- c("NA","Unknown","No registration")
# ir$birth.reg <- NA
# ir$birth.reg[which(ir$birth.reg.raw %in% registered)] <- 1
# ir$birth.reg[which(ir$birth.reg.raw %in% nonregistered)] <- 0
# 
# #Weight and height
# famros.birthdays <- famros[c("pid","fid12","tb1y_a_p","tb1m_a_p")]
# ch <- data.frame(child,as.is=TRUE,check.names=FALSE)
# ch <- join(
#   ch
#   ,famros.birthdays
#   ,by=c("pid","fid12")
# )
# 
# code.age.months <- function(cyearV,cmonthV,byearV,bmonthV,ageV){
#   age.monthsV <- c()
#   for(i in 1:length(cyearV)){
#     cyear <- cyearV[i]
#     cmonth <- cmonthV[i]
#     byear <- byearV[i]
#     bmonth <- bmonthV[i]
#     age <- ageV[i]
#     if(is.na(bmonth)){
#       age.months <- age*12
#     }
#     else if(cmonth==bmonth){
#       age.months <- (cyear - byear)*12
#     }else if(cmonth>bmonth){
#       age.months <- (cyear - byear)*12 + (cmonth-bmonth)
#     }else if(cmonth<bmonth){
#       age.months <- ((cyear - byear) - 1)*12 + (12 - (bmonth-cmonth))
#     }
#     if(!is.na(age.months)){
#       if(age.months<0){
#         age.months <- 0
#       } 
#     }
#     age.monthsV <- c(age.monthsV,age.months)
#   }
#   return(age.monthsV)
# }
# ch$tb1m_a_p[which(ch$tb1m_a_p<0)] <- NA
# ch$cfps2012_age[which(ch$cfps2012_age<0)] <- NA
# ch$age.months <- code.age.months(ch$cyear,ch$cmonth,ch$cfps2012_birthy_best,ch$tb1m_a_p,ch$cfps2012_age)
# 
# names(ch)[which(names(ch)=="wa103")] <- "weight.kg"
# ch$weight.kg[which(ch$weight.kg<0)] <- NA
# ch$weight.kg <- ch$weight.kg/2
# names(ch)[which(names(ch)=="wa104")] <- "height.cm"
# ch$height.cm[which(ch$height.cm<0)] <- NA
# ch <- subset(ch,age.months<=60)
# names(ch)[which(names(ch)=="cfps2012_gender")] <- "gender"
# ch$gender <- unfactor(ch$gender)
# ch$gender[which(ch$gender=="NA")] <- NA
# ch$gender[which(ch$gender=="Male")] <- 1
# ch$gender[which(ch$gender=="Female")] <- 2
# names(ch)[which(names(ch)=="rswt_natcs12")] <- "weights"
# ch$weights <- ch$weights/100000
# names(ch)[which(names(ch)=="cid")] <- "cluster"
# names(ch)[which(names(ch)=="fid12")] <- "household"
# ch <- ch[complete.cases(ch[c("weight.kg","height.cm","age.months","gender","weights")]),]
# keep <- c("cluster","household","pid","weight.kg","height.cm","age.months","gender","weights")
# ch <- ch[keep]
# 
# igu.dir <- "D:/Documents/igrowup_R/"
# weianthro<-read.table(paste0(igu.dir,"/weianthro.txt"),header=T,sep="",skip=0)
# lenanthro<-read.table(paste0(igu.dir,"/lenanthro.txt"),header=T,sep="",skip=0)
# bmianthro<-read.table(paste0(igu.dir,"/bmianthro.txt"),header=T,sep="",skip=0)
# hcanthro<-read.table(paste0(igu.dir,"/hcanthro.txt"),header=T,sep="",skip=0)
# acanthro<-read.table(paste0(igu.dir,"/acanthro.txt"),header=T,sep="",skip=0)
# ssanthro<-read.table(paste0(igu.dir,"/ssanthro.txt"),header=T,sep="",skip=0)
# tsanthro<-read.table(paste0(igu.dir,"/tsanthro.txt"),header=T,sep="",skip=0)
# wflanthro<-read.table(paste0(igu.dir,"/wflanthro.txt"),header=T,sep="",skip=0)
# wfhanthro<-read.table(paste0(igu.dir,"/wfhanthro.txt"),header=T,sep="",skip=0)
# source(paste0(igu.dir,"igrowup_standard.r"))
# source(paste0(igu.dir,"igrowup_restricted.r"))
# igrowup.restricted(FileLab="ch",FilePath=igu.dir,
#                    mydf=ch, sex=gender
#                    , age=age.months, age.month=TRUE
#                    , weight=weight.kg
#                    , lenhei=height.cm
#                    , sw=weights)
# 
# zscores <- read.csv(paste0(igu.dir,"ch_z_rc.csv"))
# zscores$standing.lying <- NA
# zscoreKeep <- c("cluster","household","pid","weight.kg","height.cm","age.months","standing.lying","zlen","zwei")
# zscores <- zscores[zscoreKeep]
# names(zscores)[which(names(zscores)=="zlen")] <- "child.height.age"
# names(zscores)[which(names(zscores)=="zwei")] <- "child.weight.age"
# 
# #Rename cluster/hh var
# # names(hr)[which(names(hr)=="provcd")] <- "province"
# # names(hr)[which(names(hr)=="countyid")] <- "county"
# names(ir)[which(names(ir)=="fid12")] <- "household"
# names(hr)[which(names(hr)=="cid")] <- "cluster"
# names(hr)[which(names(hr)=="fid12")] <- "household"
# 
# #Household head
# hh.heads <- unique(ir$tf10pid)
# head <- subset(ir,pid %in% hh.heads)
# names(head)[which(names(head)=="age")] <- "head.age"
# names(head)[which(names(head)=="sex")] <- "head.sex"
# keep <- c("household","head.age","head.sex")
# head <- head[keep]
# 
# ir <- join(
#   ir
#   ,head
#   ,by=c("household")
# )
# 
# keep <- c("cluster","household","wealth","weights","urban")
# hr <- hr[keep]
# 
# ir <- join(
#   ir
#   ,hr
#   ,by=c("household")
# )
# 
# povcalcut <- subset(povcalcuts,filename=="China")$hc
# np20cut <- 0.2
# nplcut <- subset(povcalcuts,filename=="China")$pl.hc
# extcut <- subset(povcalcuts,filename=="China")$extreme
# cuts <- c(povcalcut,np20cut,nplcut,extcut)
# povperc <- weighted.percentile(ir$wealth,ir$weights,prob=cuts)
# 
# ir$p20 <- (ir$wealth < povperc[1])
# ir$np20 <- (ir$wealth < povperc[2])
# ir$npl <- (ir$wealth < povperc[3])
# ir$ext <- (ir$wealth < povperc[4])
# 
# ir <- join(
#   ir
#   ,zscores
#   ,by=c("cluster","household","pid")
# )
# 
# keep <- c("wealth","weights","urban","educ","age","sex","cluster","household","head.sex","head.age","p20"
#           ,"birth.cert","birth.reg","age.months","weight.kg","height.cm","standing.lying","child.height.age"
#           ,"woman.bmi","man.bmi","np20","npl","ext"
# )
# irNames <- names(ir)
# namesDiff <- setdiff(keep,irNames)
# if(length(namesDiff)>0){
#   for(y in 1:length(namesDiff)){
#     ir[namesDiff[y]] <- NA
#     message(paste("Missing variable",namesDiff[y]))
#   } 
# }
# ir <- ir[keep]
# 
# codeAgeCat <- function(x){
#   startAge <- 0
#   ageDiff <- 4
#   endAge <- 4
#   if(is.na(x)){
#     return("missing")
#   }
#   while(startAge<95){
#     endAge <- startAge+ageDiff
#     if(x>=startAge & x<=endAge){
#       return(
#         paste0(startAge,"-",endAge)  
#       )
#     }
#     startAge <- endAge + 1
#   }
#   if(x>=95){
#     return("95+")
#   }
#   return("missing")
# }
# 
# ir$ageCategory <- vapply(ir$age,codeAgeCat,character(1))
# ir$ageCategory <- factor(ir$ageCategory,
#                          levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
#                                     ,"35-39","40-44","45-49","50-54","55-59","60-64"
#                                     ,"65-69","70-74","75-79","80-84","85-89","90-94"
#                                     ,"95+","missing")                          
# )
# 
# ir$head.ageCategory <- vapply(ir$head.age,codeAgeCat,character(1))
# ir$head.ageCategory <- factor(ir$head.ageCategory,
#                               levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
#                                          ,"35-39","40-44","45-49","50-54","55-59","60-64"
#                                          ,"65-69","70-74","75-79","80-84","85-89","90-94"
#                                          ,"95+","missing")                          
# )
# 
# #Not really stunting, do we want to just call this "nutrition"?
# ir$stunting <- NA
# # ir$stunting[which(ir$child.height.age<= (-6))] <- "Implausibly low"
# ir$stunting[which(ir$child.height.age > (-6) & ir$child.height.age<= (-3))] <- "Severely stunted"
# ir$stunting[which(ir$child.height.age > (-3) & ir$child.height.age<= (-2))] <- "Stunted, but not severely"
# ir$stunting[which(ir$child.height.age > (-2) & ir$child.height.age< (6))] <- "Not stunted"
# # ir$stunting[which(ir$child.height.age>= (6))] <- "Implausibly high"
# 
# ir$stunting <- factor(ir$stunting
#                       ,levels=c(
#                         "Implausibly low"
#                         ,"Severely stunted"
#                         ,"Stunted, but not severely"
#                         ,"Not stunted"
#                         ,"Implausibly high"
#                       ))
# ir$filename <- "China"
# china.data.total <- ir

setwd("F:/Documents/Data/MICSmeta/")
load("br.data.total.RData")
data.total <- rbind(br.data.total,data.total,fill=TRUE)
save(data.total, file="data.total.intermediate2.RData")

# load("child.maternal.RData")
# setnames(child.health,"skilled.attendant","ch.skilled.attendant")
# valid.vacc <- c(TRUE,"TRUE","Yes","Oui","Sí")
# no.vacc <- c(FALSE,"FALSE","No","Non","No sabe")
# missing.vacc <- c("DK",NA,"Missing","NSP","Manquant")
# recode.vacc <- function(x){
#   if(is.na(x)){return(NA)}
#   else if(x %in% valid.vacc){return(1)}
#   else if(x %in% no.vacc){return(0)}
#   else if(x %in% missing.vacc){return(NA)}
#   return(NA)
# }
# child.health$vacc <- sapply(child.health$any.vacc,recode.vacc)
# child.health.tab <- child.health[,.(
#   ch.skilled.attendant=sum(ch.skilled.attendant==TRUE,na.rm=TRUE)>=1
#   ,any.vacc=sum(vacc,na.rm=TRUE)>=1
# ),by=.(filename,cluster,household)]
# maternal.health.tab <- maternal.health[,.(
#   ceb=sum(ceb,na.rm=TRUE)
#   ,cdead=sum(cdead,na.rm=TRUE)
#   ,skilled.attendant=sum(skilled.attendant,na.rm=TRUE)>=1
#   ,maternal.deaths=sum(maternal.deaths,na.rm=TRUE)
# ),by=.(filename,cluster,household)]
# data.total <- join(data.total,child.health.tab,by=c("filename","cluster","household"))
# data.total <- join(data.total,maternal.health.tab,by=c("filename","cluster","household"))

wd <- "F:/Documents/Data/P20_2013/meta"
setwd(wd)

save(data.total,file="HLPF2017.RData")
library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(descr)
library(WDI)
library(varhandle)
require(zoo)
# 
# # Stop crosstab from plotting everything
 options(descr.plot = FALSE)

setwd("F:/Documents/Data/P20_2013/meta")
load("HLPF2017.RData")

data.total$sex <- factor(data.total$sex,levels=c("Male","Female"))
# 
# data.total$whipple <- substr(as.character(data.total$age),nchar(as.character(data.total$age)),nchar(as.character(data.total$age)))
# data.total$whipple05 <- data.total$whipple %in% c("0","5")
# mean(data.total$whipple05,na.rm=TRUE)*500
# 
# p20 <- subset(data.total,p20==TRUE & age>=15 & age<=49)
# mean(p20$whipple05,na.rm=TRUE)*500
# nonp20 <- subset(data.total,p20==FALSE  & age>=15 & age<=49)
# mean(nonp20$whipple05,na.rm=TRUE)*500
# 
# children <- subset(data.total,!is.na(age.months))
# children$whipple <- substr(as.character(children$age.months),nchar(as.character(children$age.months)),nchar(as.character(children$age.months)))
# children$whipple06 <- children$whipple %in% c("0","6")
# mean(children$whipple06,na.rm=TRUE)*500
# p20.kids <- subset(children,p20==TRUE)
# mean(p20.kids$whipple06,na.rm=TRUE)*500
# nonp20.kids <- subset(children,p20==FALSE)
# mean(nonp20.kids$whipple06,na.rm=TRUE)*500

pop.confidence <- function(x,y,w,pop){
  ct <- crosstab(x,y,weight=w,prop.t=TRUE,drop.levels=FALSE)
  props <- ct$prop.tbl
  cv <- sd(w,na.rm=TRUE)/mean(w,na.rm=TRUE)
  deft <- cv*cv+1
  n <- ct$total.n
  SEs <- sqrt(((1-(n/pop))/n)*(pop/(pop-1))*(props*(1-props)))
  corrected.SEs <- SEs*deft
  low.end <- (props-(2*corrected.SEs))*pop
  low.end <- pmax(low.end,0)
  estimate.point <- props*pop
  high.end <- (props+(2*corrected.SEs))*pop
  high.end <- pmin(high.end,pop)
  return(
    list(
      low = low.end
      ,estimate = estimate.point
      ,high = high.end
    )
  )
}

countryMeta = povcalcuts
# countryMeta <- read.csv("headcounts.csv",as.is=TRUE)
# povcalcuts$filename[which(povcalcuts$filename=="bdhr70dt")] <- "bdhr61dt"
# povcalcuts$filename[which(povcalcuts$filename=="bfhr70dt")] <- "bfhr62dt"
# povcalcuts$filename[which(povcalcuts$filename=="idhr63dt")] <- "idhr51dt"
# povcalcuts$filename[which(povcalcuts$filename=="kehr7hdt")] <- "kehr70dt"
# povcalcuts$filename[which(povcalcuts$filename=="mdhr6hdt")] <- "mdhr51dt"
# povcalcuts$filename[which(povcalcuts$filename=="mwhr71dt")] <- "mwhr7hdt"
# povcalcuts$filename[which(povcalcuts$filename=="phhr61dt")] <- "phhr52dt"
# # povcalcuts$filename[which(povcalcuts$filename=="tzhr6adt")] <- "tzhr63dt"
# povcalcuts$filename[which(povcalcuts$filename=="ughr72dt")] <- "ughr60dt"

indicator <- "SI.POV.NAHC"

dat <- WDI(country = "all", 
           indicator = indicator, 
           start = 1990, 
           end = 2016,
           extra = TRUE
           #cache = new_cache
)

dat <- dat[c("iso3c","year","SI.POV.NAHC")]
names(dat) <- c("iso3","year","hc")
dat$hc <- dat$hc/100
dat <- dat[order(dat$year),]
dat <- dat[order(dat$iso3),]
colname <- "hc"
dat <- ddply(dat,.(iso3),function(x)
{
  naLen <- nrow(x[which(is.na(x[,colname])),])
  allLen <- nrow(x)
  valueLen <- allLen-naLen
  ival <- x[,colname]
  x[,paste("original",colname,sep="-")] <- ival 
  if(valueLen>=2)
  {
    interpVals <- na.approx(x[,colname],na.rm=FALSE,rule=2)
  }
  else if(valueLen==1){
    interpVals <- rep(sum(x[,colname],na.rm=TRUE),allLen)
  }
  else{
    interpVals <- rep(NA,allLen)
  }
  x[,colname] <- interpVals
  return(x)
}
)
names(dat) <- c("iso3","year","pl.hc","pl.hc.original")
countryMeta <- join(countryMeta,dat,by=c("iso3","year"))

newNames <- c("p20.rural"
              ,"p20.urban"
              ,"p80.rural"
              ,"p80.urban"
              ,"p80.over25.noeduc"
              ,"p80.over25.primary"
              ,"p80.over25.secondary"
              ,"p80.over25.higher"
              ,"p20.over25.noeduc"
              ,"p20.over25.primary"
              ,"p20.over25.secondary"
              ,"p20.over25.higher"
              ,"p80.o25.m.noeduc"
              ,"p80.o25.m.primary"
              ,"p80.o25.m.secondary"
              ,"p80.o25.m.higher"
              ,"p20.o25.m.noeduc"
              ,"p20.o25.m.primary"
              ,"p20.o25.m.secondary"
              ,"p20.o25.m.higher"
              ,"p80.o25.f.noeduc"
              ,"p80.o25.f.primary"
              ,"p80.o25.f.secondary"
              ,"p80.o25.f.higher"
              ,"p20.o25.f.noeduc"
              ,"p20.o25.f.primary"
              ,"p20.o25.f.secondary"
              ,"p20.o25.f.higher"
              ,"p20.male"
              ,"p20.female"
              ,"p80.male"
              ,"p80.female"
              ,"p20.male.head"
              ,"p20.female.head"
              ,"p80.male.head"
              ,"p80.female.head"
              ,"p80.unregistered"
              ,"p80.registered"
              ,"p20.unregistered"
              ,"p20.registered"
              ,"p80.notstunted"
              ,"p80.stunted"
              ,"p20.notstunted"
              ,"p20.stunted"
              ,"ext.rural"
              ,"ext.urban"
              ,"n.ext.rural"
              ,"n.ext.urban"
              ,"n.ext.over25.noeduc"
              ,"n.ext.over25.primary"
              ,"n.ext.over25.secondary"
              ,"n.ext.over25.higher"
              ,"ext.over25.noeduc"
              ,"ext.over25.primary"
              ,"ext.over25.secondary"
              ,"ext.over25.higher"
              ,"ext.male"
              ,"ext.female"
              ,"n.ext.male"
              ,"n.ext.female"
              ,"ext.male.head"
              ,"ext.female.head"
              ,"n.ext.male.head"
              ,"n.ext.female.head"
              ,"n.ext.unregistered"
              ,"n.ext.registered"
              ,"ext.unregistered"
              ,"ext.registered"
              ,"n.ext.notstunted"
              ,"n.ext.stunted"
              ,"ext.notstunted"
              ,"ext.stunted"
              ,"p20.male.stunted"
              ,"p20.female.stunted"
              ,"p20.male.notstunted"
              ,"p20.female.notstunted"
              ,"p80.male.stunted"
              ,"p80.female.stunted"
              ,"p80.male.notstunted"
              ,"p80.female.notstunted"
              ,"p80.maternal.deaths"
              ,"p80.no.maternal.deaths"
              ,"p20.maternal.deaths"
              ,"p20.no.maternal.deaths"
              ,"male.stunted"
              ,"female.stunted"
              ,"male.notstunted"
              ,"female.notstunted"
              ,"surveyed.pop"
              ,"surveyed.households"
              ,"surveyed.men"
              ,"surveyed.women"
              ,"P20.0-4"
              ,"P20.5-9"
              ,"P20.10-14"
              ,"P20.15-19"
              ,"P20.20-24"
              ,"P20.25-29"
              ,"P20.30-34"
              ,"P20.35-39"
              ,"P20.40-44"
              ,"P20.45-49"
              ,"P20.49-54"
              ,"P20.55-59"
              ,"P20.60-64"
              ,"P20.65-69"
              ,"P20.70-74"
              ,"P20.75-79"
              ,"P20.80-84"
              ,"P20.85-90"
              ,"P20.90-95"
              ,"P20.95+"
              ,"P80.0-4"
              ,"P80.5-9"
              ,"P80.10-14"
              ,"P80.15-19"
              ,"P80.20-24"
              ,"P80.25-29"
              ,"P80.30-34"
              ,"P80.35-39"
              ,"P80.40-44"
              ,"P80.45-49"
              ,"P80.49-54"
              ,"P80.55-59"
              ,"P80.60-64"
              ,"P80.65-69"
              ,"P80.70-74"
              ,"P80.75-79"
              ,"P80.80-84"
              ,"P80.85-90"
              ,"P80.90-95"
              ,"P80.95+"

            
)

for(i in 1:length(newNames)){
  countryMeta[[newNames[i]]] <- NA
}

filenames <- countryMeta$filename
for(i in 1:length(filenames)){
  this.filename <- filenames[i]
  message(this.filename)
  dat <- subset(data.total,filename==this.filename)
  surveyed.pop <- nrow(dat)
  surveyed.households <- length(unique(dat$household))
  under5 <- subset(dat,age<5)
  over5 <- subset(dat,age>=5)
  under15 <- subset(dat,age<15)
  over15 <- subset(dat,age>=15)
  over25 <- subset(dat,age>=25)
  
  over25.male <- subset(over25,sex=="Male")
  over25.female <- subset(over25,sex=="Female")
  women <- subset(dat,sex=="Female")
  men <- subset(dat,sex=="Male")
  surveyed.pop <- nrow(dat)
  countryMeta$surveyed.pop[which(countryMeta$filename==this.filename)] <- surveyed.pop
  surveyed.households <- nrow(unique(data.frame(dat)[c("cluster","household")]))
  countryMeta$surveyed.households[which(countryMeta$filename==this.filename)] <- surveyed.households
  surveyed.men <- nrow(men)
  countryMeta$surveyed.men[which(countryMeta$filename==this.filename)] <- surveyed.men
  surveyed.women <- nrow(women)
  countryMeta$surveyed.women[which(countryMeta$filename==this.filename)] <- surveyed.women
  if(nrow(dat)>0){
    this.pop <- subset(countryMeta,filename==this.filename)$pop.total
    this.pop.under5.male <- subset(countryMeta,filename==this.filename)$male.under5
    this.pop.under5.female <- subset(countryMeta,filename==this.filename)$female.under5
    this.pop.under5 <- this.pop.under5.female + this.pop.under5.male
    this.pop.over5 <- this.pop - this.pop.under5
    this.pop.under15 <- this.pop.under5 + subset(countryMeta,filename==this.filename)$female.5.14 +
      subset(countryMeta,filename==this.filename)$male.5.14
    this.pop.over15 <- this.pop - this.pop.under15
    this.pop.female <- subset(countryMeta,filename==this.filename)$pop.female
    this.pop.male <- subset(countryMeta,filename==this.filename)$pop.male
    this.pop.over25.male <- subset(countryMeta,filename==this.filename)$male.25.plus
    this.pop.over25.female <- subset(countryMeta,filename==this.filename)$female.25.plus
    this.pop.over25 <- this.pop.over25.male + this.pop.over25.female
    #Urban-P20
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$p20,dat$weights,this.pop)
      countryMeta$p80.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Urban-ext
    if(length(dat$urban[which(!is.na(dat$urban))])!=0){
      confidence.tab <- pop.confidence(dat$urban,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$n.ext.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$ext.rural[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$ext.urban[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})
    }
    #Educ-P20
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$p20,over25$weights,this.pop.over25)
      countryMeta$p80.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Educ-ext
    if(length(over25$educ[which(!is.na(over25$educ))])!=0){
      confidence.tab <- pop.confidence(over25$educ,over25$ext,over25$weights,this.pop.over25)
      countryMeta$n.ext.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$n.ext.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$ext.over25.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$ext.over25.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    #Sex-P20
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Sex-ext
    if(length(dat$sex[which(!is.na(dat$sex))])!=0){
      confidence.tab <- pop.confidence(dat$sex,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$n.ext.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$ext.male[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$ext.female[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-P20
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$p20,dat$weights,this.pop)
      countryMeta$p80.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$p80.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$p20.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$p20.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Head-sex-ext
    if(length(dat$head.sex[which(!is.na(dat$head.sex))])!=0){
      confidence.tab <- pop.confidence(dat$head.sex,dat$ext,dat$weights,this.pop)
      countryMeta$n.ext.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","FALSE"]},error=function(e){0})
      countryMeta$n.ext.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","FALSE"]},error=function(e){0})
      countryMeta$ext.male.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Male","TRUE"]},error=function(e){0})
      countryMeta$ext.female.head[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Female","TRUE"]},error=function(e){0}) 
    }
    #Maternal deaths
    dat$maternal <- dat$maternal.deaths > 0 
    if(length(dat$maternal[which(!is.na(dat$maternal))])!=0){
      confidence.tab <- pop.confidence(dat$maternal,dat$p20,dat$weights,this.pop)
      countryMeta$p80.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p80.no.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p20.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})
      countryMeta$p20.no.maternal.deaths[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0}) 
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$p80.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$p20.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$p20.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 registration
    if(length(under5$birth.reg[which(!is.na(under5$birth.reg))])!=0){
      confidence.tab <- pop.confidence(under5$birth.reg,under5$ext,under5$weights,this.pop.under5)
      countryMeta$n.ext.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","FALSE"]},error=function(e){0})
      countryMeta$n.ext.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","FALSE"]},error=function(e){0})
      countryMeta$ext.unregistered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0","TRUE"]},error=function(e){0})
      countryMeta$ext.registered[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["1","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition p20
    under5$stunted <- (under5$child.height.age <= -2) & (under5$child.height.age > -6)
    under5$stunted[which(is.na(under5$stunting))] <- NA
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$p20,under5$weights,this.pop.under5)
      countryMeta$p80.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition ext
    if(length(under5$stunted[which(!is.na(under5$stunted))])!=0){
      confidence.tab <- pop.confidence(under5$stunted,under5$ext,under5$weights,this.pop.under5)
      countryMeta$n.ext.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$n.ext.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$ext.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$ext.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Under5 nutrition by gender
    under5.male <- subset(under5,sex=="Male")
    under5.female <- subset(under5,sex=="Female")
    if(length(under5.male$stunted[which(!is.na(under5.male$stunted))])!=0){
      confidence.tab <- pop.confidence(under5.male$stunted,under5.male$p20,under5.male$weights,this.pop.under5.male)
      countryMeta$p80.male.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.male.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.male.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.male.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0}) 
    }
    if(length(under5.female$stunted[which(!is.na(under5.female$stunted))])!=0){
      confidence.tab <- pop.confidence(under5.female$stunted,under5.female$p20,under5.female$weights,this.pop.under5.female)
      countryMeta$p80.female.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","FALSE"]},error=function(e){0})
      countryMeta$p80.female.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","FALSE"]},error=function(e){0})
      countryMeta$p20.female.notstunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["FALSE","TRUE"]},error=function(e){0})
      countryMeta$p20.female.stunted[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["TRUE","TRUE"]},error=function(e){0})  
    }
    #Age distribution
    # if(length(countryMeta$ageCategory[which(!is.na(countryMeta$AgeCategory))])!=0){
    #   confidence.tab <- pop.confidence(countryMeta$AgeCategory,over25$p20,over25$weights,this.pop.over25)
    #   countryMeta$p80.0to4[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0-4","FALSE"]},error=function(e){0})
    #   countryMeta$p80.5to9[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["5-9","FALSE"]},error=function(e){0})
    #   countryMeta$p80.10to14[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["10-14","FALSE"]},error=function(e){0})
    #   countryMeta$p80.15to19[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["15-19","FALSE"]},error=function(e){0})
    #   countryMeta$p80.20to24[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["20-24","FALSE"]},error=function(e){0})
    #   countryMeta$p80.25to29[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["25-29","FALSE"]},error=function(e){0})
    #   countryMeta$p80.30to34[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["30-34","FALSE"]},error=function(e){0})
    #   countryMeta$p80.35to39[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["35-39","FALSE"]},error=function(e){0})
    #   countryMeta$p80.40to44[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["40-44","FALSE"]},error=function(e){0})
    #   countryMeta$p80.45to49[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["45-49","FALSE"]},error=function(e){0})
    #   countryMeta$p80.50to54[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["50-54","FALSE"]},error=function(e){0})
    #   countryMeta$p80.55to59[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["55-59","FALSE"]},error=function(e){0})
    #   countryMeta$p80.60to64[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["60-64","FALSE"]},error=function(e){0})
    #   countryMeta$p80.65to69[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["65-69","FALSE"]},error=function(e){0})
    #   countryMeta$p80.70to74[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["70-74","FALSE"]},error=function(e){0})
    #   countryMeta$p80.75to79[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["75-79","FALSE"]},error=function(e){0})
    #   countryMeta$p80.80to84[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["80-84","FALSE"]},error=function(e){0})
    #   countryMeta$p80.85to89[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["85-89","FALSE"]},error=function(e){0})
    #   countryMeta$p80.90to94[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["90-94","FALSE"]},error=function(e){0})
    #   countryMeta$p80.95plus[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["95+","FALSE"]},error=function(e){0})
    #   countryMeta$p20.0to4[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["0-4","TRUE"]},error=function(e){0})
    #   countryMeta$p20.5to9[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["5-9","TRUE"]},error=function(e){0})
    #   countryMeta$p20.10to14[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["10-14","TRUE"]},error=function(e){0})
    #   countryMeta$p20.15to19[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["15-19","TRUE"]},error=function(e){0})
    #   countryMeta$p20.20to24[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["20-24","TRUE"]},error=function(e){0})
    #   countryMeta$p20.25to29[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["25-29","TRUE"]},error=function(e){0})
    #   countryMeta$p20.30to34[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["30-34","TRUE"]},error=function(e){0})
    #   countryMeta$p20.35to39[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["35-39","TRUE"]},error=function(e){0})
    #   countryMeta$p20.40to44[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["40-44","TRUE"]},error=function(e){0})
    #   countryMeta$p20.45to49[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["45-49","TRUE"]},error=function(e){0})
    #   countryMeta$p20.50to54[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["50-54","TRUE"]},error=function(e){0})
    #   countryMeta$p20.55to59[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["55-59","TRUE"]},error=function(e){0})
    #   countryMeta$p20.60to64[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["60-64","TRUE"]},error=function(e){0})
    #   countryMeta$p20.65to69[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["65-69","TRUE"]},error=function(e){0})
    #   countryMeta$p20.70to74[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["70-74","TRUE"]},error=function(e){0})
    #   countryMeta$p20.75to79[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["75-79","TRUE"]},error=function(e){0})
    #   countryMeta$p20.80to84[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["80-84","TRUE"]},error=function(e){0})
    #   countryMeta$p20.85to89[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["85-89","TRUE"]},error=function(e){0})
    #   countryMeta$p20.90to94[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["90-94","TRUE"]},error=function(e){0})
    #   countryMeta$p20.95plus[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["95+","TRUE"]},error=function(e){0})
    # }
    #Educ-P20 by gender
    if(length(over25$educ[which(!is.na(over25.male$educ))])!=0){
      confidence.tab <- pop.confidence(over25.male$educ,over25.male$p20,over25.male$weights,this.pop.over25.male)
      countryMeta$p80.o25.m.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.m.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.o25.m.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.m.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
    if(length(over25$educ[which(!is.na(over25.female$educ))])!=0){
      confidence.tab <- pop.confidence(over25.female$educ,over25.female$p20,over25.female$weights,this.pop.over25.female)
      countryMeta$p80.o25.f.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","FALSE"]},error=function(e){0})
      countryMeta$p80.o25.f.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","FALSE"]},error=function(e){0})
      countryMeta$p20.o25.f.noeduc[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["No education, preschool","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.primary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Primary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.secondary[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Secondary","TRUE"]},error=function(e){0})
      countryMeta$p20.o25.f.higher[which(countryMeta$filename==this.filename)] <- tryCatch({confidence.tab$estimate["Higher","TRUE"]},error=function(e){0})
    }
  }
}

countryMeta$female.notstunted <- countryMeta$p80.female.notstunted+countryMeta$p20.female.notstunted
countryMeta$female.stunted <- countryMeta$p80.female.stunted+countryMeta$p20.female.stunted
countryMeta$male.notstunted <- countryMeta$p80.male.notstunted+countryMeta$p20.male.notstunted
countryMeta$male.stunted <- countryMeta$p80.male.stunted+countryMeta$p20.male.stunted

# write.csv(countryMeta,"bycountry_tabs_201321June2017.csv",row.names=FALSE,na="")
# 
# 
# 
# 
# bycountry_tabs_2013 <- read_csv("bycountry_tabs_201321June2017.csv")
# bycountry_tabs_Nepal <- read_csv("F:/Documents/Data/P20_2013/meta/bycountry_tabs_2013.csv")
# bycountry_tabs_Nepal <- subset(bycountry_tabs_Nepal, iso3=="NPL")
# bycountry_tabs_2013=rbindlist(list(bycountry_tabs_2013,bycountry_tabs_Nepal),fill=TRUE)


dat = countryMeta
dat$urbanHC=dat$p20.urban/(dat$p20.urban+dat$p80.urban)
dat$ruralHC=dat$p20.rural/(dat$p20.rural+dat$p80.rural)
dat$p20.over25.rate=(dat$p20.over25.noeduc+dat$p20.over25.primary+dat$p20.over25.secondary+dat$p20.over25.higher)/(dat$p20.rural+dat$p20.urban)
dat$p80.over25.rate=(dat$p80.over25.noeduc+dat$p80.over25.primary+dat$p80.over25.secondary+dat$p80.over25.higher)/(dat$p80.rural+dat$p80.urban)
dat$p20.o25.f.noed.rate=dat$p20.o25.f.noeduc/(dat$p20.o25.f.noeduc+dat$p20.o25.f.primary+dat$p20.o25.f.secondary+dat$p20.o25.f.higher)
dat$p20.o25.m.noed.rate=dat$p20.o25.m.noeduc/(dat$p20.o25.m.noeduc+dat$p20.o25.m.primary+dat$p20.o25.m.secondary+dat$p20.o25.m.higher)
dat$p80.o25.f.noed.rate=dat$p80.o25.f.noeduc/(dat$p80.o25.f.noeduc+dat$p80.o25.f.primary+dat$p80.o25.f.secondary+dat$p80.o25.f.higher)
dat$p80.o25.m.noed.rate=dat$p80.o25.m.noeduc/(dat$p80.o25.m.noeduc+dat$p80.o25.m.primary+dat$p80.o25.m.secondary+dat$p80.o25.m.higher)
dat$P20.stunting.rate=dat$p20.stunted/(dat$p20.stunted+dat$p20.notstunted)
dat$P80.stunting.rate=dat$p80.stunted/(dat$p80.stunted+dat$p80.notstunted)
dat$p20.birth.reg.rate=dat$p20.registered/(dat$p20.registered+dat$p20.unregistered)
dat$p80.birth.reg.rate=dat$p80.registered/(dat$p80.registered+dat$p80.unregistered)
# HLPF = subset(dat, iso3=="BGD"|iso3=="NPL"|iso3=="NGA"|iso3=="KEN"|iso3=="BRA") 

keep = c("iso3","hc","extreme","pop.total","urbanHC", "ruralHC", "p20.over25.rate",  "p80.over25.rate" , "p20.o25.f.noed.rate"
, "p20.o25.m.noed.rate"  ,  "p80.o25.f.noed.rate" ,"p80.o25.m.noed.rate"
, "P20.stunting.rate"  , "P80.stunting.rate"  , "p20.birth.reg.rate" 
  , "p80.birth.reg.rate" )
dat=dat[keep]
write.csv(dat,"~/Poverty data/Gates Poverty work/HLPF country data.csv", row.names=FALSE, na="")