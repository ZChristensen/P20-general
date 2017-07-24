setwd("C:/Users/Zach/Documents/GNR 2017")
load("total_triple.RData")
library(data.table)
library(Hmisc)

data.total$male.overweight = data.total$man.bmi>=25
data.total$male.obese = data.total$man.bmi>=30
data.total$female.overweight = data.total$woman.bmi>=25
data.total$female.obese = data.total$woman.bmi>=30
data.total$stunted = data.total$child.height.age<=-2
data.total$wasted = data.total$child.weight.height<=-2


clustertab=data.table(data.total)[,.(
  mean.male.overweight=weighted.mean(male.overweight, weights, na.rm=TRUE)
 ,mean.male.obese=weighted.mean(male.obese, weights, na.rm=TRUE)
 ,mean.female.overweight=weighted.mean(female.overweight, weights, na.rm=TRUE)
 ,mean.female.obese=weighted.mean(female.obese, weights, na.rm=TRUE)
 ,mean.stunted = weighted.mean(stunted, weights, na.rm=TRUE)
 ,mean.wasted = weighted.mean(wasted, weights, na.rm=TRUE)
 ,mean.P20 = weighted.mean(p20, weights, na.rm=TRUE)
 ,mean.ext = weighted.mean(ext, weights, na.rm=TRUE)
 ,mean.wealth = weighted.mean(wealth, weights, na.rm=TRUE)
 ),by=.(filename,cluster)]
clusters=read.csv('stuntingclusters.csv')
setnames(clusters,"DHSCLUST","cluster")
joineddata=merge(clustertab,clusters, by=c("filename","cluster"))
write.csv(joineddata,"Overweightwithlatlong.csv")

regiontab=data.table(data.total)[,.(
 mean.male.overweight=mean(male.overweight, na.rm=TRUE)
  ,mean.male.obese=mean(male.obese, na.rm=TRUE)
  ,mean.female.overweight=mean(female.overweight, na.rm=TRUE)
  ,mean.female.obese=mean(female.obese,  na.rm=TRUE)
  ,mean.stunted = mean(stunted, na.rm=TRUE)
  ,mean.wasted = weighted.mean(wasted, weights, na.rm=TRUE)
  ,mean.P20 = weighted.mean(p20, weights, na.rm=TRUE)
  ,mean.ext = mean(ext, na.rm=TRUE)
  ,mean.wealth = mean(wealth,  na.rm=TRUE)
),by=.(filename,region)]
write.csv(regiontab,"regionalnutrition.csv")
