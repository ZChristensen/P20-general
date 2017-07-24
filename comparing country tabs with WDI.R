library(Hmisc)
library(plyr)
library(foreign)
library(data.table)
library(varhandle)
library(survey)
library(readr)
library(WDI)

bycountry_tabs <- read_csv("~/Poverty data/July 2017 P20 updates/bycountry_tabs_2013_2.csv")
headcounts <- read_csv("~/Poverty data/July 2017 P20 updates/headcounts.csv")
Eulertab=data.table(bycountry_tabs)[,.(
                                      p80unregrt=p80.unregistered/(p80.registered+p80.unregistered)
                                    ,p20unregrt=p20.unregistered/(p20.registered+p20.unregistered)
                                    ,reg.rt=(p20.registered+p80.registered)/(p20.registered+p20.unregistered+p80.registered+p80.unregistered)
                                    ,p80stunting.rt=p80.stunted/(p80.stunted+p80.notstunted)
                                    ,p20stunting.rt=p20.stunted/(p20.stunted+p20.notstunted)
                                    ,stunting.rt=(p80.stunted+p20.stunted)/(p80.stunted+p80.notstunted+p20.stunted+p20.notstunted)
                                    ,femalestunt.rt= (p80.female.stunted+p20.female.stunted)/(p80.female.stunted+p80.female.notstunted+p20.female.stunted+p20.female.notstunted)
                                    ,malestunt.rt= (p80.male.stunted+p20.male.stunted)/(p80.male.stunted+p80.male.notstunted+p20.male.stunted+p20.male.notstunted)
                                    ,urban.rt=(p20.urban+p80.urban)/(p20.urban+p80.urban+p20.rural+p80.rural)
                                    ,primarycompletion.rt= (1-((p80.over25.noeduc+p20.over25.noeduc)/(male.25.plus+female.25.plus)))               
                                    ,birth.attend.rt=(p80.one.skilled+p20.one.skilled)/(p80.one.skilled+p20.one.skilled+p80.not.one.skilled+p20.not.one.skilled)
),by=.(iso3)]  
Eulertab=merge(Eulertab,headcounts, by="iso3", all=TRUE)
Eulertab$iso3c=Eulertab$iso3
#WDI = WDI(country="all", indicator=c("SE.PRM.CUAT.ZS","SH.STA.STNT.ZS","SP.REG.BRTH.ZS", "SP.URB.TOTL.IN.ZS", "SH.STA.BRTC.ZS"), start=1996, end=2016, extra=TRUE)
#WDI = WDI(country="all", indicator=c("SE.PRM.CUAT.ZS","SH.STA.STNT.ZS","SP.REG.BRTH.ZS", "SP.URB.TOTL.IN.ZS", "SH.STA.BRTC.ZS"), start=2006, end=2016, extra=TRUE)
WDI = WDI(country="all", indicator=c("SE.PRM.CUAT.ZS","SH.STA.STNT.ZS","SP.REG.BRTH.ZS", "SP.URB.TOTL.IN.ZS", "SH.STA.BRTC.ZS", "SH.STA.STNT.FE.ZS","SH.STA.STNT.MA.ZS"), start=2006, end=2016, extra=TRUE)
WDI$iso3c=unfactor(WDI$iso3c)
WDI$primary.comp.WDI=WDI$SE.PRM.CUAT.ZS/100
WDI$stunting.WDI=WDI$SH.STA.STNT.ZS/100
WDI$female.stunt.WDI=WDI$SH.STA.STNT.FE.ZS/100
WDI$male.stunt.WDI=WDI$SH.STA.STNT.MA.ZS/100
WDI$birth.reg.WDI=WDI$SP.REG.BRTH.ZS/100
WDI$urban.WDI=WDI$SP.URB.TOTL.IN.ZS/100
WDI$skilled.attendant.WDI=WDI$SH.STA.BRTC.ZS/100
WDI$svyYear=WDI$year
# # WDI.tab =data.table(WDI)[,.(
#                             primary.comp.WDI=mean(SE.PRM.CUAT.ZS, na.rm=TRUE)/100
#                             ,stunting.WDI=mean(SH.STA.STNT.ZS, na.rm=TRUE)/100
#                             ,birth.reg.WDI=mean(SP.REG.BRTH.ZS, na.rm=TRUE)/100
#                             ,urban.WDI=mean(SP.URB.TOTL.IN.ZS, na.rm=TRUE)/100
#                             ,skilled.attendent.WDI=mean(SH.STA.BRTC.ZS, na.rm=TRUE)/100
#   )
#                             ,by=.(iso3c,svyYear)
#                             ]
comparison  =merge(WDI,Eulertab, by=c("iso3c","svyYear"), all=TRUE)
comparison$diff.primary = comparison$primary.comp.WDI-comparison$primarycompletion.rt
comparison$diff.stunting=comparison$stunting.WDI-comparison$stunting.rt
comparison$diff.stunting.fe=comparison$female.stunt.WDI-comparison$femalestunt.rt
comparison$diff.stunting.ma=comparison$male.stunt.WDI-comparison$malestunt.rt
comparison$diff.birth.reg=comparison$birth.reg.WDI-comparison$reg.rt
comparison$diff.urban=comparison$urban.WDI-comparison$urban.rt
comparison$diff.attend = comparison$skilled.attendant.WDI-comparison$birth.attend.rt
##Note that WDI skilled attendants look at births, while our birth attendance rate is looking at women and counts anyone who has had a skilled birth in the past period of concern.
describe(comparison$diff.primary)
describe(comparison$diff.birth.reg)
describe(comparison$diff.urban)
describe(comparison$diff.attend)
describe(comparison$diff.stunting)
describe(comparison$diff.stunting.fe)
describe(comparison$diff.stunting.ma)
