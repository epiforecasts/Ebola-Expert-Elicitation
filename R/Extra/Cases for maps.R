# wd
getwd()
wdir<-"~/Documents/GitHub/Ebola-DRC-maps/"
setwd(wdir)

# packages
require(dplyr)
require(RColorBrewer)
require(leaflet)
require(ggplot2)
require(mapview)
require(raster)
require(htmltools)
library(sp)
require(sf)
require(rgeos)
require(htmlwidgets)
library(spdep)
library(tidyverse)


### get data

# cases, from James
headers =  read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrr9DRaC2fXzPdmOxLW-egSYtxmEp_RKoYGggt-zOKYXSx4RjPsM4EO19H7OJVX1esTtIoFvlKFWcn/pub?gid=1564028913&single=true&output=csv", header = F, nrows = 1, as.is = TRUE)
evd_cases = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrr9DRaC2fXzPdmOxLW-egSYtxmEp_RKoYGggt-zOKYXSx4RjPsM4EO19H7OJVX1esTtIoFvlKFWcn/pub?gid=1564028913&single=true&output=csv", skip = 2, header = F)
# evd_cases = read.csv("/Users/eoccaros/Documents/VEEPED/Ebola/Data/HDX/Data_ DRC Ebola Outbreak, North Kivu and Ituri - MOH-By-Health-Zone.csv", skip = 2, header = F)
colnames(evd_cases)= headers

### clean evd_cases
# correct classes
evd_cases<-as.data.frame(evd_cases)
evd_cases$health_zone<-toupper(as.character(evd_cases$health_zone))
evd_cases<-evd_cases%>%mutate_all(as.character)
evd_cases$total_cases<-as.numeric(evd_cases$total_cases)
# only keep records with 1 or more cases
evd_cases<-evd_cases%>%filter(total_cases>0)
# add a row column in case any problems later
evd_cases$rowN<-1:nrow(evd_cases)
# resolve those where the cases database and the spatial databases don't match
evd_cases$health_zone<-gsub("KAYINA", "KAYNA",evd_cases$health_zone)
evd_cases$health_zone<-as.character(evd_cases$health_zone)
evd_cases$health_zone<-gsub("MANGURUJIPA", "MANGUREDJIPA",evd_cases$health_zone)
evd_cases$health_zone<-as.character(evd_cases$health_zone)
evd_cases$health_zone<-gsub("N/A", NA ,evd_cases$health_zone)
evd_cases$health_zone<-as.character(evd_cases$health_zone)
evd_cases$health_zone<-gsub("RWAMPARA \\(BUNIA)", "RWAMPARA" ,evd_cases$health_zone)


# remove cases where health zone not specified
evd_cases<-evd_cases[!is.na(evd_cases$health_zone),] #remove HZ with no name that has 1 case
# check if any NAs
table(is.na(evd_cases$health_zone))
table(is.na(evd_cases$total_cases))

## output particular data on EVD cases for plot 
# include only cases in the last 2 weeks of data
evd_cases$report_date<-as.Date(evd_cases$report_date)

novquestions_cases<-evd_cases%>%
  filter(report_date>"2019-10-05" & report_date<="2019-10-19")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

decquestions_cases<-evd_cases%>%
  filter(report_date>"2019-10-26" & report_date<="2019-11-09")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

janquestions_cases<-evd_cases%>%
  filter(report_date>"2019-12-01" & report_date<="2019-12-15")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

febquestions_cases<-evd_cases%>%
  filter(report_date>"2019-12-29" & report_date<="2020-01-12")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

marquestions_cases<-evd_cases%>%
  filter(report_date>"2020-01-28" & report_date<="2020-02-11")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

marquestions_cases
  
