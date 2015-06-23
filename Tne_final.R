##################################################################################
##                                                                              ##
##                        Transportation and Environment Final                  ##
##                                                                              ##
##################################################################################

#####################################
#          Calling Packages         #
#####################################
library(ggplot2)
library(plyr)
library(gtools)
library(reshape2)
library(ggmap)
library(dplyr)

######################################
#  Inserting the files together     ##
######################################

# 
path <- "C:/Course readings etc/Transportation & Environment/Decibel data/"
files <- list.files(path=path, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep=""), header=TRUE))
}

# Putting location in each files

list.location1 <- ls(pattern="*Location1_*")
list.location2 <- ls(pattern="*Location2_*")
list.location3 <- ls(pattern="*Location3_*")
list.location4 <- ls(pattern="*Location4_*")

location1_all <- rbind_all(mget(list.location1))
location2_all <- rbind_all(mget(list.location2))
location3_all <- rbind_all(mget(list.location3))
location4_all <- rbind_all(mget(list.location4))

location1_all$Location <- "One"
location2_all$Location <- "Two"
location3_all$Location <- "Three"
location4_all$Location <- "Four"

# Combining, second and daytime

Data_all <- rbind(location1_all, location2_all, location3_all, location4_all)
Data_all$Measuretime <- substr(Data_all$Time, 1, 8)
Data_all$Hour <- substr(Data_all$Time, 1, 2)
Data_all <- Data_all[, -2]
Data_all$Average <- as.numeric(as.character(Data_all$Average))
Data_all$Peak <- as.numeric(as.character(Data_all$Peak))
Data_all$Hour <- as.numeric(as.character(Data_all$Hour))
Data_all$Daytime<- ifelse(Data_all$Hour < 12,"AM", "PM")

# To summarize daat for each second-

Second_level = function(Data_all){
  Mean = mean(Data_all$Average, na.rm=T)
  Maximum= max(Data_all$Peak, na.rm=T)
  data.frame(Mean, Maximum)
}

Data_second <- ddply(Data_all, .(Date,Location,Daytime,Measuretime), Second_level)

# Summarizing again to get the total summary( could be done in the forst step itself)

Session_summary = function(file){
  Average= mean(file$Mean)
  Peak= max(file$Maximum)
  data.frame(Average, Peak)
}

Location_summary <- ddply(Data_second, .(Location), Session_summary )

## Also got the EPA described level of 70 DB and how much that's being crossed.

Higher_noise <- subset(Data_second, Data_second$Mean>= 70)
Higher_table <- ddply(Higher_noise, .(Location), summarize,length(Location))

## Analysis for individual sessions

Session_1 <- subset(Data_second, Data_second$Date == "2015:05:20" &
                      Data_second$Daytime == "AM")
Session_2 <- subset(Data_second, Data_second$Date == "2015:05:20" & 
                      Data_second$Daytime == "PM")
Session_3 <- subset(Data_second, Data_second$Date == "2015-05-22" & 
                      Data_second$Daytime == "AM")
Session_4 <- subset(Data_second, Data_second$Date == "2015-05-22" & 
                      Data_second$Daytime == "PM")
Session_5 <- subset(Data_second, Data_second$Date == "2015-05-27" & 
                      Data_second$Daytime == "AM")
Session_6 <- subset(Data_second, Data_second$Date == "2015-05-27" & 
                      Data_second$Daytime == "PM")

## Summarizing individual sessions

summary_session1<- ddply(Session_1, .(Location), Session_summary )
names(summary_session1)<- c("Location","One_avg", "One_peak")
summary_session2<- ddply(Session_2, .(Location), Session_summary )
names(summary_session2)<- c("Location","Two_avg", "Two_peak")
summary_session3<- ddply(Session_3, .(Location), Session_summary )
names(summary_session3)<- c("Location","Three_avg", "Three_peak")
summary_session4<- ddply(Session_4, .(Location), Session_summary )
names(summary_session4)<- c("Location","Four_avg", "Four_peak")
summary_session5<- ddply(Session_5, .(Location), Session_summary )
names(summary_session5)<- c("Location","Five_avg", "Five_peak")
summary_session6<- ddply(Session_6, .(Location), Session_summary )
names(summary_session6)<- c("Location","Six_avg", "Six_peak")

# Merging all

summary_sessiona <- merge( summary_session5, summary_session6,by= "Location",all=TRUE)
summary_session <- cbind(summary_session1, summary_session2, summary_session3, 
                         summary_session4,summary_sessiona)

# Reordering things
summary_session<- summary_session[, -c(4,7,10,13)]
summary_session_order<- summary_session[c(2,4,3,1), c(1:2,4,6,8,10,12,3,5,7,9,11,13)]

# CSV output- Location summary
write.csv(Location_summary, 
          "C:/Course readings etc/Transportation & Environment/Location_summary.csv")
# CSV output- Location summary
write.csv(Higher_table, 
          "C:/Course readings etc/Transportation & Environment/Higher_table.csv")
# CSV output- Sessionwise summary
write.csv(summary_session_order, 
          "C:/Course readings etc/Transportation & Environment/Session_summary.csv")