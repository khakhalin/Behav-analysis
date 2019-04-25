# Analyzer of the summary file

require(tidyr)
require(dplyr)
require(ggplot2)
require(viridis)

rm(list=ls()) # Clear workspace

folderName <- "C:/Users/Arseny/Documents/2_Behavior/behav-git/" # Work
# folderName <- "C:/Users/Sysadmin/Documents/_Science/Behav-analysis-git/"

d <- read.csv(paste(folderName,'summary.csv',sep=""))
names(d)

d <- mutate(d,type=ifelse(substr(expName,1,1)=="C","Control","Alcohol"))
d <- mutate(d,type = factor(type, levels=c("Control","Alcohol")))
head(d)

ggplot(data=d,aes(type,v)) + theme_classic() + geom_jitter(w=0.2) + ylab('Swimming speed, cm/s')
t.test(data=d,v~type) # 0.04

ggplot(data=d,aes(type,r)) + theme_classic() + geom_jitter(w=0.2) + ylab('Swimming off-edge')
t.test(data=d,r~type) # 0.17

ggplot(data=d,aes(type,dm)) + theme_classic() + geom_jitter(w=0.2) + ylab('Mean distance to others, cm')
# Low = huddling in a group, Large = free swimming
t.test(data=d,dm~type) # 0.9

ggplot(data=d,aes(type,ds)) + theme_classic() + geom_jitter(w=0.2) + ylab('SD distance to others, cm')
# Low = uniformly spread; high = some clustered, some swim alone
t.test(data=d,ds~type) # 0.05