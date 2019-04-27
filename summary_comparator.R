# Analyzer of the summary file

require(tidyr)
require(dplyr)
require(ggplot2)
require(viridis)

rm(list=ls()) # Clear workspace

#folderName <- "C:/Users/Arseny/Documents/2_Behavior/behav-git/" # Work
folderName <- "C:/Users/Sysadmin/Documents/_Science/Behav-analysis-git/"

### ------------------- Part 1: per-tadpole analysis

d <- read.csv(paste(folderName,'per-tadpole-data.csv',sep=""))
names(d)

d <- mutate(d,type=ifelse(substr(expName,1,1)=="C","Control","Alcohol"))
d <- mutate(d,type = factor(type, levels=c("Control","Alcohol")))
head(d)

ggplot(data=d,aes(type,v)) + theme_classic() + geom_jitter(width=0.2,height=0) + ylab('Swimming speed, cm/s')
t.test(data=d,v~type) # 0.04

ggplot(data=d,aes(type,r)) + theme_classic() + geom_jitter(width=0.2,height=0) + ylab('Swimming off-edge')
t.test(data=d,r~type) # 0.17

# Don't use these two comparisons; use schooling instead (below):
ggplot(data=d,aes(type,dm)) + theme_classic() + geom_jitter(width=0.2,height=0) + ylab('Mean distance to others, cm')
# Low = huddling in a group, Large = free swimming
t.test(data=d,dm~type) # 0.9
ggplot(data=d,aes(type,ds)) + theme_classic() + geom_jitter(width=0.2,height=0) + ylab('SD distance to others, cm')
# Low = uniformly spread; high = some clustered, some swim alone
t.test(data=d,ds~type) # 0.05

### ------------------- Part 2: schooling analysis

d <- read.csv(paste(folderName,'schooling-data.csv',sep=""))
d <- mutate(d,type=ifelse(substr(expName,1,1)=="C","Control","Alcohol"))
d <- mutate(d,type = factor(type, levels=c("Control","Alcohol")))
head(d)

ggplot(data=d,aes(type,dm)) + theme_classic() + geom_jitter(width=0.05,height=0)
t.test(data=d, dm~type) # 0.4

ggplot(data=d,aes(type,ds)) + theme_classic() + geom_jitter(width=0.05,height=0)
t.test(data=d, ds~type) # 0.5

ggplot(data=d,aes(type,align)) + theme_classic() + geom_jitter(width=0.05,height=0)
t.test(data=d, align~type) # 0.5
