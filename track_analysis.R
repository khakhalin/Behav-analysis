# Analyzer for Alex's tracking files

require(tidyr)
require(dplyr)
require(ggplot2)

folderName <- "C:/Users/Arseny/Documents/2_Behavior/Alex tracker/"

d <- read.csv(paste(folderName,'Alcohol1.csv',sep=""))
# headers: time,x1,y1,a1,x2,y2,a2,x3,y3,a3,x4,y4,a4,x5,y5,a5,

names(d)
d <- select(d,-X) # Remove last empty column
d <- d[-(1:10),] # remove first 10 frames

ggplot(data=d,aes(time,x1)) + geom_line() + theme_classic()

nTime <- dim(d)[1]

dd <- d[-1,]-d[1:(nTime-1),] # Differences dataframe
dd$step <- dd$time
dd$time <- d$time[1:(nTime-1)]

# Plot trajectories for 1 tadpole:
ggplot(data=d,aes(x1,y1)) + geom_path() + theme_classic() # geom_path for trajectories!

head(d)
d <- d[1:(nTime-1),] # Shorten, to make same length as dd
# ggplot(data=d,aes(time,a1)) + geom_line() + theme_classic() # Accuracy

dcopy <- d # Just in case I mess upp
d <- dcopy # Restore

tads <- list() # Init empty list
for(iTad in 1:5){
  print(iTad)
  #dd <- dd %>% mutate(temp=sqrt(.[[3*(iTad-1)+2]]^2 + .[[3*(iTad-1)+3]]^2)/step*1000) # Calculate speed
  #d <- cbind(d,dd$temp) # Move this column to d
  #names(d)[16+iTad] <- paste0("s",iTad) # Name it s1..s5
  
  tads[[iTad]] <- data.frame(time=d$time, a=d[[3*(iTad-1)+1]],
                             x=d[[3*(iTad-1)+2]], y=d[[3*(iTad-1)+3]],
                             vx=dd[[3*(iTad-1)+2]]/dd$step, vy=dd[[3*(iTad-1)+3]]/dd$step)
  tads[[iTad]] <- mutate(tads[[iTad]], v=sqrt(vx^2 + vy^2))
  
  p <- ggplot() + theme_classic()
  for(t in 1:nTime){
    if(!is.na(tads[[iTad]]$v[t]) & t>10 & t<nTime-10){
      if(tads[[iTad]]$v[t]>0.5){
        p <- p + geom_path(data=data.frame(x=-10:10, y=tads[[iTad]]$v[(t-10):(t+10)]), aes(x,y))
      }
    }
  }
  print(p)
  
  if(iTad>1){
    for(jTad in 1:(iTad-1)){
      print(jTad+100)
    }
  }
}
head(tads[[3]])


ggplot(tads[[3]],aes(a,v)) + geom_jitter(alpha=0.2,w=0.02,h=0) + theme_classic() + 
  geom_smooth(method="lm",se=F)
