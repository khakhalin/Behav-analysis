# Analyzer for Alex's tracking files

require(tidyr)
require(dplyr)
require(ggplot2)
require(viridis)

rm(list=ls()) # Clear workspace

do_basics = 0 # Whether we need to calculate per-tadpole values, or go straight to schooling

### As silly R cannot do relative folders well, have to put full path here
# folderName <- "C:/Users/Arseny/Documents/2_Behavior/behav-git/data/" # Work
folderName <- "C:/Users/Sysadmin/Documents/_Science/Behav-analysis-git/data/"

expName = 'Control4'
d <- read.csv(paste(folderName,expName,'.csv',sep=""))
# headers: time,x1,y1,a1,x2,y2,a2,x3,y3,a3,x4,y4,a4,x5,y5,a5,
# headers: time,x1,y1,a1,x2,y2,a2,x3,y3,a3,x4,y4,a4,x5,y5,a5,

names(d)
if("X" %in% names(d))
  d <- select(d,-X) # Remove last empty column (not sure where it comes from, for some sets)
d <- d[-(1:10),] # remove first 10 frames
names(d)[1] <- "time" # Rename 1st col (if it's named weirdly)
d <- d %>% filter(!is.na(time)) # Skip all NA rows
nTime <- dim(d)[1]
dd <- d[-1,]-d[1:(nTime-1),] # Differences dataframe
dd$step <- dd$time
dd$time <- d$time[1:(nTime-1)]
d <- d[1:(nTime-1),] # Shorten, to make same length as dd
# head(d)

### --- Pack tadpole data in a list of dataframes
tads <- list() # Init empty list
for(iTad in 1:5){ # normally would be in 1:5
  # print(iTad)
  tads[[iTad]] <- data.frame(time=d$time, a=d[[3*(iTad-1)+4]],
                             x=d[[3*(iTad-1)+2]], y=d[[3*(iTad-1)+3]],
                             vx=dd[[3*(iTad-1)+2]]/dd$step, vy=dd[[3*(iTad-1)+3]]/dd$step)
  
  for(t in 2:nrow(tads[[iTad]])-1){ # Running average for speeds: this will slow everhything down a lot :(
    tads[[iTad]]$vx[t] <- mean(tads[[iTad]]$vx[(t-1):(t+1)], na.rm=T)
    tads[[iTad]]$vy[t] <- mean(tads[[iTad]]$vy[(t-1):(t+1)], na.rm=T)
  }
  tads[[iTad]] <- mutate(tads[[iTad]], v=sqrt(vx^2 + vy^2))
}


### --- Guess dish edges from trajectories:
dishSides <- c(median(c(min(tads[[1]]$x),min(tads[[2]]$x),min(tads[[3]]$x),min(tads[[4]]$x),min(tads[[5]]$x))),
               median(c(max(tads[[1]]$x),max(tads[[2]]$x),max(tads[[3]]$x),max(tads[[4]]$x),max(tads[[5]]$x))),
               median(c(min(tads[[1]]$y),min(tads[[2]]$y),min(tads[[3]]$y),min(tads[[4]]$y),min(tads[[5]]$y))),
               median(c(max(tads[[1]]$y),max(tads[[2]]$y),max(tads[[3]]$y),max(tads[[4]]$y),max(tads[[5]]$y))))
dishSides

# Guess dish size from trajectories:
dishDiameter <- median(c(dishSides[2]-dishSides[1],dishSides[4]-dishSides[3]))
dishDiameter

# Guess dish center from trajectories:
dishCenter <- c(mean(c(dishSides[1],dishSides[2])),mean(c(dishSides[3],dishSides[4])))
dishCenter

if(do_basics){
  # Plot trajectory for 1 tadpole:
  ggplot(data=tads[[2]],aes(x,y,color=v)) + geom_path() + # it's good to use geom_path for trajectories!
    theme_void() + scale_colour_viridis() + coord_fixed(ratio=1)
  last_plot() + geom_point(data=data.frame(x=dishCenter[1],y=dishCenter[2]),aes(x,y),color="red")
  
  
  ### --- Average speeds
  out <- data.frame(expName=rep(expName,5),id=1:5) # Initialize output dataframe
  newSlice <- data.frame()
  for(iTad in 1:5){
    newSlice <- rbind(newSlice, tads[[iTad]] %>% filter(!is.na(v)) %>% filter(a>0.5) %>% summarize(v=mean(v)))
    # Technically a before should also be good, for v estimation to be decent
  }
  out <- cbind(out,newSlice)
  out$v <- out$v*1000/dishDiameter*14 # from pixels/ms to cm/s
  
  
  ### --- Share of time spent in the middle (within 2/3 r from the center)
  newSlice <- data.frame()
  for(iTad in 1:5){
    temp <- tads[[iTad]] %>% filter(a>0.5) # Only leave good points
    newSlice <- rbind(newSlice, data.frame(r=mean(
      sqrt((temp$x-dishCenter[1])^2 + (temp$y-dishCenter[2])^2) < dishDiameter/3)))
  }
  #newSlice
  out <- cbind(out,newSlice)
  #out
  
  
  ### --- Tadpole-tadpole distances, calculated per tadpole
  dist <- data.frame()
  for(iTad in 1:5){
    if(iTad>1){ # Because for loops in R are weird, need this extra check
      tempi <- tads[[iTad]][,1:4] # Only first 4 columns
      names(tempi) <- c("t1","a1","x1","y1") # Rename
      for(jTad in 1:(iTad-1)){
        temp <- cbind(tempi,tads[[jTad]][,1:4]) # Add second tad with original column names
        temp <- temp %>% filter(a>0.5 & a1>0.5) # Only leave reasonable points
        dist <- rbind(dist, summarize(temp, i=iTad, j=jTad, 
                                      dm=mean(sqrt((x-x1)^2 + (y-y1)^2)),
                                      ds = sd(sqrt((x-x1)^2 + (y-y1)^2))))
      }
    }
  }
  # dist
  newSlice <- data.frame()
  for(iTad in 1:5){
    temp <- rbind(filter(dist,i==iTad),filter(dist,j==iTad))
    newSlice <- rbind(newSlice, summarize(temp, dm=mean(dm), ds=mean(ds)))
  }
  # newSlice
  out <- cbind(out,newSlice)
  out <- mutate(out, dm = dm/dishDiameter*14, ds = ds/dishDiameter*14) # From pixels to cm
  
  print(out, row.names=FALSE) # <-- Main output
  
  stop() # Don't execute everything after
}


### ------------------ Schooling assessment
# Part one: calculate the distances
dfDist <- data.frame() # pairwise inter-tadpole distances
dfAlign <- data.frame() # pairwise inter-tadpole alignment
for(iTad in 1:5){
  if(iTad>1){ # Because for loops in R are weird (can go backwards), need this extra check
    tempi <- tads[[iTad]][,1:7] # Only get first 7 columns (there should be only 7 columns, but just in case)
    names(tempi) <- c("t1","a1","x1","y1","vx1","vy1","v1") # Rename
    for(jTad in 1:(iTad-1)){
      temp <- cbind(tempi,tads[[jTad]][,1:7]) # Add second tad with original column names
      temp <- cbind(temp,with(temp,sqrt((x-x1)^2 + (y-y1)^2))) # Calculate distance across time
      names(temp)[length(names(temp))]<-"d"  # Rename this new column to d (for distance)
      temp <- cbind(temp,with(temp,(vx1*vx + vy1*vy)/(v1*v)))
      names(temp)[length(names(temp))]<-"align"  # Rename this new column to align
      if(nrow(dfDist)==0){
        dfDist <- data.frame(y=temp$d) # Name doesn't matter here; will be renamed below
        dfAlign <- data.frame(y=temp$align)
      }
      else{
        dfDist <- cbind(dfDist, temp$d)
        dfAlign <- cbind(dfAlign, temp$align)
      }
      names(dfDist)[length(names(dfDist))]<-sprintf("c%d%d",iTad,jTad)  # Rename last column
      names(dfAlign)[length(names(dfAlign))]<-sprintf("c%d%d",iTad,jTad)  # Rename last column
    }
  }
}
#' This align value is naive and imperfect, as it compares speed values in a given point, and this estimation is
#' very noisy. A better estimation should be based on a smoothed model of tadpole swimming. Also if v==0 happens,
#' then dot product/v = nan, and if all are nan, the mean is also nan. Which is silly.
#' The running average I added above (with length=3) should help, but it would be even better to just carry last
#' reliable orientation over small noisy frames, assuming that once a tadpole arrived at a location, it remains stable,
#' and all micro-motions are just noise.


# Part two: calculate certainties
for(iTad in 1:5){
  if(iTad==1)
    afull <- data.frame(time=tads[[iTad]]$time, a=tads[[iTad]]$a)
  else
    afull <- cbind(afull,tads[[iTad]]$a)
  names(afull)[length(names(afull))]<-sprintf("a%d",iTad)
}
afull <- afull %>% mutate(a=a1*a2*a3*a4*a5) # Sort of total cumulative certainty

# Part 3: Sample at good frames
# Time is measured in ms and runs ro about 11 seconds. Let's take measurements every 2 minutes.
maxTime <- tads[[1]]$time[nrow(tads[[1]])]
schooling <- data.frame() # We'll be collecting data here
for(iFrame in 0:5){
  currentTime <- afull$time[1]+iFrame*60*1000*2
  if(currentTime<=maxTime){
    currentFrame <- min(which(afull$time>currentTime & afull$a>0.8))+1 # Find closest certain frame, add 1 for safety
    if(currentFrame <= maxTime){ # We are still within the video
      print(sprintf("%d",currentFrame))
      temp <- data.frame(d=as.numeric(dfDist[currentFrame,])) # Select a row, transpose into a column
      
      ## Calculate aligns now
      align <- c()
      for(iDist in 1:10){
        if(dfDist[currentFrame,iDist]<dishDiameter/2) # If closer than r_dish...
          align <- c(align, dfAlign[currentFrame,iDist]) # add this align value to the list
      }
      
      tempSum <- summarize(temp, dm=mean(d), ds=sd(d))
      schooling <- rbind(schooling, data.frame(dm=tempSum$dm, ds=tempSum$ds, 
                                               align=mean(align, na.rm=T))) # Store outputs
    }
  }
}
schooling <- schooling %>% mutate(dm=dm/dishDiameter*14, ds=ds/dishDiameter*14)
schooling <- cbind(data.frame(name=rep(expName,nrow(schooling))),schooling)

print(sprintf("Number of rows: %d",nrow(tads[[1]])), quote=F)
print(sprintf("Total length: %d min",round(tads[[1]]$time[nrow(tads[[1]])]/1000/60)), quote=F)

print(schooling, row.names=FALSE) # Main output for this section



stop() # Don't execute further



### ------- Assorted visual tests

# Check if time values make sense:
qplot(1:nrow(tads[[1]]),tads[[1]]$time) + theme_bw()
print(last_plot())

# Check accuracy history
ggplot(data=gather(select(head(afull),-a),key,a,-time),aes(time,a,color=key)) + geom_line() + theme_classic()

# Do uncertain points have higher speed?
ggplot(tads[[1]],aes(a,v)) + geom_jitter(alpha=0.2,w=0.02,h=0) + theme_classic() + 
  geom_smooth(method="lm",se=F)



### ------- Some sketches (not very successful for now) for trajectory smoothing

#p <- ggplot() + theme_classic()
# for(t in 1:nTime){
#   if(!is.na(tads[[iTad]]$v[t])){
#     if(tads[[iTad]]$a[t]<0.75){
#       tads[[iTad]]$v[t] <- (tads[[iTad]]$v[t-1] + tads[[iTad]]$v[t+1])/2 # Shameful; replace with something meaningful
#     }
#     if(t>3 & t<nTime-20){
#       if(tads[[iTad]]$v[t]>0.25){
#         #p <- p + geom_path(data=data.frame(x=-3:20, y=tads[[iTad]]$v[(t-3):(t+20)]), aes(x,y), alpha=0.5)
#       }
#     }
#   }
# }
#print(p)



### ------------------- Graveyard


### Unsuccessful Dalaunay triangulation
#' It technically worked, buti t was a stupid idea, so it shouldn't be used.
#' The thing is: with only 5 points (tadpoles), we always get one point connected to the rest
#' four with edges, like a fan. If points are on a circle (like tadpoles during normal swim)
#' it means that a pair of tadpoles on the opposite sides of a dish are technically "neighboring
#' tadpoles", accordidng to Delaunay.
#' So while it works great for large group of tadpoles (~20, as in James 2015), it totally doensn't 
#' work for small groups.

#install.packages("deldir") # Delaunay triangulation
require(deldir)

# Test Delaunay triangulation:
dtemp <- data.frame()
t <- 500
for(iTad in 1:5){
  dtemp <- rbind(dtemp,data.frame(x=tads[[iTad]]$x[t], y=tads[[iTad]]$y[t]))
}
dtemp
myTri = deldir(dtemp$x,dtemp$y)$delsgs
myTri
ggplot() + theme_classic() + geom_point(data=dtemp,aes(x,y))
for(i in 1:nrow(myTri)){
  last_plot() + geom_line(data=data.frame(x=c(myTri$x1[i],myTri$x2[i]),
                                          y=c(myTri$y1[i],myTri$y2[i])),aes(x,y))
}
print(last_plot())
