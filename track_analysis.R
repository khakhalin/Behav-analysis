# Analyzer for Alex's tracking files

require(tidyr)
require(dplyr)
require(ggplot2)
require(viridis)

folderName <- "C:/Users/Arseny/Documents/2_Behavior/behav-git/data/" # Work
# folderName <- "C:/Users/Sysadmin/Documents/_Science/Behav-analysis-git/data/"

d <- read.csv(paste(folderName,'Alcohol2.csv',sep=""))
# headers: time,x1,y1,a1,x2,y2,a2,x3,y3,a3,x4,y4,a4,x5,y5,a5,

names(d)
d <- select(d,-X) # Remove last empty column
d <- d[-(1:10),] # remove first 10 frames
nTime <- dim(d)[1]
dd <- d[-1,]-d[1:(nTime-1),] # Differences dataframe
dd$step <- dd$time
dd$time <- d$time[1:(nTime-1)]
d <- d[1:(nTime-1),] # Shorten, to make same length as dd
head(d)

#dcopy <- d # Just in case I mess upp - prob not needed anymore
#d <- dcopy # Restore

tads <- list() # Init empty list
for(iTad in 1:5){ # normally would be in 1:5
  print(iTad)
  #dd <- dd %>% mutate(temp=sqrt(.[[3*(iTad-1)+2]]^2 + .[[3*(iTad-1)+3]]^2)/step*1000) # Calculate speed
  #d <- cbind(d,dd$temp) # Move this column to d
  #names(d)[16+iTad] <- paste0("s",iTad) # Name it s1..s5
  
  tads[[iTad]] <- data.frame(time=d$time, a=d[[3*(iTad-1)+4]],
                             x=d[[3*(iTad-1)+2]], y=d[[3*(iTad-1)+3]],
                             vx=dd[[3*(iTad-1)+2]]/dd$step, vy=dd[[3*(iTad-1)+3]]/dd$step)
  tads[[iTad]] <- mutate(tads[[iTad]], v=sqrt(vx^2 + vy^2))
  
  #p <- ggplot() + theme_classic()
  for(t in 1:nTime){
    if(!is.na(tads[[iTad]]$v[t])){
      if(tads[[iTad]]$a[t]<0.75){
        tads[[iTad]]$v[t] <- (tads[[iTad]]$v[t-1] + tads[[iTad]]$v[t+1])/2 # Shameful; replace with something meaningful
      }
      if(t>3 & t<nTime-20){
        if(tads[[iTad]]$v[t]>0.25){
          #p <- p + geom_path(data=data.frame(x=-3:20, y=tads[[iTad]]$v[(t-3):(t+20)]), aes(x,y), alpha=0.5)
        }
      }
    }
  }
  #print(p)
}
head(tads[[1]])

# Guess dish edges from trajectories:
dishSides <- c(median(min(tads[[1]]$x),min(tads[[2]]$x),min(tads[[3]]$x),min(tads[[4]]$x),min(tads[[5]]$x)),
               median(max(tads[[1]]$x),max(tads[[2]]$x),max(tads[[3]]$x),max(tads[[4]]$x),max(tads[[5]]$x)),
               median(min(tads[[1]]$y),min(tads[[2]]$y),min(tads[[3]]$y),min(tads[[4]]$y),min(tads[[5]]$y)),
               median(max(tads[[1]]$y),max(tads[[2]]$y),max(tads[[3]]$y),max(tads[[4]]$y),max(tads[[5]]$y)))
dishSides

# Guess dish size from trajectories:
dishDiameter <- median(c(dishSides[2]-dishSides[1],dishSides[4]-dishSides[3]))
dishDiameter

# Guess dish center from trajectories:
dishCenter <- c(mean(c(dishSides[1],dishSides[2])),mean(c(dishSides[3],dishSides[4])))
dishCenter

# Plot trajectory for 1 tadpole:
ggplot(data=tads[[2]],aes(x,y,color=v)) + geom_path() + # it's good to use geom_path for trajectories!
  theme_void() + scale_colour_viridis() + coord_fixed(ratio=1)
last_plot() + geom_point(data=data.frame(x=dishCenter[1],y=dishCenter[2]),aes(x,y),color="red")


for(iTad in 1:5){
  if(iTad>1){
    for(jTad in 1:(iTad-1)){
      print(iTad*1000+jTad)
    }
  }
}




# Do uncertain points have higher speed?
ggplot(tads[[1]],aes(a,v)) + geom_jitter(alpha=0.2,w=0.02,h=0) + theme_classic() + 
  geom_smooth(method="lm",se=F)






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
