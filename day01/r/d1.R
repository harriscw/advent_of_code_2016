rm(list=ls())
# input = "R2, L3"
# input = "R2, R2, R2"
# input = "R5, L5, R5, R3"
# input = "R8, R4, R4, R8"
input=readLines("aoc/advent_of_code_2016/day01/input.txt")
input=trimws(unlist(strsplit(input,","))) #convert to a vector

manhattanDistance <- function(vect1, vect2){#function for manhattan distance found on internet
  dist <- abs(vect1 - vect2)
  dist <- sum(dist)
  return(dist)
}

rl_list=list("R"=-90,"L"=90) #look up table for Left vs Right

facing=90 #direction you are facing, initially north
pos=c(0,0) #position
pos_df=data.frame(x=pos[1],y=pos[2]) #dataframe for all positions ever visited

for(i in input){ #iterate over each instruction
  
  thisone=c(substring(i,1,1),substring(i,2)) #parse into direction vs steps
  steps=as.numeric(thisone[2])

  facing=facing+rl_list[[thisone[1]]] #updare what degree you are facing
  dir=(facing/90) %% 4 #use modulo to always get consitent dir. 0=E,1=N,3=W,4=S
  
  if(dir==0){ #now update position.  also keep a dataframe of every position for part 2
    pos=c(pos[1]+steps,pos[2])
    for(j in 1:steps){
      pos_df = rbind(pos_df,
                     data.frame(x=tail(pos_df[,"x"],1)+1,y=tail(pos_df[,"y"],1))
                     )
    }
  }else if(dir==1){
    pos=c(pos[1],pos[2]+steps)
    for(j in 1:steps){
      pos_df = rbind(pos_df,
                     data.frame(x=tail(pos_df[,"x"],1),y=tail(pos_df[,"y"],1)+1)
      )
    }
  }else if(dir==2){
    pos=c(pos[1]-steps,pos[2])
    for(j in 1:steps){
      pos_df = rbind(pos_df,
                     data.frame(x=tail(pos_df[,"x"],1)-1,y=tail(pos_df[,"y"],1))
      )
    }
  }else if(dir==3){
    pos=c(pos[1],pos[2]-steps)
    for(j in 1:steps){
      pos_df = rbind(pos_df,
                     data.frame(x=tail(pos_df[,"x"],1),y=tail(pos_df[,"y"],1)-1)
      )
    }
  }

}


print(paste("Part 1:",manhattanDistance(c(0,0),pos)))

for(i in 2:nrow(pos_df)){ #now for part 2 find the dup that occurs first
  print(i)
  thisone=c(pos_df[i,"x"],pos_df[i,"y"]) #for each row...
  
  for(j in 1:nrow(pos_df[1:i-1,])){ #...see if it already occurred
    
    thatone=c(pos_df[j,"x"],pos_df[j,"y"])
    
    if(identical(thisone,thatone)){
      print(thisone)
      print(thatone)
      print(paste("Part 2:",manhattanDistance(c(0,0),thisone)))
      stop("the end")
    }
  }
}

###
# Do a plot
###

plot(pos_df,cex=.5)
points(thisone[1],thisone[2],pch=4,cex=3,col="red")
points(0,0,pch=19,col="green",cex=2)
points(tail(pos_df[,"x"],1),tail(pos_df[,"y"],1),pch=19,col="red",cex=2)

###
# Do an animation
###

library(gganimate)
library(gifski)

pos_df$n = 1:nrow(pos_df)
p = ggplot(pos_df, aes(x=x, y=y)) +
  geom_point() +
  transition_time(n) +
  shadow_mark()

animate(
  plot = p, 
  nframes = nrow(pos_df), 
  fps = 50, 
  end_pause = 8
)

