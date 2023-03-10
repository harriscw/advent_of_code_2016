input = readLines("../input.txt")
input = lapply(input,function(x){
  unlist(strsplit(x,""))
         })

num_lookup=list( #Look up table for a given coordinate
  list(1,2,3),
  list(4,5,6),
  list(7,8,9)
)

x=2 #set initial coordinates
y=2

do_the_move=function(x,y,instrux){ #Function to change x and y given an instruction
  if(instrux=="U"){
    y=max(y-1,1)
  }else if(instrux=="D"){
    y=min(y+1,3)
  }else if(instrux=="L"){
    x=max(x-1,1)
  }else if(instrux=="R"){
    x=min(x+1,3)
  }
  return(c(x,y))
}

acc=c()
for(each in input){ #loop over each instruction
  for(instrux in each){#loop over each direction within the instruction
    res=do_the_move(x=x,y=y,instrux=instrux) #do the move
    x=res[1] #reset x and y
    y=res[2]
  }
  acc=c(acc,num_lookup[[y]][[x]]) #append final number to list
}

print(paste0("Part 1: ",paste(acc,collapse="")))
