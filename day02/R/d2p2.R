input = readLines("../input.txt")
input = lapply(input,function(x){
  unlist(strsplit(x,""))
         })

num_lookup=list( #Look up table for a given coordinate.
  "1"=list("3"="1"), #converting everything to char now
  "2"=list("2"="2","3"="3","4"="4"), #because you cant name list slot numeric
  "3"=list("1"="5","2"="6","3"="7","4"="8","5"="9"), #feels janky
  "4"=list("2"="A","3"="B","4"="C"),
  "5"=list("3"="D")
)

x=1 #set initial coordinates
y=3

#decided a lookup table for out of bounds coordinates is less annoying than a million if-else clauses
bad_coords=data.frame(y=c(1:5,     rep(0,5),1,1,1,1,2,2,4,4,5,5,5,5,rep(6,5), 1:5),
                      x=c(rep(0,5),1:5,     1,2,4,5,1,5,1,5,1,2,4,5,1:5,      rep(6,5))
                      )

do_the_move=function(x,y,instrux){ #Function to change x and y given an instruction
  if(instrux=="U"){
    new=y-1 #where im told to move
    bad=bad_coords[bad_coords$y==new,"x"] #is it out of bounds?
    if(!(x %in% bad)){y=new} #if not go there, if it is then dont
  }else if(instrux=="D"){
    new=y+1
    bad=bad_coords[bad_coords$y==new,"x"]
    if(!(x %in% bad)){y=new}
  }else if(instrux=="L"){
    new=x-1
    bad=bad_coords[bad_coords$x==new,"y"]
    if(!(y %in% bad)){x=new}
  }else if(instrux=="R"){
    new=x+1
    bad=bad_coords[bad_coords$x==new,"y"]
    if(!(y %in% bad)){x=new}
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
  acc=c(acc,num_lookup[[as.character(y)]][[as.character(x)]]) #append final number to list
}#*****NOTE the line above now has 2 as.character() calls added because I had to change my lookup to character

print(paste0("Part 2: ",paste(acc,collapse="")))
