import os
import sys

def folder_setup(i):
    if(int(i)<10): #padding for folder names
        day="day0"+i
    else:
        day="day"+i

    if not os.path.exists(day): #if dir doesn't already exist
        os.makedirs(day) #make dir
        os.makedirs(day+"/py") #make subdir

        #initialize with some data read code
        outstring = 'text_file = open("../input.txt", "r")\nlines = [v.strip("\\n") for v in text_file.readlines()]\nprint(lines)\n'

        for j in range(1,3): #make some python empty shell programs
            file=day+"/py/d"+i+"p"+str(j)+".py" #define filename
            open(file, 'w').close() #create the file
            with open(file, 'w') as f: #write out the initial data read code
                f.write(outstring)

        for file in os.listdir(): #print all the files in current directory
            print(file)

    else:
        print(day,"directory exists")
            
folder_setup(i=sys.argv[1])
