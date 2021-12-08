# step 1: define the name of the directory in which all txt files are going to be dumped
dir <- 'data'                                                   # name of data folder                                         
dir.create(dir,showWarnings = FALSE)                            # will create that folder for you, if it doesn't exist already

# step 2: outside R, copy all your data files here

# step 3: let R know the names of those files
files <- dir(dir, pattern = ".txt",full.names=TRUE)             # get the names of all text files

# step 4: read them into R
meta <- read.multimeta(files,tz=Sys.timezone(),plot=TRUE)        # use plot=FALSE if you do not want to see what lines were selected to calculate e.g. volume.                 
View(meta)                                                       # have a preview to see everything is ok                         

## step 5: save data ascsv
write.csv(meta,file='csv/multinet.csv',row.names = FALSE)

