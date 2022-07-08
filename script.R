###########################################################
####### script to read in multinet data ###################
###########################################################

## some preparations --------------------------------------
# install packages if not already available
list.of.packages <- c("data.table")
new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, function(x) suppressMessages(require(x, character.only = TRUE)))

### source R directory to get functions
invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source, encoding = 'UTF-8'))


## DO the actual thing ------------------------------------

# step 1: define the name of the directory in which all txt files are going to be dumped
dir <- 'data/IML2022_PMZA_spring'                                                   # name of data folder                                         
dir.create(dir,showWarnings = FALSE)                            # will create that folder for you, if it doesn't exist already

# step 2: outside R, copy all your data files here

# step 3: let R know the names of those files
files <- dir(dir, pattern = ".txt",full.names=TRUE)             # get the names of all text files

par(mfrow=c(4,4))
# step 4: read them into R
meta <- read.multimeta(files,tz=Sys.timezone(),plot=TRUE,min.dive.depth=5)  # use plot=FALSE if you do not want to see what lines were selected to calculate e.g. volume.                 
View(meta)                                                       # have a preview to see everything is ok                         

## step 5: save data ascsv
write.csv(meta,file='csv/multinet.csv',row.names = FALSE)

