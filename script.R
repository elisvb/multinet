source('R/reading.R', encoding = 'UTF-8') 
dir <- 'data'                                                   # dump all txt files from multinet here                                           

files <- dir(dir, pattern = ".txt",full.names=TRUE)             # get the names of all text files

## read in meta data (summary statistics for all files)
meta <- read.multimeta(files,tz=Sys.timezone())        # this presumes the correct time zone of the files is your laptop one!!! change tz if not                   

    # add extra columns if necessary
    meta$`Data Start UTC` <- meta$`Data Start`         # a new column with a different time zone if you want
    attr(meta$`Data Start UTC`, "tzone") <- "utc"
    meta$`Data End UTC` <- meta$`Data End`             # a new column with a different time zone if you want
    attr(meta$`Data End UTC`, "tzone") <- "utc"
    
    meta$Date <- as.Date(meta$`Data Start`)            # add a column with only date

## read in raw data (the data without the header information)
dat <- read.multidat(files)                                 

## merge the meta data and the raw data if you want everything
all <- merge(meta,dat)                                             

## save only the metadata as csv
write.csv(meta,file='csv/multinet.csv',row.names = FALSE)

