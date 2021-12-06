##' Read in multinet meta data
##' @param x vector of text file names from oceanLabs (character)
##' @param tz time zone (character, UTC by default)
##' @details ...
read.multimeta <- function(file,tz='utc'){
    ret <- lapply(file,function(x){
        print(x)
        d <- readLines(x)                                                       # read all lines as vector elements
        if(!grepl('Data Start: ',d[1])) stop('x is not a multinet dat file')    # if the first is wrong, some wrong file format
        
        cols <- c('Data Start','Data End','Cruise','Ship','Station','Consecutive','Bottom depth','Day/Night','Latitude','Longitude') # columns we want
        id.cols <- unlist(sapply(cols,grep,d))                                                            # line numbers of those columns
        
        sub <- d[id.cols]                                      # take only those lines
        sub <- gsub('\t',' ',sub)                              # small correction for when whitespaces are read in as \t
        sub <- sub[grep(paste(cols,collapse = '|'),sapply(strsplit(sub,': '),'[',1))] # if any of the columns names accidentally ocures in the notes/comments, this will remove this line
        sub <- sapply(strsplit(sub,': '),'[',2)                # keep the stuff after :
        sub <- trimws(sub)                                     # remove white spaces before and after
        
        dat <- data.frame(matrix(sub,nrow=1))                  # convert this to a data.frame
        names (dat) <- cols
        
        dat$filename <- x                                      # for tractability and merging with rest of data if necessary
        
        dat[,1] <- as.POSIXct(dat[,1],tz=tz)                   # correct class
        dat[,2] <- as.POSIXct(dat[,2],tz=tz)                   # correct class
        dat[,7] <- as.numeric(dat[,7])                         # correct class
        
        extra  <- read.multidat(x)                             # read in the table and extract info by net
        extra  <- data.frame(
            Net         = as.numeric(unique(extra$Net)),
            Depth.min   = tapply(extra$`Pressure [dbar]`, extra$`Net []`, min), # min depth per net
            Depth.max   = tapply(extra$`Pressure [dbar]`, extra$`Net []`, max), # max depth per net
            Time.start  = tapply(extra$`Time [hh:mm:ss]`, extra$`Net []`, min),  # start time per net
            Time.end    = tapply(extra$`Time [hh:mm:ss]`, extra$`Net []`, max),  # end time per net
            Volume      = tapply(extra$`Volume [m³]`, extra$`Net []`, max)) # total volume filtered
        
        dat <- cbind(dat,extra)
        return(dat)
    })
    do.call('rbind',ret)
}

##' Read in multinet tabular data
##' @param file vector of text file names from oceanLabs (character)
##' @details ...
read.multidat <- function(file){
    ret <- lapply(file,function(x){
        d <- readLines(x)                                                       # read all lines as vector elements
        if(!grepl('Data Start: ',d[1])) stop('x is not a multinet dat file')    # if the first is wrong, some wrong file format
        
        id.header <- grep('Time.*Pressure.*Volume',d)                           # this is the line where the table starts
        
        dat <- read.table(x,skip=id.header-1,sep='\t',header=TRUE)              # read the raw data
        names(dat) <- unlist(strsplit(readLines(x)[id.header],'\t'))            # pretty column names
        dat$filename <- x                                                       # for tractability and merging with rest of data if necessary
        
        return(dat)
    })
    do.call('rbind',ret)
}
