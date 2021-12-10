##' Read in multinet meta data
##' @param x vector of text file names from oceanLabs (character)
##' @param tz time zone (character, UTC by default)
##' @param plot logical. plot selected depths?
##' @import data.table
##' @details ...
read.multimeta <- function(file,tz='utc',plot=FALSE){
    require(data.table)                                         # delete if packaged
    ret <- lapply(file,function(x){
        print(x)
        d <- readLines(x)                                                       # read all lines as vector elements
        if(!grepl('Data Start: ',d[1])) stop('x is not a multinet dat file')    # if the first is wrong, some wrong file format
        
        cols <- c('Data Start','Date ','Time UTC','Cruise','Ship','Station','Consecutive','Bottom depth','Day/Night','Latitude','Longitude') # columns we want
        id.cols <- unlist(sapply(cols,grep,d,ignore.case=TRUE))                # line numbers of those columns
        id.cols <- id.cols[id.cols < grep('Notes',d)]         # exclude lines after the notes (in case they contain a string in cols)
        
        sub <- d[id.cols]                                      # take only those lines
        sub <- gsub('\t',' ',sub)                              # small correction for when whitespaces are read in as \t
        sub2 <- sapply(strsplit(sub,': '),'[',2)              # keep the stuff after ": "
        if(any(is.na(sub2))) sub2[which(is.na(sub2))] <- sapply(strsplit(sub[which(is.na(sub2))],':'),'[',2)  # if there is a line with no space after :, correct his
        sub2 <- trimws(sub2)                                     # remove white spaces before and after
        
        dat <- data.frame(matrix(sub2,nrow=1))                  # convert this to a data.frame
        names(dat) <- cols
        
        dat$filename <- x                                      # for tractability and merging with rest of data if necessary
        
        dat[,1] <- as.POSIXct(dat[,1],tz=tz)                   # correct class
        
        ## all need to be in UTC
        ## net = 0 needs to be removed.
        ## delete all depths negative before calcs.
        ## volume:volume at depth max, minus the volume at negative depths.
        ## delete negatives untill consistent line of positives (if it replunges it does nto count)
        
        raw  <- read.multidat(x)                             # read in the table and extract info by net
        raw$id <- 1:nrow(raw)
        
        # remove lines where not diving (before or after)
        if(plot) plot(raw$id,raw$`Pressure [dbar]`,main=x,xlab='time',ylab='depth')
        check.diving <- data.frame(unclass(rle(raw$`Pressure [dbar]`>=0)))  # table with seconds when diving (below surface) and when its not
        start.id <- which(check.diving$values==TRUE & check.diving$lengths==max(check.diving[check.diving$values,]$lengths)) # identify when the longest continuous dive is
        start <- sum(check.diving[0:(start.id-1),'lengths'])+1  # row where the dive starts
        end <- sum(check.diving[1:(start.id),'lengths'])        # row where the dive ends
        raw.dive <- raw[c(start:end),]                          # select only rows when diving

        # start
        raw.dive <- raw.dive[raw.dive$`Net []`!=0,]
        if(plot) points(raw.dive$id,raw.dive$`Pressure [dbar]`,col='red')
        
        # calculate some basic statistics
        extra  <- data.frame(
            Net         = as.numeric(unique(raw.dive$Net)),
            Depth.min   = tapply(raw.dive$`Pressure [dbar]`, raw.dive$`Net []`, min), # min depth per net
            Depth.max   = tapply(raw.dive$`Pressure [dbar]`, raw.dive$`Net []`, max), # max depth per net
            Time.start  = tapply(raw.dive$`Time [hh:mm:ss]`, raw.dive$`Net []`, min),  # start time per net
            Time.end    = tapply(raw.dive$`Time [hh:mm:ss]`, raw.dive$`Net []`, max),  # end time per net
            Volume      = tapply(raw.dive$`Volume [m³]`, raw.dive$`Net []`, max)) # total volume filtered
        
        dat <- cbind(dat,extra)
        if(ncol(dat)!=18) warning(paste0('missing column/information for file: ',x))
        return(dat)
    })
    as.data.frame(rbindlist(ret))
}

##' Read in multinet tabular data
##' @param file vector of text file names from oceanLabs (character)
##' @import data.table
##' @details ...
read.multidat <- function(file){
    require(data.table)                                         # delete if packaged
    ret <- lapply(file,function(x){
        d <- readLines(x)                                                       # read all lines as vector elements
        if(!grepl('Data Start: ',d[1])) stop('x is not a multinet dat file')    # if the first is wrong, some wrong file format
        
        id.header <- grep('Time.*Pressure.*Volume',d)                           # this is the line where the table starts
        
        dat <- read.table(x,skip=id.header-1,sep='\t',header=TRUE)              # read the raw data
        names(dat) <- unlist(strsplit(readLines(x)[id.header],'\t'))            # pretty column names
        dat$filename <- x                                                       # for tractability and merging with rest of data if necessary
        
        return(dat)
    })
    as.data.frame(rbindlist(ret))
}
