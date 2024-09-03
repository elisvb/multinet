##' Read in multinet meta data
##' @param x vector of text file names from oceanLabs (character)
##' @param tz time zone (character, UTC by default)
##' @param plot logical. plot selected depths?
##' @param min.dive.depth default 5. minimum depth (meters) for a dive to be recognised as a dive (rather than surface troubles)
##' @param encoding encoding ("latin1","UTF-8",""unknown", etc.)
##' @import data.table
##' @details ...
read.multimeta <- function(file,tz='utc',plot=FALSE,min.dive.depth=5,encoding="latin1"){
    require(data.table)                                         # delete if packaged
    ret <- lapply(file,function(x){
        print(x)
        d <- readLines(x, encoding = encoding)                                                  # read all lines as vector elements
        if(!grepl('Data Start: ',d[1])) stop('x is not a multinet dat file')    # if the first is wrong, some wrong file format
        
        cols <- c('Data Start','Date ','Time UTC','Cruise','Ship','Station','Consecutive','Bottom depth','Day/Night','Latitude','Longitude') # columns we want
        maxchar <- max(nchar(cols))
        id.cols <- unlist(sapply(cols,grep,substr(d,1,maxchar),ignore.case=TRUE))   # line numbers of those columns
        id.cols <- id.cols[id.cols < grep('Notes',d)]                          # Because any of the column names might be at the start of the notes, exclude them and add notes after separately. 
        id.cols <- c(id.cols,Notes=grep('Notes:',d))                           # add the notes
        
        sub <- d[id.cols]                                      # take only those lines
        sub <- gsub('\t',' ',sub)                              # small correction for when whitespaces are read in as \t
        sub2 <- sapply(strsplit(sub,': '),'[',2)               # keep the stuff after ": "
        if(any(is.na(sub2))) sub2[which(is.na(sub2))] <- sapply(strsplit(sub[which(is.na(sub2))],':'),'[',2)  # if there is a line with no space after :, correct his
        sub2 <- trimws(sub2)                                     # remove white spaces before and after
        
        dat <- data.frame(matrix(sub2,nrow=1))                  # convert this to a data.frame
        names(dat) <- names(id.cols)
        
        dat$filename <- x                                      # for tractability and merging with rest of data if necessary
        
        dat[,1] <- as.POSIXct(dat[,1],tz=tz)                   # correct class

        raw  <- read.multidat(x)                             # read in the table and extract info by net
        raw$id <- 1:nrow(raw)
        
        # remove the 0 net and any other one that is going down (e.g. sometimes the third net if two dives)
        upd <- tapply(raw$`Pressure [dbar]`, raw$`Net []`, function(x){ifelse(x[1]>x[length(x)],'up','down')}) # nets up or down
        raw$updown <- upd[raw$`Net []`+1]                                                                      # add to dataframe
        raw.dive <- raw[raw$updown=='up',]                                                                     # keep only the descending ones
        
        # remove nets that were opened and closed at the surface (shallower than min.dive.depth)
        depth.max <- tapply(raw.dive$`Pressure [dbar]`, raw.dive$`Net []`, max) 
        nets.max <- as.numeric(names(depth.max[depth.max>min.dive.depth]))
        raw.dive <- raw.dive[raw.dive$`Net []` %in% nets.max,] 
        
        # remove above surface
        raw.dive <- raw.dive[raw.dive$`Pressure [dbar]`>=0,]
        
        # remove when the net went down a bit after having surfaced
        replunge <- tapply(raw.dive$id, raw.dive$`Net []`, function(x){ifelse(length(unique(diff(x)))==1,FALSE,TRUE)}) # did this happen for any net?
        if(any(replunge)){                                                                                                                  # if this happens
            for(i in as.numeric(names(replunge[replunge]))){                                                                                # for those nets , delete the second time below water
                y <- diff(raw.dive[raw.dive$`Net []`==i,"id"])                                                                              # where is there a jump
                toremove <- which(y>1)[1]                                                                                                      # start of the jump
                toremove <- c(toremove:length(y))+1                                                                                         # start till end
                toremove <- raw.dive[raw.dive$`Net []`==i,][toremove,]$id                                                                   # id of those lines
                raw.dive <- raw.dive[!raw.dive$id %in% toremove,]                                                                           # delete them
            }
        }

        # plots
        if(plot){
            plot(raw$id,-raw$`Pressure [dbar]`,main=x,xlab='time',ylab='depth',pch=16)
            colfunc <- colorRampPalette(c("red","green"))
            nets <- unique(raw.dive$`Net []`)
            thesecols <- colfunc(length(nets))
            for(i in nets) points(raw.dive[raw.dive$`Net []`==i,]$id,-raw.dive[raw.dive$`Net []`==i,]$`Pressure [dbar]`,col=thesecols[which(nets==i)],pch=16)
         } 
        
        # calculate some basic statistics
        extra  <- data.frame(
            Net         = as.numeric(unique(raw.dive$Net)),
            Depth.min   = tapply(raw.dive$`Pressure [dbar]`, raw.dive$`Net []`, min), # min depth per net
            Depth.max   = tapply(raw.dive$`Pressure [dbar]`, raw.dive$`Net []`, max), # max depth per net
            Time.start  = tapply(raw.dive$`Time [hh:mm:ss]`, raw.dive$`Net []`, min),  # start time per net
            Time.end    = tapply(raw.dive$`Time [hh:mm:ss]`, raw.dive$`Net []`, max),  # end time per net
            Volume      = tapply(raw.dive$`Volume [m³]`, raw.dive$`Net []`, max)) # total volume filtered
        
        dat <- cbind(dat,extra)
        if(ncol(dat)!=19) warning(paste0('missing column/information for file: ',x))
        return(dat)
    })
    as.data.frame(rbindlist(ret,fill=TRUE))
}

##' Read in multinet tabular data
##' @param file vector of text file names from oceanLabs (character)
##' @param encoding encoding ("latin1","UTF-8",""unknown", etc.)
##' @import data.table
##' @details ...
read.multidat <- function(file,encoding="latin1"){
    require(data.table)                                         # delete if packaged
    ret <- lapply(file,function(x){
        d <- readLines(x, encoding=encoding)                                                       # read all lines as vector elements
        if(!grepl('Data Start: ',d[1])) stop('x is not a multinet dat file')    # if the first is wrong, some wrong file format
        
        id.header <- grep('Time.*Pressure.*Volume',d)                           # this is the line where the table starts
        
        dat <- read.table(x,skip=id.header-1,sep='\t',header=TRUE,encoding = encoding)              # read the raw data
        names(dat) <- unlist(strsplit(readLines(x, encoding=encoding)[id.header],'\t'))            # pretty column names
        dat$filename <- x                                                       # for tractability and merging with rest of data if necessary
        
        return(dat)
    })
    as.data.frame(rbindlist(ret))
}
