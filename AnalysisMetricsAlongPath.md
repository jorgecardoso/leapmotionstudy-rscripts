Title
========================================================


```r
require(psych)
```

```
## Loading required package: psych
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
## 
## Attaching package: 'ggplot2'
## 
## The following object is masked from 'package:psych':
## 
##     %+%
```

```r
require(doBy)
```

```
## Loading required package: doBy
## Loading required package: survival
## Loading required package: splines
## Loading required package: MASS
```

```r
require(grid)
```

```
## Loading required package: grid
```

```r
library(doParallel)
```

```
## Loading required package: foreach
## Loading required package: iterators
## Loading required package: parallel
```

```r
library(reshape2)
```



```r
distanceBreaks <- c(seq(0,0.9, by=.1), 1)
filenameMeasuresAlongPath <- paste("data/", "measuresAlongPath-feup.txt", sep="")
filenameMeasuresAlongPathClean <- paste("data/", "measuresAlongPathClean-feup.txt", sep="")
filenameprefix <- "blocks4-7"

# if we have already saved the clean data file, just load it and skip to the aggregation
fileExists <- file.exists(filenameMeasuresAlongPathClean)
fileExists <- FALSE

if ( !fileExists) {
    measuresAlongPath1 = read.csv(filenameMeasuresAlongPath, sep="", head = TRUE)
    
    #set the factor columns
    measuresAlongPath1$Device <- factor(measuresAlongPath1$Device)
    levels(measuresAlongPath1$Device) <- list( Mouse=c(1), LeapMotion=c(0), LeapMotionTouchless=c(4) )
    measuresAlongPath1$UserId <- factor(measuresAlongPath1$UserId)
    measuresAlongPath1$Block <- factor(measuresAlongPath1$Block)
    measuresAlongPath1$Sequence <- factor(measuresAlongPath1$Sequence)
    measuresAlongPath1$CircleID <- factor(measuresAlongPath1$CircleID)
    
    
    
    # We will consider only blocks 4 to 7 in the rest of the analysis, see AnalysisMackenzieMetrics for learning effect analysis
    
    measuresAlongPath1 <- measuresAlongPath1[as.numeric(measuresAlongPath1$Block) > 3 & 
                                                 as.numeric(measuresAlongPath1$Block) < 8 ,]
    
    # Drop unused block levels
    measuresAlongPath1$Block <- factor(measuresAlongPath1$Block)
    

    
    
    # create cuts for the distance
    measuresAlongPath1$CutsDistance<-cut(measuresAlongPath1$Distance, breaks=distanceBreaks, include.lowest=TRUE, right=TRUE)
    
    
    # we need all levels complete so create a data frame with all combinations and merge
    all <- with(measuresAlongPath1, expand.grid(Device = levels(Device), UserId = levels(UserId), 
                                                Block=levels(Block), Sequence=levels(Sequence), 
                                                CircleID=levels(CircleID), CutsDistance=levels(CutsDistance)))
    measuresAlongPath1 <- merge(measuresAlongPath1, all, all.y = TRUE)
    measuresAlongPath1$TACPercent[is.na(measuresAlongPath1$TACPercent)] = 0
    measuresAlongPath1$TREPercent[is.na(measuresAlongPath1$TREPercent)] = 0
    measuresAlongPath1$MDCPercent[is.na(measuresAlongPath1$MDCPercent)] = 0
    measuresAlongPath1$ODCPercent[is.na(measuresAlongPath1$ODCPercent)] = 0
    
    measuresAlongPath1$TACEvents[is.na(measuresAlongPath1$TACEvents)] = 0
    measuresAlongPath1$TREEvents[is.na(measuresAlongPath1$TREEvents)] = 0
    measuresAlongPath1$MDCEvents[is.na(measuresAlongPath1$MDCEvents)] = 0
    measuresAlongPath1$ODCEvents[is.na(measuresAlongPath1$ODCEvents)] = 0
    
    
    # just for testing
    #measuresAlongPath <- measuresAlongPath1[ measuresAlongPath1$Device=="LeapMotionTouchless" & measuresAlongPath1$UserId==0 & measuresAlongPath1$Block==1 & measuresAlongPath1$Sequence==1 ,]
    #measuresAlongPath <- measuresAlongPath1[ measuresAlongPath1$Device=="LeapMotionTouchless" & measuresAlongPath1$UserId==0 & measuresAlongPath1$Block==1  ,]
    measuresAlongPath <- measuresAlongPath1
    
    #####remove circleIds with zero events
    #check if file with clean data exists
#     getIndexes <- function(zeroes, i) {
#         return (which(measuresAlongPath$Device==zeroes[i,]$Device & measuresAlongPath$UserId==zeroes[i,]$UserId & measuresAlongPath$Block==zeroes[i,]$Block & measuresAlongPath$Sequence==zeroes[i,]$Sequence & measuresAlongPath$CircleID==zeroes[i,]$CircleID))
#     }
#     
# 
#     cl <- makeCluster(8)
#     registerDoParallel(cl)
#     
#     zeroes<-aggregate(measuresAlongPath[ ,DVars], 
#                       measuresAlongPath[ ,c("Device", "UserId", "Block", "Sequence", "CircleID")], sum)
#     zeroes<-zeroes[zeroes$x==0,]
#     indexes<-numeric()
#     for ( user in unique(zeroes$UserId) ) {
#         print(paste(user))
#         f <- foreach(i = 1:nrow(zeroes[zeroes$UserId==user,]), .combine='c') %dopar% {
#             getIndexes(zeroes[zeroes$UserId==user,], i)
#         }
#         measuresAlongPath <- measuresAlongPath[-f,]
#     }
#     stopCluster(cl)
    
    write.table(measuresAlongPath, file = filenameMeasuresAlongPathClean, sep=" ", row.names=FALSE)
} else {
    measuresAlongPath = read.csv(filenameMeasuresAlongPathClean, sep="", head = TRUE)
}



# Compute the aggregate means for each variable.
# We agregate all values for the various circles
#aggDataEventsProbabilityDistribution <-aggregate(measuresAlongPath[ ,c("TACPercent", "TREPercent", "MDCPercent", "ODCPercent")], 
#                       measuresAlongPath[ ,c("Device", "UserId", "Block", "Sequence", "CircleID", "CutsDistance")], sum)
 
aggDataEventsDistribution <-aggregate(measuresAlongPath[ ,c("TACEvents", "TREEvents", "MDCEvents", "ODCEvents")], 
                                      measuresAlongPath[ ,c("Device", "UserId", "Block", "Sequence", "CircleID", "CutsDistance")], sum)

#Correct TRE Events count because the script the creates the measuresalongpath file wrongly counts a TRE in the first target entry
aggDataEventsDistribution$TREEvents = ifelse(aggDataEventsDistribution$TREEvents > 0, aggDataEventsDistribution$TREEvents-1, 0)

#summary(aggDataEventsDistribution$TREEvents)
```




```r
# 
#  aggData<-aggregate( aggDataEventsProbabilityDistribution[ , c("TREPercent","TACPercent",  "MDCPercent", "ODCPercent")], aggDataEventsProbabilityDistribution[ , c("Device", "CutsDistance")], mean)
#  
#  aggData<-melt(aggData, id.vars=1:2)
#  
#  ggplot(aggData, aes(x=CutsDistance, y=value, group=Device, colour=Device)) +
#      geom_bar(stat="identity") + 
#      facet_wrap( Device ~ variable, nrow=3) 
#      #geom_line()



aggData<-aggregate( aggDataEventsDistribution[ , c("TREEvents","TACEvents",  "MDCEvents", "ODCEvents")], 
                     aggDataEventsDistribution[ , c("Device", "CutsDistance")], mean)

aggData<-melt(aggData, id.vars=1:2)
#reverse the cutsdistance factor so we get a plot with minimum distance on the right
aggData$CutsDistance <- factor(aggData$CutsDistance, rev(levels(aggData$CutsDistance)))


for ( event in c("TREEvents","TACEvents",  "MDCEvents", "ODCEvents") ) {
    p <- ggplot(aggData[aggData$variable==event,], aes(x=CutsDistance, y=value, group=Device, colour=Device, fill=Device)) + 
        #stat_summary(fun.y="mean", geom="bar") + 
        geom_bar(stat="identity") + #, width=.5, position = position_dodge(width=0.5)) +
        facet_wrap( ~ Device, nrow=3)  + 
        #geom_bar() +
        #coord_flip() +
        ylab(event) +
        xlab("Distance to target (%)") +
        #theme(legend.position=c(.5,-0.1), legend.direction="horizontal", 
        #      axis.text.x = element_blank()) + 
        scale_x_discrete(breaks=c(levels(aggData$CutsDistance)[1], 
                                  levels(aggData$CutsDistance)[length(levels(aggData$CutsDistance))]), labels=c("100%", "0%")) +
        scale_fill_brewer(palette="Set1") + 
        scale_colour_brewer(palette="Set1") 
    
     print(p)
    
    ggsave(file = paste("charts/",filenameprefix,"-metricsalongpath-",event,".pdf", sep=""), width=30/2.54, height=16/2.54, dpi=100)
}
```

![plot of chunk analysis-measures-alongpath](figure/analysis-measures-alongpath1.png) ![plot of chunk analysis-measures-alongpath](figure/analysis-measures-alongpath2.png) ![plot of chunk analysis-measures-alongpath](figure/analysis-measures-alongpath3.png) ![plot of chunk analysis-measures-alongpath](figure/analysis-measures-alongpath4.png) 
