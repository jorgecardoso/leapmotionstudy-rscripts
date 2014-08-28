


library(psych)
library(ggplot2)
library(doBy)
library(grid)
library(doParallel)
library(reshape2)


pathBreaks <- c(seq(0,1, by=.05))



filenameInTransformed <- paste("data/", "transformed-feup.txt", sep="")
filenameInMeasuresAlongPath <- paste("data/", "measuresAlongPath-feup.txt", sep="")

filenameOutWindowMEMOMV <- paste("data/", "measuresWindow-20-MEMOMV-feup.txt", sep="")
filenameOutWindowTRETACMDCODC <- paste("data/", "measuresWindow-20-TRETACMDCODC-feup.txt", sep="")




measuresAlongPath = read.csv(filenameInMeasuresAlongPath, sep="", head = TRUE)

transformed = read.csv(filenameInTransformed, sep="", head = TRUE,
                       colClasses = c("integer", #NumberDevice
                                      "integer", #UserId
                                      "integer", #Block
                                      "integer", #Sequence
                                      "NULL",    #NumberClicks
                                      "NULL",    #NumberCircles
                                      "integer", #CircleID
                                      rep("NULL", 11), #"DistanceCenter" "PixelStartCircleX" "PixelStartCircleY" "PixelEndCircleX" "PixelEndCircleY" "TargetWidth" "ElapsedTime" "MouseX" "MouseY" "transfTargetx" "transfTargety"
                                      "double", #rx
                                      "double", #ry
                                      "NULL", #inside
                                      "double", #percentpath
                                      rep("NULL", 5), #"speeds" "accels" "displacement" "distance" "insidecount"
                                      "double" #distanceToTarget
                       ) 
)





cl <- makeCluster(8)
registerDoParallel(cl)

# The function assumes that the data frame passed in its argument contains only the pointer sample for 
# one target selection
transform <- function (partial) {
    
    partial$cuts<-cut(partial$percentpath, breaks=pathBreaks, include.lowest=TRUE, right=TRUE)
    splits <- split(partial, partial$cuts)
    
    window <- data.frame()
    for (segment in splits) {
        
        
        MO <- sum(segment$ry) / nrow(segment)
        if ( nrow(segment) < 2 ) {
            MV = 0            
        } else {
            MV <- sqrt(sum((segment$ry-MO)*(segment$ry-MO))/(nrow(segment)-1))
        }
        ME <- sum(abs(segment$ry))/nrow(segment)
        
        window<-rbind(window, data.frame(NumberDevice = segment$NumberDevice[1],
                                         UserId = segment$UserId[1],
                                         Block = segment$Block[1],
                                         Sequence = segment$Sequence[1],
                                         CircleID = segment$CircleID[1],
                                         Cut = segment$cuts[1],
                                         MO = MO,
                                         MV = MV,
                                         ME = ME,
                                         SampleCount = nrow(segment)
        ) )
    }
    
    
    return(window)
}



# This function is executed as a parallel job, for calculating the transformed pointer coordinates for a given sequence.
transformationJob <- function(user, device, block) {
    newDataSequence <- data.frame()
    
    for (sequence in 1:max(transformed[transformed$UserId==user & 
                                           transformed$NumberDevice==device & 
                                           transformed$Block==block,]$Sequence) ) {
        
        for (cid in 1:max(transformed$CircleID) ) {
            
            # indexes for the current target selection
            sampleIndexes <- which(transformed$UserId == user & transformed$NumberDevice == device & 
                                       transformed$Block == block & transformed$Sequence == sequence & 
                                       transformed$CircleID == cid)
            
            # partial will hold the new calculated variables for the current selection path
            partial <- transform(transformed[sampleIndexes, ])
            newDataSequence <- rbind(newDataSequence, partial)
        }
    }
    return (newDataSequence)
}

print("Calculating window metrics...")
newData <- data.frame()
time = system.time(
    for (user in unique(transformed$UserId) ) {
        #for (user in 4 ) {
        
        newDataUser <- data.frame()
        
        for (device in unique(transformed[transformed$UserId==user,]$NumberDevice) ) {
            #for (device in unique(transformed$NumberDevice)[1] ) {
            
            print(paste("User:", user, " Device:", device, " Blocks:",  
                        max(transformed[transformed$UserId==user & transformed$NumberDevice==device,]$Block )))
            
            f <- foreach(block = 1:max(transformed[transformed$UserId==user & transformed$NumberDevice==device,]$Block),
                         .combine='rbind') %dopar% transformationJob(user, device, block)
            
            newDataUser <- rbind(newDataUser, f)
            
        }
        newData <- rbind(newData, newDataUser)
    }
)
print(time)


stopCluster(cl)

colnames(newData)[colnames(newData)=="NumberDevice"] <- "Device"
newData$Device <- factor(newData$Device)
levels(newData$Device) <- list( Mouse=c(1), Touchpad=c(2), LeapMotion=c(0), LeapMotionTouchless=c(4) )
newData$UserId <- factor(newData$UserId)
newData$Block <- factor(newData$Block)
newData$Sequence <- factor(newData$Sequence)
newData$CircleID <- factor(newData$CircleID)

write.table(newData, file = filenameOutWindowMEMOMV, sep=" ", row.names=FALSE)





    
    #set the factor columns
    measuresAlongPath$Device <- factor(measuresAlongPath$Device)
    levels(measuresAlongPath$Device) <- list( Mouse=c(1), Touchpad=c(2), LeapMotion=c(0), LeapMotionTouchless=c(4) )
    measuresAlongPath$UserId <- factor(measuresAlongPath$UserId)
    measuresAlongPath$Block <- factor(measuresAlongPath$Block)
    measuresAlongPath$Sequence <- factor(measuresAlongPath$Sequence)
    measuresAlongPath$CircleID <- factor(measuresAlongPath$CircleID)
    
    
    
    
    # create cuts for the distance
    measuresAlongPath$Cut<-cut(measuresAlongPath$PercentPath, breaks=pathBreaks, 
                                         include.lowest=TRUE, right=TRUE)
    
    
    # we need all levels complete so create a data frame with all combinations and merge
    all <- with(measuresAlongPath, expand.grid(Device = levels(Device), UserId = levels(UserId), 
                                                Block=levels(Block), Sequence=levels(Sequence), 
                                                CircleID=levels(CircleID), Cut=levels(Cut)))
    measuresAlongPath <- merge(measuresAlongPath, all, all.y = TRUE)
    measuresAlongPath$TACPercent[is.na(measuresAlongPath$TACPercent)] = 0
    measuresAlongPath$TREPercent[is.na(measuresAlongPath$TREPercent)] = 0
    measuresAlongPath$MDCPercent[is.na(measuresAlongPath$MDCPercent)] = 0
    measuresAlongPath$ODCPercent[is.na(measuresAlongPath$ODCPercent)] = 0
    
    measuresAlongPath$TACEvents[is.na(measuresAlongPath$TACEvents)] = 0
    measuresAlongPath$TREEvents[is.na(measuresAlongPath$TREEvents)] = 0
    measuresAlongPath$MDCEvents[is.na(measuresAlongPath$MDCEvents)] = 0
    measuresAlongPath$ODCEvents[is.na(measuresAlongPath$ODCEvents)] = 0
    
    



# Compute the aggregate means for each variable.
# We agregate all values for the various circles

aggDataEventsDistribution <-aggregate(measuresAlongPath[ ,c("TACEvents", "TREEvents", "MDCEvents", "ODCEvents")], 
                                      measuresAlongPath[ ,c("Device", "UserId", "Block", "Sequence", "CircleID", "Cut")], sum)

write.table(aggDataEventsDistribution, file = filenameOutWindowTRETACMDCODC, sep=" ", row.names=FALSE)

rm(list = ls(all = TRUE)) 