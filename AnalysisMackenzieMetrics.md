

Analisys of the Leap Motion experiment at UCP
========================================================


This document provides the analysis of the experiment conducted at UCP/CITAR comparing the movement of three devices - Mouse, Touchpad, and Leap Motion (via Touchless app) - for pointing tasks. 

We use Maczenzie's accuracy metrics to compare the devices.


```r
PRODUCE_BULK_CHARTS = FALSE
PRODUCE_BULK_HIST_CHARTS = TRUE
PRODUCE_BULK_CHARTS_DEVICE_PLUS_USERID = FALSE

#install.packages(c("ggplot2", "doBy", "psych", "car", "xtable"))
require(ggplot2)
require(doBy)
require(psych)
require(car)
require(xtable)
source("functions.R")

# load metrics data
data <- read.csv(file="data/measures-feup.txt", head=TRUE, sep="")

# Users 0 and 5 did not complete the experiment
data <- data[ data$UserId != 5,]

# Only blocks <= 7
data <- data[ data$Block <= 7,]

# convert the column to factor and name the levels
data$Device <- as.factor(data$Device)

# Set the levels 
# LeapMotion=c(0),  Mouse=c(1), Touchpad=c(2), LeapMotionTouchless=c(4)
# LeapMotion does not exist in the dataset from the UCP experiment
levels(data$Device) <- list(Mouse=c(1), LeapMotion=c(0), LeapMotionTouchless=c(4))

# convert the block column to factor
data$Block <- factor(data$Block)

# In this analysis we use ErrorRateBinary, so we drop the ErrorRate column and change the column name ErrorRateBinary to ErrorRate
drops <- c("ErrorRate")
data<-data[,!(names(data) %in% drops)]
names(data)[names(data) == "ErrorRateBinary"] <- "ErrorRate"

# Compute the aggregate means for each variable.
# We agregate all values for the various circles
IVars <- c("Device", "Block", "Sequence", "UserId")
DVars <- c("Throughput", "ErrorRate", "MovementTime", "TRE", "TAC", "ODC", "MDC", "MV", "ME", "MO")
aggData <-aggregate(data[ ,DVars], data[ ,IVars], mean)
aggData$Block <- factor(aggData$Block)
```

Movement time as function of block
-------------------------------------

```r
describe.MT.aggData <- describeBy(aggData$MovementTime, aggData[,c("Block")])
describe.MT.aggData <-do.call("rbind", describe.MT.aggData)
describe.MT.aggData <- cbind(Block=rownames(describe.MT.aggData), describe.MT.aggData)
describe.MT.aggData$Block <- factor(describe.MT.aggData$Block)

p <- ggplot(describe.MT.aggData, aes(x=Block, y=mean )) + 
    geom_point(stat="identity") +
    geom_line(aes(group=vars)) + 
    ylab("Movement time (seconds) with 95% conf int") +
    #xlab("Sequence") +
    #ggtitle("Throughput") +
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), colour="Black",
                  width=.2                    # Width of the error bars
                  ) +
    theme(legend.position="none") +
    theme() 
p
```

![plot of chunk movement-time-block](figure/movement-time-block1.png) 

```r
p <- ggplot(aggData, aes_string(x="Block", y="MovementTime", group="Device", colour="Device" )) + 
            stat_summary(fun.y="mean", geom="line") + 
            stat_summary(fun.y="mean", geom="point", aes(shape=Device)) + 
            theme(legend.direction = "horizontal", legend.position = "top") +
            #ylab("Percentage (%)") +
            #xlab("Sequence") +
            #ggtitle(var) +
            scale_fill_brewer(palette="Set1") + 
            scale_colour_brewer(palette="Set1") +
            theme() #noop
p
```

![plot of chunk movement-time-block](figure/movement-time-block2.png) 

```r
ggsave(file = paste("charts/","all-movementtime-block-lineplot.pdf", sep=""), 
               width=13/2.54, height=7/2.54, dpi=100)
```

<!-- a comment here 

-->

Throughput  as function of block
-------------------------------------

```r
describe.T.aggData <- describeBy(aggData$Throughput, aggData[,c("Block")])
describe.T.aggData <-do.call("rbind", describe.T.aggData)
describe.T.aggData <- cbind(Block=rownames(describe.T.aggData), describe.T.aggData)
describe.T.aggData$Block <- factor(describe.T.aggData$Block)

p <- ggplot(describe.T.aggData, aes(x=Block, y=mean )) + 
    geom_point(stat="identity") +
    geom_line(aes(group=vars)) + 
    ylab("Throughput (bps) with 95% conf int") +
    #xlab("Sequence") +
    #ggtitle("Throughput") +
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), colour="Black",
                  width=.2                    # Width of the error bars
                  ) +
    theme(legend.position="none") +
    theme() 
p
```

![plot of chunk throughput-block](figure/throughput-block1.png) 

```r
p <- ggplot(aggData, aes_string(x="Block", y="Throughput", group="Device", colour="Device" )) + 
            stat_summary(fun.y="mean", geom="line") + 
            stat_summary(fun.y="mean", geom="point", aes(shape=Device)) + 
            theme(legend.direction = "horizontal", legend.position = "top") +
            #ylab("Percentage (%)") +
            #xlab("Sequence") +
            #ggtitle(var) +
            theme() #noop
p
```

![plot of chunk throughput-block](figure/throughput-block2.png) 

```r
ggsave(file = paste("charts/","all-throughput-block-lineplot.pdf", sep=""), 
               width=20/2.54, height=16/2.54, dpi=100)
```

Error rate  as function of block
-------------------------------------

```r
p <- ggplot(aggData, aes_string(x="Block", y="ErrorRate", group="Device", colour="Device" )) + 
            stat_summary(fun.y="mean", geom="line") + 
            stat_summary(fun.y="mean", geom="point", aes(shape=Device)) + 
            theme(legend.direction = "horizontal", legend.position = "top") +
            #ylab("Percentage (%)") +
            #xlab("Sequence") +
            #ggtitle(var) +
            theme() #noop
p
```

![plot of chunk ErrorRate-block](figure/ErrorRate-block.png) 

```r
ggsave(file = paste("charts/","all-throughput-block-lineplot.pdf", sep=""), 
               width=20/2.54, height=16/2.54, dpi=100)
```

Learning effect
-------------------------
To estimate the learning effect, we ran pairwise t-tests for average throughput per block (considering all devices) with a significance level of 5%:


```r
#describeBy(data$MovementTime, data$Block)

# pairwise t-tests to determine learning effect blocks
p.t.test <- pairwise.t.test(aggData$Throughput, aggData$Block, paired=T, p.adjust.method="none")
diag(p.t.test$p.value) 
```

```
## [1] 1.025e-04 9.962e-07 3.124e-04 6.753e-01 9.812e-01 4.457e-05
```

```r
diag(p.t.test$p.value) < 0.05
```

```
## [1]  TRUE  TRUE  TRUE FALSE FALSE  TRUE
```

```r
# We will consider only blocks 4 to 8 in the rest of the analysis
filenameprefix <- "blocks4-7"
aggData.noLearn <- aggData[as.numeric(aggData$Block) > 3,]

# Drop unused block levels
aggData.noLearn$Block <- factor(aggData.noLearn$Block)
```

The results indicate a clear learning effect in blocks 1 to 3, but also indicate a significant different between blocks 6 and 7, suggesting that participants were still learning after block 6. However, in our following analysis we discard only blocks 1 to 3, since those represent the most significant learning effect.





Summary statistics 
------------


```r
options(width = 200)

# describe the main variables and store in file
s<-describeBy(aggData.noLearn[, DVars], aggData.noLearn$Device, mat=FALSE, digits=2)
s <- do.call("rbind", s)

print(xtable(s), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sun Aug 24 10:37:17 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> vars </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> skew </TH> <TH> kurtosis </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> Mouse.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 0.59 </TD> <TD align="right"> 4.98 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 0.63 </TD> <TD align="right"> 3.68 </TD> <TD align="right"> 6.49 </TD> <TD align="right"> 2.82 </TD> <TD align="right"> 0.13 </TD> <TD align="right"> -0.47 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> Mouse.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.03 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 1.17 </TD> <TD align="right"> 1.22 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> Mouse.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 0.81 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 0.81 </TD> <TD align="right"> 0.11 </TD> <TD align="right"> 0.61 </TD> <TD align="right"> 1.10 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.58 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> Mouse.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> -0.52 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> Mouse.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 1.61 </TD> <TD align="right"> 0.34 </TD> <TD align="right"> 1.60 </TD> <TD align="right"> 1.60 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 2.60 </TD> <TD align="right"> 1.80 </TD> <TD align="right"> 0.32 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.03 </TD> </TR>
  <TR> <TD align="right"> Mouse.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 1.17 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 1.07 </TD> <TD align="right"> 1.15 </TD> <TD align="right"> 0.59 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 2.47 </TD> <TD align="right"> 2.27 </TD> <TD align="right"> 0.45 </TD> <TD align="right"> -0.72 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> Mouse.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 4.26 </TD> <TD align="right"> 0.85 </TD> <TD align="right"> 4.07 </TD> <TD align="right"> 4.20 </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 2.47 </TD> <TD align="right"> 6.87 </TD> <TD align="right"> 4.40 </TD> <TD align="right"> 0.58 </TD> <TD align="right"> -0.02 </TD> <TD align="right"> 0.06 </TD> </TR>
  <TR> <TD align="right"> Mouse.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 22.62 </TD> <TD align="right"> 7.08 </TD> <TD align="right"> 21.73 </TD> <TD align="right"> 22.27 </TD> <TD align="right"> 7.82 </TD> <TD align="right"> 9.51 </TD> <TD align="right"> 48.22 </TD> <TD align="right"> 38.72 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 0.16 </TD> <TD align="right"> 0.53 </TD> </TR>
  <TR> <TD align="right"> Mouse.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 20.09 </TD> <TD align="right"> 5.56 </TD> <TD align="right"> 19.90 </TD> <TD align="right"> 19.91 </TD> <TD align="right"> 5.78 </TD> <TD align="right"> 8.99 </TD> <TD align="right"> 41.02 </TD> <TD align="right"> 32.03 </TD> <TD align="right"> 0.40 </TD> <TD align="right"> 0.19 </TD> <TD align="right"> 0.41 </TD> </TR>
  <TR> <TD align="right"> Mouse.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> -2.46 </TD> <TD align="right"> 6.63 </TD> <TD align="right"> -2.29 </TD> <TD align="right"> -2.48 </TD> <TD align="right"> 6.29 </TD> <TD align="right"> -18.62 </TD> <TD align="right"> 21.81 </TD> <TD align="right"> 40.43 </TD> <TD align="right"> 0.26 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 0.49 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 2.80 </TD> <TD align="right"> 0.57 </TD> <TD align="right"> 2.77 </TD> <TD align="right"> 2.79 </TD> <TD align="right"> 0.60 </TD> <TD align="right"> 1.59 </TD> <TD align="right"> 4.21 </TD> <TD align="right"> 2.61 </TD> <TD align="right"> 0.21 </TD> <TD align="right"> -0.67 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 0.11 </TD> <TD align="right"> 0.12 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.60 </TD> <TD align="right"> 0.60 </TD> <TD align="right"> 1.33 </TD> <TD align="right"> 1.80 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 1.69 </TD> <TD align="right"> 0.55 </TD> <TD align="right"> 1.55 </TD> <TD align="right"> 1.61 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.12 </TD> <TD align="right"> 3.13 </TD> <TD align="right"> 1.69 </TD> <TD align="right"> 3.23 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 0.25 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.34 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 1.06 </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 1.92 </TD> <TD align="right"> 0.67 </TD> <TD align="right"> 1.80 </TD> <TD align="right"> 1.86 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 3.87 </TD> <TD align="right"> 0.90 </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 3.61 </TD> <TD align="right"> 1.78 </TD> <TD align="right"> 3.13 </TD> <TD align="right"> 3.35 </TD> <TD align="right"> 1.28 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 11.00 </TD> <TD align="right"> 10.20 </TD> <TD align="right"> 1.62 </TD> <TD align="right"> 3.24 </TD> <TD align="right"> 0.13 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 7.33 </TD> <TD align="right"> 2.48 </TD> <TD align="right"> 6.67 </TD> <TD align="right"> 7.01 </TD> <TD align="right"> 1.93 </TD> <TD align="right"> 3.47 </TD> <TD align="right"> 17.07 </TD> <TD align="right"> 13.60 </TD> <TD align="right"> 1.26 </TD> <TD align="right"> 1.57 </TD> <TD align="right"> 0.19 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 26.40 </TD> <TD align="right"> 13.37 </TD> <TD align="right"> 22.27 </TD> <TD align="right"> 24.32 </TD> <TD align="right"> 8.52 </TD> <TD align="right"> 8.53 </TD> <TD align="right"> 88.30 </TD> <TD align="right"> 79.77 </TD> <TD align="right"> 1.68 </TD> <TD align="right"> 3.37 </TD> <TD align="right"> 1.00 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 21.11 </TD> <TD align="right"> 10.79 </TD> <TD align="right"> 18.25 </TD> <TD align="right"> 19.36 </TD> <TD align="right"> 6.31 </TD> <TD align="right"> 7.14 </TD> <TD align="right"> 80.19 </TD> <TD align="right"> 73.05 </TD> <TD align="right"> 2.42 </TD> <TD align="right"> 7.96 </TD> <TD align="right"> 0.80 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> -1.57 </TD> <TD align="right"> 8.06 </TD> <TD align="right"> -1.97 </TD> <TD align="right"> -2.07 </TD> <TD align="right"> 5.38 </TD> <TD align="right"> -25.27 </TD> <TD align="right"> 39.38 </TD> <TD align="right"> 64.65 </TD> <TD align="right"> 1.29 </TD> <TD align="right"> 5.43 </TD> <TD align="right"> 0.60 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 2.31 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 2.23 </TD> <TD align="right"> 2.28 </TD> <TD align="right"> 0.42 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 3.75 </TD> <TD align="right"> 2.48 </TD> <TD align="right"> 0.56 </TD> <TD align="right"> -0.08 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 1.40 </TD> <TD align="right"> 2.08 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 1.94 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 1.85 </TD> <TD align="right"> 1.90 </TD> <TD align="right"> 0.44 </TD> <TD align="right"> 1.04 </TD> <TD align="right"> 4.10 </TD> <TD align="right"> 3.06 </TD> <TD align="right"> 0.93 </TD> <TD align="right"> 1.49 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 2.53 </TD> <TD align="right"> 2.53 </TD> <TD align="right"> 2.64 </TD> <TD align="right"> 13.98 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 2.24 </TD> <TD align="right"> 0.67 </TD> <TD align="right"> 2.13 </TD> <TD align="right"> 2.18 </TD> <TD align="right"> 0.59 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.33 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 1.19 </TD> <TD align="right"> 2.76 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 4.20 </TD> <TD align="right"> 2.27 </TD> <TD align="right"> 3.87 </TD> <TD align="right"> 3.98 </TD> <TD align="right"> 2.37 </TD> <TD align="right"> 1.07 </TD> <TD align="right"> 15.67 </TD> <TD align="right"> 14.60 </TD> <TD align="right"> 1.26 </TD> <TD align="right"> 3.20 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 8.37 </TD> <TD align="right"> 2.76 </TD> <TD align="right"> 8.13 </TD> <TD align="right"> 8.13 </TD> <TD align="right"> 2.72 </TD> <TD align="right"> 3.80 </TD> <TD align="right"> 22.40 </TD> <TD align="right"> 18.60 </TD> <TD align="right"> 1.19 </TD> <TD align="right"> 3.02 </TD> <TD align="right"> 0.21 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 21.87 </TD> <TD align="right"> 7.01 </TD> <TD align="right"> 20.08 </TD> <TD align="right"> 20.89 </TD> <TD align="right"> 4.93 </TD> <TD align="right"> 11.89 </TD> <TD align="right"> 51.25 </TD> <TD align="right"> 39.36 </TD> <TD align="right"> 1.71 </TD> <TD align="right"> 3.76 </TD> <TD align="right"> 0.52 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> 17.73 </TD> <TD align="right"> 5.48 </TD> <TD align="right"> 16.53 </TD> <TD align="right"> 16.89 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 9.71 </TD> <TD align="right"> 43.75 </TD> <TD align="right"> 34.04 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 5.55 </TD> <TD align="right"> 0.41 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 180.00 </TD> <TD align="right"> -1.81 </TD> <TD align="right"> 5.18 </TD> <TD align="right"> -1.99 </TD> <TD align="right"> -1.78 </TD> <TD align="right"> 4.53 </TD> <TD align="right"> -22.26 </TD> <TD align="right"> 14.59 </TD> <TD align="right"> 36.85 </TD> <TD align="right"> -0.18 </TD> <TD align="right"> 1.61 </TD> <TD align="right"> 0.39 </TD> </TR>
   </TABLE>

```r
s <- cbind(Device=rownames(s), s)

write.table(s, file = paste("tables/", filenameprefix,"-measures-device-describeby.csv", sep=""), sep=",", row.names=FALSE)

remove(s)


# descriptive stats for device*userid
s1<-describeBy(aggData.noLearn[,DVars], 
               list(aggData.noLearn$Device,aggData.noLearn$UserId), mat=FALSE, digits=2)
s1 <- do.call("rbind", s1)
s1 <- cbind(Device=rownames(s1), s1)

write.table(s1, file = paste("tables/", filenameprefix,"-measures-device+userid-describeby.csv", sep=""), sep=",", row.names=FALSE)

remove(s1)
#s1
```

Histograms
-----------------
![plot of chunk charts-bulk-hist](figure/charts-bulk-hist1.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist2.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist3.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist4.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist5.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist6.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist7.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist8.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist9.png) ![plot of chunk charts-bulk-hist](figure/charts-bulk-hist10.png) 

Boxplots
----------------



Lineplots
-------------


MO Over TRE
-----------------------

```r
s <- summaryBy(ErrorRate+TRE+TAC+MDC+ODC+MV+ME+MO+MovementTime+Throughput~Device, 
               data=aggData.noLearn,
               FUN=c(mean,sd))
# plot the movement offset over the TRE for each device
p <- ggplot(s, aes(x=TRE.mean, y=abs(MO.mean), group=Device, colour=Device )) +
    geom_point(size=5, aes(shape=Device)) + 
    #coord_cartesian(xlim = c(0, 0.4), ylim=c(0, 8)) + 
    ylab("Movement offset (pixels)") +
    xlab("Target Re-Entry") +
    theme(legend.position=c(0.55,0.9), legend.direction="horizontal") +
    theme()
p
```

![plot of chunk chart-MO-over-TRE](figure/chart-MO-over-TRE.png) 

```r
ggsave(file = paste("charts/",filenameprefix,"-MO-TRE.pdf", sep=""), width=14/2.54, height=7/2.54, dpi=100)
```

Throughput and Error rate
-----------------

```r
# plot the throughput and ErrorRate

describe <- describeBy(aggData.noLearn$Throughput, list(aggData.noLearn$Device))
describe<-do.call("rbind",describe)
describe <- cbind(Device=rownames(describe), describe)
levels(describe$Device) <- list( Mouse=c("Mouse"), LeapMotion=c("LeapMotion"), LeapMotionTouchless=c("LeapMotionTouchless"))
p1 <- ggplot(describe, aes(x=Device, y=mean, colour=Device, fill=Device )) + 
    geom_bar(stat="identity") +
    ylab("BPS") +
    xlab("Throughput") +
    #ggtitle("Throughput") +
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), colour="Black",
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    theme(legend.position="none", 
          #axis.text.x = element_text(angle = 20, hjust = 1),
          axis.text.x=element_blank()) +
    scale_fill_brewer(palette="Set1") + 
    scale_colour_brewer(palette="Set1") +
    theme() 

#ggsave(file = paste("charts/",filenameprefix,"-Throughput.pdf",sep=""), width=20/2.54, height=16/2.54, dpi=100)

describe <- describeBy(aggData.noLearn$ErrorRate, list(aggData.noLearn$Device))
describe<-do.call("rbind",describe)
describe <- cbind(Device=rownames(describe), describe)
levels(describe$Device) <- list( Mouse=c("Mouse"), LeapMotion=c("LeapMotion"), LeapMotionTouchless=c("LeapMotionTouchless"))

p2 <- ggplot(describe, aes(x=Device, y=mean*100, colour=Device, fill=Device )) + 
    geom_bar(stat="identity") +
    ylab("Percent.(%)") +
    xlab("ErrorRate") +
    #ggtitle("ErrorRate") +
    geom_errorbar(aes(ymin=100*(mean-1.96*se), ymax=100*(mean+1.96*se)), colour="Black",
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    
    theme(legend.position=c(0,0.9), legend.direction="horizontal", 
          #axis.text.x = element_text(angle = 20, hjust = 1),
          axis.text.x=element_blank()) +
    scale_fill_brewer(palette="Set1") + 
    scale_colour_brewer(palette="Set1") +
    theme() 

#pdf(file = paste("charts/",filenameprefix,"-Throughput+ErrorRate.pdf",sep=""), width=20/2.54, height=16/2.54)
pdf(file = paste("charts/",filenameprefix,"-Throughput+ErrorRate-small.pdf",sep=""), width=16/2.54, height=7/2.54)
multiplot(p1, p2, cols=2)
dev.off()
```

```
## pdf 
##   2
```

```r
p1
p2
```

![plot of chunk chart-Throughput-and-ErrorRate](figure/chart-Throughput-and-ErrorRate1.png) ![plot of chunk chart-Throughput-and-ErrorRate](figure/chart-Throughput-and-ErrorRate2.png) 


Barplots for metrics
-----------------

```r
describeMetrics <- describeBy(aggData.noLearn[, c("TRE","TAC","MDC","ODC","MV","ME","MO")], list(aggData.noLearn$Device))
a=cbind(describeMetrics$LeapMotionTouchless, metric=rownames(describeMetrics$LeapMotionTouchless))
a=cbind(a, Device="LeapMotionTouchless")
b=cbind(describeMetrics$Mouse, metric=rownames(describeMetrics$Mouse))
b=cbind(b, Device="Mouse")
c=cbind(describeMetrics$Touchpad, metric=rownames(describeMetrics$Touchpad))
c=cbind(c, Device="Touchpad")
describeMetrics = rbind(a, b, c)
```

```
## Error: numbers of columns of arguments do not match
```

```r
describeMetrics$metric <- factor(describeMetrics$metric, levels=c("TRE", "TAC", "MDC", "ODC", "MV", "ME", "MO"))

levels(describeMetrics$Device) <- list( Mouse=c("Mouse"), Touchpad=c("Touchpad"), LeapMotionTouchless=c("LeapMotionTouchless"))
```

```
## Error: attempt to set an attribute on NULL
```

```r
ggplot(describeMetrics, aes(x=Device, y=abs(mean), group=Device, colour=Device, fill=Device)) + 
    #stat_summary(fun.y="mean", geom="bar") + 
    geom_bar(stat="identity", width=.5, position = position_dodge(width=0.5)) +
    #geom_bar() +
    #coord_flip() +
    ylab("Mean") +
    xlab("") +
    geom_errorbar(aes(ymin=abs(mean)-1.96*abs(se), ymax=abs(mean)+1.96*abs(se)), colour="Black",
                  width=.2,                    # Width of the error bars
                  size = .1,
                  position=position_dodge(.5)) +
    facet_wrap(  ~ metric,nrow=1, scales="free") +
    theme(legend.position=c(.5,-0.1), legend.direction="horizontal", 
          axis.text.x = element_blank()) + 
    scale_fill_brewer(palette="Set1") + 
    scale_colour_brewer(palette="Set1") 
```

```
## Error: ggplot2 doesn't know how to deal with data of class by
```

```r
ggsave(file = paste("charts/",filenameprefix,"-metrics.pdf", sep=""), width=30/2.54, height=6/2.54, dpi=100)
```

Correlations
---------------

```r
vars <- c("Throughput", "TRE", "TAC", "ODC", "MDC", "MV", "ME", "MO")


print(xtable(round(cor(aggData.noLearn[, vars]), 2)), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Sun Aug 24 10:37:28 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Throughput </TH> <TH> TRE </TH> <TH> TAC </TH> <TH> ODC </TH> <TH> MDC </TH> <TH> MV </TH> <TH> ME </TH> <TH> MO </TH>  </TR>
  <TR> <TD align="right"> Throughput </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.54 </TD> <TD align="right"> -0.48 </TD> <TD align="right"> -0.70 </TD> <TD align="right"> -0.75 </TD> <TD align="right"> -0.11 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> -0.07 </TD> </TR>
  <TR> <TD align="right"> TRE </TD> <TD align="right"> -0.54 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 0.81 </TD> <TD align="right"> 0.74 </TD> <TD align="right"> 0.36 </TD> <TD align="right"> 0.24 </TD> <TD align="right"> 0.13 </TD> </TR>
  <TR> <TD align="right"> TAC </TD> <TD align="right"> -0.48 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.70 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 0.17 </TD> <TD align="right"> 0.14 </TD> </TR>
  <TR> <TD align="right"> ODC </TD> <TD align="right"> -0.70 </TD> <TD align="right"> 0.81 </TD> <TD align="right"> 0.70 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 0.39 </TD> <TD align="right"> 0.28 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD align="right"> MDC </TD> <TD align="right"> -0.75 </TD> <TD align="right"> 0.74 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.18 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.07 </TD> </TR>
  <TR> <TD align="right"> MV </TD> <TD align="right"> -0.11 </TD> <TD align="right"> 0.36 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 0.39 </TD> <TD align="right"> 0.18 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.94 </TD> <TD align="right"> 0.08 </TD> </TR>
  <TR> <TD align="right"> ME </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.24 </TD> <TD align="right"> 0.17 </TD> <TD align="right"> 0.28 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.94 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> MO </TD> <TD align="right"> -0.07 </TD> <TD align="right"> 0.13 </TD> <TD align="right"> 0.14 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> </TR>
   </TABLE>


Analysis of Variance - Differences between devices
---------------------
Analysis of variance of the throughput for the three devices showed significant differences 

```r
#ggplot(aggData.noLearn, aes(x=1:nrow(aggData.noLearn), y=MO, group=Device, colour=Device)) + geom_point()

#ggplot(aggData.noLearn, aes(x=Throughput, y=MO,  colour=Device)) + geom_point()



leveneTest(Throughput ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)  
## group   2    2.71  0.068 .
##       537                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(ErrorRate ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    42.3 <2e-16 ***
##       537                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(TAC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value  Pr(>F)    
## group   2    23.4 1.9e-10 ***
##       537                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(TRE ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    45.3 <2e-16 ***
##       537                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(MDC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2      51 <2e-16 ***
##       537                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(ODC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    74.6 <2e-16 ***
##       537                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(ME ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value  Pr(>F)    
## group   2    19.3 7.7e-09 ***
##       537                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(MV ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value  Pr(>F)    
## group   2    33.1 2.7e-14 ***
##       537                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(MO ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)   
## group   2    5.63 0.0038 **
##       537                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
for ( var in DVars ) {
    print(paste("Variances for ", var))
    varMouse = var(aggData.noLearn[aggData.noLearn$Device=="Mouse", var])
    varTouch = var(aggData.noLearn[aggData.noLearn$Device=="Touchpad", var])
    varLeap = var(aggData.noLearn[aggData.noLearn$Device=="LeapMotionTouchless", var])
    all = c(varMouse, varTouch, varLeap)
    print (paste("Mouse: ", varMouse, " Touchpad: ", varTouch, " LeapMotionTouchless: ", varLeap, " Max/Min: ", max(all)/min(all)))
} 
```

```
## [1] "Variances for  Throughput"
## [1] "Mouse:  0.351474699357933  Touchpad:  NA  LeapMotionTouchless:  0.275948456577687  Max/Min:  NA"
## [1] "Variances for  ErrorRate"
## [1] "Mouse:  0.00210166218359887  Touchpad:  NA  LeapMotionTouchless:  0.00896213531967722  Max/Min:  NA"
## [1] "Variances for  MovementTime"
## [1] "Mouse:  0.0104874557554314  Touchpad:  NA  LeapMotionTouchless:  0.263038683838058  Max/Min:  NA"
## [1] "Variances for  TRE"
## [1] "Mouse:  0.00613269880681426  Touchpad:  NA  LeapMotionTouchless:  0.0920420718670253  Max/Min:  NA"
## [1] "Variances for  TAC"
## [1] "Mouse:  0.116978550244844  Touchpad:  NA  LeapMotionTouchless:  0.447562452582937  Max/Min:  NA"
## [1] "Variances for  ODC"
## [1] "Mouse:  0.278306090075178  Touchpad:  NA  LeapMotionTouchless:  5.16620718670253  Max/Min:  NA"
## [1] "Variances for  MDC"
## [1] "Mouse:  0.719815021725636  Touchpad:  NA  LeapMotionTouchless:  7.63459700669012  Max/Min:  NA"
## [1] "Variances for  MV"
## [1] "Mouse:  50.153358541449  Touchpad:  NA  LeapMotionTouchless:  49.182501352306  Max/Min:  NA"
## [1] "Variances for  ME"
## [1] "Mouse:  30.9138930313236  Touchpad:  NA  LeapMotionTouchless:  30.0275873928223  Max/Min:  NA"
## [1] "Variances for  MO"
## [1] "Mouse:  43.9253523631899  Touchpad:  NA  LeapMotionTouchless:  26.7915572972957  Max/Min:  NA"
```

```r
# Anova for Throughput
aov.Throughput <- aov(Throughput~Device+Error(factor(UserId) / Device), data=aggData.noLearn)
#aov.Throughput <- aov(Throughput~Device, data=aggData.noLearn)
#aov.Throughput
summary(aov.Throughput)
```

```
## 
## Error: factor(UserId)
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals  8   74.3    9.28               
## 
## Error: factor(UserId):Device
##           Df Sum Sq Mean Sq F value  Pr(>F)    
## Device     2    742     371     134 9.9e-11 ***
## Residuals 16     44       3                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: Within
##            Df Sum Sq Mean Sq F value Pr(>F)
## Residuals 513   52.9   0.103
```

```r
# pairwise comparisons, adjusting p-value
# all devices are significantly different from each other, in terms of throughput
TukeyHSD(aov.Throughput)
```

```
## Error: no applicable method for 'TukeyHSD' applied to an object of class "c('aovlist', 'listof')"
```

```r
# Anova for TAC
aov.TAC <- aov(TAC~Device, data=aggData.noLearn)
#aov.TAC
summary(aov.TAC)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2   35.8   17.92    53.1 <2e-16 ***
## Residuals   537  181.3    0.34                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov.TAC)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = TAC ~ Device, data = aggData.noLearn)
## 
## $Device
##                                  diff    lwr    upr p adj
## LeapMotion-Mouse               0.3122 0.1683 0.4562     0
## LeapMotionTouchless-Mouse      0.6311 0.4871 0.7751     0
## LeapMotionTouchless-LeapMotion 0.3189 0.1749 0.4629     0
```

```r
# Anova for TRE
aov.TRE <- aov(TRE~Device, data=aggData.noLearn)
#aov.TRE
summary(aov.TRE)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2   8.46    4.23    78.4 <2e-16 ***
## Residuals   537  28.99    0.05                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov.TRE)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = TRE ~ Device, data = aggData.noLearn)
## 
## $Device
##                                     diff      lwr    upr  p adj
## LeapMotion-Mouse               0.2651852  0.20762 0.3227 0.0000
## LeapMotionTouchless-Mouse      0.2659259  0.20836 0.3235 0.0000
## LeapMotionTouchless-LeapMotion 0.0007407 -0.05682 0.0583 0.9995
```

```r
# Anova for MDC
aov.MDC <- aov(MDC~Device, data=aggData.noLearn)
#aov.MDC
summary(aov.MDC)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2   1647     824     170 <2e-16 ***
## Residuals   537   2600       5                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov.MDC)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = MDC ~ Device, data = aggData.noLearn)
## 
## $Device
##                                 diff    lwr   upr p adj
## LeapMotion-Mouse               3.073 2.5282 3.618     0
## LeapMotionTouchless-Mouse      4.114 3.5690 4.659     0
## LeapMotionTouchless-LeapMotion 1.041 0.4956 1.586     0
```

```r
# Anova for ODC
aov.ODC <- aov(ODC~Device, data=aggData.noLearn)
#aov.ODC
summary(aov.ODC)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2    926     463     161 <2e-16 ***
## Residuals   537   1543       3                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov.ODC)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = ODC ~ Device, data = aggData.noLearn)
## 
## $Device
##                                  diff    lwr   upr p adj
## LeapMotion-Mouse               2.4367 2.0167 2.857 0.000
## LeapMotionTouchless-Mouse      3.0256 2.6056 3.446 0.000
## LeapMotionTouchless-LeapMotion 0.5889 0.1689 1.009 0.003
```

```r
# pairwise.t.test(aggData.noLearn$ODC, aggData.noLearn$Device,  paired=F, pool=F)
# t.test(aggData.noLearn$ODC[aggData.noLearn$Device == "Mouse"], aggData.noLearn$ODC[aggData.noLearn$Device == "Touchpad"],  paired=F, var.equal=T)

# Anova for MV
aov.MV <- aov(MV~Device, data=aggData.noLearn)
#aov.MV
summary(aov.MV)
```

```
##              Df Sum Sq Mean Sq F value  Pr(>F)    
## Device        2   2121    1060    11.4 1.4e-05 ***
## Residuals   537  49788      93                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov.MV)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = MV ~ Device, data = aggData.noLearn)
## 
## $Device
##                                   diff    lwr    upr  p adj
## LeapMotion-Mouse                3.7806  1.395  6.166 0.0006
## LeapMotionTouchless-Mouse      -0.7462 -3.132  1.639 0.7427
## LeapMotionTouchless-LeapMotion -4.5268 -6.912 -2.141 0.0000
```

```r
# Anova for ME
aov.ME <- aov(ME~Device, data=aggData.noLearn)
#aov.ME
summary(aov.ME)
```

```
##              Df Sum Sq Mean Sq F value  Pr(>F)    
## Device        2   1083     541    9.16 0.00012 ***
## Residuals   537  31735      59                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov.ME)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = ME ~ Device, data = aggData.noLearn)
## 
## $Device
##                                  diff     lwr     upr  p adj
## LeapMotion-Mouse                1.011 -0.8935  2.9154 0.4257
## LeapMotionTouchless-Mouse      -2.368 -4.2721 -0.4631 0.0101
## LeapMotionTouchless-LeapMotion -3.379 -5.2830 -1.4741 0.0001
```

```r
# Anova for MO
aov.MO <- aov(MO~Device, data=aggData.noLearn)
#aov.MO
summary(aov.MO)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)
## Device        2     77    38.3    0.85   0.43
## Residuals   537  24279    45.2
```

```r
TukeyHSD(aov.MO)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = MO ~ Device, data = aggData.noLearn)
## 
## $Device
##                                   diff     lwr   upr  p adj
## LeapMotion-Mouse                0.8913 -0.7744 2.557 0.4199
## LeapMotionTouchless-Mouse       0.6498 -1.0159 2.316 0.6299
## LeapMotionTouchless-LeapMotion -0.2415 -1.9073 1.424 0.9380
```


```r
model.tre <- lm(Throughput~TRE, data=aggData.noLearn)
summary(model.tre)
```

```
## 
## Call:
## lm(formula = Throughput ~ TRE, data = aggData.noLearn)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.354 -0.803 -0.029  0.838  3.903 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.1097     0.0686    59.9   <2e-16 ***
## TRE          -2.6611     0.1793   -14.8   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.1 on 538 degrees of freedom
## Multiple R-squared:  0.29,	Adjusted R-squared:  0.289 
## F-statistic:  220 on 1 and 538 DF,  p-value: <2e-16
```

```r
model.odc <- lm(Throughput~ODC, data=aggData.noLearn)
summary(model.odc)
```

```
## 
## Call:
## lm(formula = Throughput ~ ODC, data = aggData.noLearn)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.273 -0.639 -0.018  0.615  3.317 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.6517     0.0686    67.8   <2e-16 ***
## ODC          -0.4275     0.0186   -22.9   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.927 on 538 degrees of freedom
## Multiple R-squared:  0.494,	Adjusted R-squared:  0.493 
## F-statistic:  526 on 1 and 538 DF,  p-value: <2e-16
```

```r
model.mdc <- lm(Throughput~MDC, data=aggData.noLearn)
summary(model.mdc)
```

```
## 
## Call:
## lm(formula = Throughput ~ MDC, data = aggData.noLearn)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.122 -0.617 -0.105  0.600  3.350 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.6750     0.0961    59.1   <2e-16 ***
## MDC          -0.3462     0.0133   -26.0   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.867 on 538 degrees of freedom
## Multiple R-squared:  0.557,	Adjusted R-squared:  0.556 
## F-statistic:  677 on 1 and 538 DF,  p-value: <2e-16
```

```r
model.odcmdc <- lm(Throughput~ODC+MDC, data=aggData.noLearn)
summary(model.odcmdc)
```

```
## 
## Call:
## lm(formula = Throughput ~ ODC + MDC, data = aggData.noLearn)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.987 -0.585 -0.063  0.556  3.921 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.4875     0.0968   56.70  < 2e-16 ***
## ODC          -0.1816     0.0276   -6.58  1.1e-10 ***
## MDC          -0.2362     0.0211  -11.22  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.835 on 537 degrees of freedom
## Multiple R-squared:  0.59,	Adjusted R-squared:  0.589 
## F-statistic:  387 on 2 and 537 DF,  p-value: <2e-16
```

```r
model.treodcmdc <- lm(MovementTime~TRE+ODC+MDC, data=aggData.noLearn)
summary(model.treodcmdc)
```

```
## 
## Call:
## lm(formula = MovementTime ~ TRE + ODC + MDC, data = aggData.noLearn)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.8588 -0.1781 -0.0146  0.1269  1.1660 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.21146    0.03090    6.84  2.1e-11 ***
## TRE         -0.35164    0.07454   -4.72  3.1e-06 ***
## ODC          0.15261    0.01017   15.00  < 2e-16 ***
## MDC          0.13694    0.00674   20.32  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.257 on 536 degrees of freedom
## Multiple R-squared:  0.845,	Adjusted R-squared:  0.845 
## F-statistic:  977 on 3 and 536 DF,  p-value: <2e-16
```

```r
model.tretac <- lm(Throughput~TRE+TAC, data=aggData.noLearn)
summary(model.tretac)
```

```
## 
## Call:
## lm(formula = Throughput ~ TRE + TAC, data = aggData.noLearn)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.313 -0.790 -0.078  0.824  3.714 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    4.644      0.168   27.69  < 2e-16 ***
## TRE           -2.016      0.256   -7.87    2e-14 ***
## TAC           -0.371      0.106   -3.49  0.00053 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.09 on 537 degrees of freedom
## Multiple R-squared:  0.306,	Adjusted R-squared:  0.304 
## F-statistic:  118 on 2 and 537 DF,  p-value: <2e-16
```


MDC Over ODC
-----------------------

```r
s <- summaryBy(ErrorRate+TRE+TAC+MDC+ODC+MV+ME+MO+MovementTime+Throughput~Device, 
               data=aggData.noLearn,
               FUN=c(mean,sd))
# plot the movement offset over the TRE for each device
p <- ggplot(s, aes(x=MDC.mean, y=abs(ODC.mean), group=Device, colour=Device )) +
    geom_point(size=5, aes(shape=Device)) + 
    #coord_cartesian(xlim = c(0, 0.4), ylim=c(0, 8)) + 
    ylab("Orthogonal direction change (pixels)") +
    xlab("Movement direction change (pixels)") +
    theme(legend.position=c(0.55,0.9), legend.direction="horizontal") +
    theme()
p
```

![plot of chunk chart-MDC-over-ODC](figure/chart-MDC-over-ODC.png) 

```r
ggsave(file = paste("charts/",filenameprefix,"-MDC-ODC.pdf", sep=""), width=14/2.54, height=7/2.54, dpi=100)
```
