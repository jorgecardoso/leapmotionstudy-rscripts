

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
data <- read.csv(file="data/measures-test.txt", head=TRUE, sep="")

# Users 0 and 5 did not complete the experiment
data <- data[data$UserId != 0 & data$UserId != 5,]

# convert the column to factor and name the levels
data$Device <- as.factor(data$Device)

# Set the levels 
# LeapMotion=c(0),  Mouse=c(1), Touchpad=c(2), LeapMotionTouchless=c(4)
# LeapMotion does not exist in the dataset from the UCP experiment
levels(data$Device) <- list( Mouse=c(1), Touchpad=c(2), LeapMotionTouchless=c(4))

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
## [1] 0.0001677 0.0112456 0.0215020 0.4091830 0.2499649 0.0011833 0.7604690
```

```r
diag(p.t.test$p.value) < 0.05
```

```
## [1]  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE
```

```r
# We will consider only blocks 4 to 8 in the rest of the analysis
filenameprefix <- "blocks4-8"
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
<!-- Tue Jul 15 19:19:11 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> vars </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> skew </TH> <TH> kurtosis </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> Mouse.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 4.85 </TD> <TD align="right"> 0.43 </TD> <TD align="right"> 4.81 </TD> <TD align="right"> 4.82 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 3.64 </TD> <TD align="right"> 6.35 </TD> <TD align="right"> 2.70 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 1.35 </TD> <TD align="right"> 0.03 </TD> </TR>
  <TR> <TD align="right"> Mouse.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.27 </TD> <TD align="right"> 0.27 </TD> <TD align="right"> 2.55 </TD> <TD align="right"> 8.76 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> Mouse.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 0.87 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.86 </TD> <TD align="right"> 0.86 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.64 </TD> <TD align="right"> 1.54 </TD> <TD align="right"> 0.90 </TD> <TD align="right"> 1.68 </TD> <TD align="right"> 11.74 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> Mouse.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 0.95 </TD> <TD align="right"> 0.74 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> Mouse.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 1.66 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 1.60 </TD> <TD align="right"> 1.65 </TD> <TD align="right"> 0.40 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 2.73 </TD> <TD align="right"> 1.93 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> -0.23 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> Mouse.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 1.19 </TD> <TD align="right"> 0.59 </TD> <TD align="right"> 1.07 </TD> <TD align="right"> 1.14 </TD> <TD align="right"> 0.59 </TD> <TD align="right"> 0.13 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.87 </TD> <TD align="right"> 0.71 </TD> <TD align="right"> -0.13 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> Mouse.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 4.86 </TD> <TD align="right"> 0.96 </TD> <TD align="right"> 4.73 </TD> <TD align="right"> 4.80 </TD> <TD align="right"> 0.94 </TD> <TD align="right"> 2.80 </TD> <TD align="right"> 7.93 </TD> <TD align="right"> 5.13 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> -0.17 </TD> <TD align="right"> 0.06 </TD> </TR>
  <TR> <TD align="right"> Mouse.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 20.52 </TD> <TD align="right"> 6.88 </TD> <TD align="right"> 19.00 </TD> <TD align="right"> 19.98 </TD> <TD align="right"> 6.90 </TD> <TD align="right"> 9.33 </TD> <TD align="right"> 50.07 </TD> <TD align="right"> 40.74 </TD> <TD align="right"> 0.85 </TD> <TD align="right"> 0.87 </TD> <TD align="right"> 0.44 </TD> </TR>
  <TR> <TD align="right"> Mouse.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 18.41 </TD> <TD align="right"> 5.27 </TD> <TD align="right"> 18.07 </TD> <TD align="right"> 18.14 </TD> <TD align="right"> 5.25 </TD> <TD align="right"> 7.96 </TD> <TD align="right"> 38.13 </TD> <TD align="right"> 30.17 </TD> <TD align="right"> 0.61 </TD> <TD align="right"> 0.52 </TD> <TD align="right"> 0.33 </TD> </TR>
  <TR> <TD align="right"> Mouse.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> -1.68 </TD> <TD align="right"> 5.93 </TD> <TD align="right"> -0.90 </TD> <TD align="right"> -1.50 </TD> <TD align="right"> 4.96 </TD> <TD align="right"> -19.18 </TD> <TD align="right"> 14.65 </TD> <TD align="right"> 33.82 </TD> <TD align="right"> -0.33 </TD> <TD align="right"> 0.31 </TD> <TD align="right"> 0.37 </TD> </TR>
  <TR> <TD align="right"> Touchpad.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 3.21 </TD> <TD align="right"> 0.42 </TD> <TD align="right"> 3.24 </TD> <TD align="right"> 3.20 </TD> <TD align="right"> 0.46 </TD> <TD align="right"> 2.24 </TD> <TD align="right"> 4.28 </TD> <TD align="right"> 2.04 </TD> <TD align="right"> 0.16 </TD> <TD align="right"> -0.51 </TD> <TD align="right"> 0.03 </TD> </TR>
  <TR> <TD align="right"> Touchpad.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.27 </TD> <TD align="right"> 0.27 </TD> <TD align="right"> 2.36 </TD> <TD align="right"> 5.86 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> Touchpad.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 1.31 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 1.30 </TD> <TD align="right"> 0.21 </TD> <TD align="right"> 0.94 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.06 </TD> <TD align="right"> 0.69 </TD> <TD align="right"> 0.46 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> Touchpad.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 0.14 </TD> <TD align="right"> 0.12 </TD> <TD align="right"> 0.13 </TD> <TD align="right"> 0.13 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.67 </TD> <TD align="right"> 0.67 </TD> <TD align="right"> 1.11 </TD> <TD align="right"> 1.49 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> Touchpad.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 1.26 </TD> <TD align="right"> 0.32 </TD> <TD align="right"> 1.23 </TD> <TD align="right"> 1.25 </TD> <TD align="right"> 0.35 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 2.27 </TD> <TD align="right"> 1.80 </TD> <TD align="right"> 0.31 </TD> <TD align="right"> -0.03 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> Touchpad.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 1.04 </TD> <TD align="right"> 0.55 </TD> <TD align="right"> 0.97 </TD> <TD align="right"> 1.01 </TD> <TD align="right"> 0.64 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 2.80 </TD> <TD align="right"> 2.73 </TD> <TD align="right"> 0.54 </TD> <TD align="right"> -0.27 </TD> <TD align="right"> 0.03 </TD> </TR>
  <TR> <TD align="right"> Touchpad.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 4.48 </TD> <TD align="right"> 1.15 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 4.36 </TD> <TD align="right"> 0.99 </TD> <TD align="right"> 2.47 </TD> <TD align="right"> 8.27 </TD> <TD align="right"> 5.80 </TD> <TD align="right"> 0.97 </TD> <TD align="right"> 1.02 </TD> <TD align="right"> 0.07 </TD> </TR>
  <TR> <TD align="right"> Touchpad.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 20.05 </TD> <TD align="right"> 6.81 </TD> <TD align="right"> 20.22 </TD> <TD align="right"> 19.93 </TD> <TD align="right"> 6.65 </TD> <TD align="right"> 6.90 </TD> <TD align="right"> 41.93 </TD> <TD align="right"> 35.03 </TD> <TD align="right"> 0.18 </TD> <TD align="right"> -0.26 </TD> <TD align="right"> 0.43 </TD> </TR>
  <TR> <TD align="right"> Touchpad.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 16.55 </TD> <TD align="right"> 5.24 </TD> <TD align="right"> 16.52 </TD> <TD align="right"> 16.35 </TD> <TD align="right"> 4.38 </TD> <TD align="right"> 6.25 </TD> <TD align="right"> 34.05 </TD> <TD align="right"> 27.80 </TD> <TD align="right"> 0.45 </TD> <TD align="right"> 0.48 </TD> <TD align="right"> 0.33 </TD> </TR>
  <TR> <TD align="right"> Touchpad.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> -1.58 </TD> <TD align="right"> 6.36 </TD> <TD align="right"> -1.37 </TD> <TD align="right"> -1.40 </TD> <TD align="right"> 6.13 </TD> <TD align="right"> -23.41 </TD> <TD align="right"> 17.47 </TD> <TD align="right"> 40.88 </TD> <TD align="right"> -0.25 </TD> <TD align="right"> 0.16 </TD> <TD align="right"> 0.40 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 2.16 </TD> <TD align="right"> 0.39 </TD> <TD align="right"> 2.19 </TD> <TD align="right"> 2.17 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 1.18 </TD> <TD align="right"> 3.22 </TD> <TD align="right"> 2.04 </TD> <TD align="right"> -0.04 </TD> <TD align="right"> -0.77 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.40 </TD> <TD align="right"> 0.40 </TD> <TD align="right"> 1.21 </TD> <TD align="right"> 1.51 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 2.11 </TD> <TD align="right"> 0.52 </TD> <TD align="right"> 2.01 </TD> <TD align="right"> 2.04 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 4.53 </TD> <TD align="right"> 3.26 </TD> <TD align="right"> 1.28 </TD> <TD align="right"> 2.26 </TD> <TD align="right"> 0.03 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 0.35 </TD> <TD align="right"> 0.21 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.34 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 0.73 </TD> <TD align="right"> 0.73 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 2.35 </TD> <TD align="right"> 0.62 </TD> <TD align="right"> 2.33 </TD> <TD align="right"> 2.33 </TD> <TD align="right"> 0.59 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.60 </TD> <TD align="right"> 3.60 </TD> <TD align="right"> 0.56 </TD> <TD align="right"> 0.71 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 4.50 </TD> <TD align="right"> 2.18 </TD> <TD align="right"> 4.20 </TD> <TD align="right"> 4.25 </TD> <TD align="right"> 1.88 </TD> <TD align="right"> 0.87 </TD> <TD align="right"> 12.67 </TD> <TD align="right"> 11.80 </TD> <TD align="right"> 1.10 </TD> <TD align="right"> 1.28 </TD> <TD align="right"> 0.14 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 8.78 </TD> <TD align="right"> 2.43 </TD> <TD align="right"> 8.47 </TD> <TD align="right"> 8.63 </TD> <TD align="right"> 2.27 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 16.67 </TD> <TD align="right"> 12.67 </TD> <TD align="right"> 0.57 </TD> <TD align="right"> -0.10 </TD> <TD align="right"> 0.15 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 20.47 </TD> <TD align="right"> 6.24 </TD> <TD align="right"> 19.00 </TD> <TD align="right"> 19.73 </TD> <TD align="right"> 4.48 </TD> <TD align="right"> 10.54 </TD> <TD align="right"> 54.93 </TD> <TD align="right"> 44.39 </TD> <TD align="right"> 1.66 </TD> <TD align="right"> 4.54 </TD> <TD align="right"> 0.39 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> 16.27 </TD> <TD align="right"> 5.33 </TD> <TD align="right"> 15.20 </TD> <TD align="right"> 15.44 </TD> <TD align="right"> 3.56 </TD> <TD align="right"> 8.57 </TD> <TD align="right"> 42.57 </TD> <TD align="right"> 34.01 </TD> <TD align="right"> 2.13 </TD> <TD align="right"> 6.29 </TD> <TD align="right"> 0.34 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 250.00 </TD> <TD align="right"> -1.49 </TD> <TD align="right"> 4.59 </TD> <TD align="right"> -1.26 </TD> <TD align="right"> -1.35 </TD> <TD align="right"> 3.92 </TD> <TD align="right"> -33.56 </TD> <TD align="right"> 10.46 </TD> <TD align="right"> 44.03 </TD> <TD align="right"> -1.41 </TD> <TD align="right"> 8.97 </TD> <TD align="right"> 0.29 </TD> </TR>
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
levels(describe$Device) <- list( Mouse=c("Mouse"), Touchpad=c("Touchpad"), LeapMotionTouchless=c("LeapMotionTouchless"))
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
levels(describe$Device) <- list( Mouse=c("Mouse"), Touchpad=c("Touchpad"), LeapMotionTouchless=c("LeapMotionTouchless"))

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
describeMetrics$metric <- factor(describeMetrics$metric, levels=c("TRE", "TAC", "MDC", "ODC", "MV", "ME", "MO"))

levels(describeMetrics$Device) <- list( Mouse=c("Mouse"), Touchpad=c("Touchpad"), LeapMotionTouchless=c("LeapMotionTouchless"))

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
ggsave(file = paste("charts/",filenameprefix,"-metrics.pdf", sep=""), width=30/2.54, height=6/2.54, dpi=100)
```

![plot of chunk chart-barplots](figure/chart-barplots.png) 

Correlations
---------------

```r
vars <- c("Throughput", "TRE", "TAC", "ODC", "MDC", "MV", "ME", "MO")


print(xtable(round(cor(aggData.noLearn[, vars]), 2)), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Tue Jul 15 19:19:16 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Throughput </TH> <TH> TRE </TH> <TH> TAC </TH> <TH> ODC </TH> <TH> MDC </TH> <TH> MV </TH> <TH> ME </TH> <TH> MO </TH>  </TR>
  <TR> <TD align="right"> Throughput </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.57 </TD> <TD align="right"> -0.40 </TD> <TD align="right"> -0.65 </TD> <TD align="right"> -0.61 </TD> <TD align="right"> -0.10 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD align="right"> TRE </TD> <TD align="right"> -0.57 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.63 </TD> <TD align="right"> 0.73 </TD> <TD align="right"> 0.60 </TD> <TD align="right"> 0.16 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> -0.02 </TD> </TR>
  <TR> <TD align="right"> TAC </TD> <TD align="right"> -0.40 </TD> <TD align="right"> 0.63 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 0.77 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> -0.11 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> ODC </TD> <TD align="right"> -0.65 </TD> <TD align="right"> 0.73 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.76 </TD> <TD align="right"> 0.21 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> -0.04 </TD> </TR>
  <TR> <TD align="right"> MDC </TD> <TD align="right"> -0.61 </TD> <TD align="right"> 0.60 </TD> <TD align="right"> 0.77 </TD> <TD align="right"> 0.76 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.12 </TD> <TD align="right"> -0.21 </TD> <TD align="right"> -0.01 </TD> </TR>
  <TR> <TD align="right"> MV </TD> <TD align="right"> -0.10 </TD> <TD align="right"> 0.16 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> 0.21 </TD> <TD align="right"> -0.12 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.93 </TD> <TD align="right"> -0.28 </TD> </TR>
  <TR> <TD align="right"> ME </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.01 </TD> <TD align="right"> -0.11 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> -0.21 </TD> <TD align="right"> 0.93 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.28 </TD> </TR>
  <TR> <TD align="right"> MO </TD> <TD align="right"> 0.05 </TD> <TD align="right"> -0.02 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> -0.04 </TD> <TD align="right"> -0.01 </TD> <TD align="right"> -0.28 </TD> <TD align="right"> -0.28 </TD> <TD align="right"> 1.00 </TD> </TR>
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
## group   2    0.85   0.43
##       747
```

```r
leveneTest(ErrorRate ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    50.4 <2e-16 ***
##       747                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(TAC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    48.8 <2e-16 ***
##       747                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(TRE ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    80.8 <2e-16 ***
##       747                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(MDC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2     112 <2e-16 ***
##       747                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(ODC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2     159 <2e-16 ***
##       747                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(ME ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)
## group   2    1.65   0.19
##       747
```

```r
leveneTest(MV ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)   
## group   2    4.85  0.008 **
##       747                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(MO ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value  Pr(>F)    
## group   2    17.6 3.3e-08 ***
##       747                    
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
## [1] "Mouse:  0.18621480358176  Touchpad:  0.177971275843047  LeapMotionTouchless:  0.15599852012554  Max/Min:  1.19369596219185"
## [1] "Variances for  ErrorRate"
## [1] "Mouse:  0.00147077197679607  Touchpad:  0.00204194556001785  LeapMotionTouchless:  0.00585424364123159  Max/Min:  3.98038834951456"
## [1] "Variances for  MovementTime"
## [1] "Mouse:  0.0081315631999286  Touchpad:  0.0399067797518965  LeapMotionTouchless:  0.266733437566515  Max/Min:  32.8022338397194"
## [1] "Variances for  TRE"
## [1] "Mouse:  0.00774340026773762  Touchpad:  0.0137959125390451  LeapMotionTouchless:  0.0434466755912539  Max/Min:  5.61080069336874"
## [1] "Variances for  TAC"
## [1] "Mouse:  0.13786245426149  Touchpad:  0.100161070950469  LeapMotionTouchless:  0.388749379741187  Max/Min:  3.88124224364005"
## [1] "Variances for  ODC"
## [1] "Mouse:  0.343373922356091  Touchpad:  0.30481599286033  LeapMotionTouchless:  4.77398225792057  Max/Min:  15.6618496723958"
## [1] "Variances for  MDC"
## [1] "Mouse:  0.914690298973672  Touchpad:  1.32112628290942  LeapMotionTouchless:  5.92468390896921  Max/Min:  6.47725674538912"
## [1] "Variances for  MV"
## [1] "Mouse:  47.392669960312  Touchpad:  46.3843224190398  LeapMotionTouchless:  38.9132533226816  Max/Min:  1.21790562118557"
## [1] "Variances for  ME"
## [1] "Mouse:  27.8230268678934  Touchpad:  27.4997402012203  LeapMotionTouchless:  28.3782501489403  Max/Min:  1.031946118083"
## [1] "Variances for  MO"
## [1] "Mouse:  35.1266232997426  Touchpad:  40.387150865384  LeapMotionTouchless:  21.05782841732  Max/Min:  1.9179162288247"
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
## Residuals  9   48.6     5.4               
## 
## Error: factor(UserId):Device
##           Df Sum Sq Mean Sq F value  Pr(>F)    
## Device     2    916     458     271 3.6e-14 ***
## Residuals 18     30       2                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: Within
##            Df Sum Sq Mean Sq F value Pr(>F)
## Residuals 720   50.6  0.0702
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
## Device        2    153    76.7     367 <2e-16 ***
## Residuals   747    156     0.2                   
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
##                                 diff     lwr     upr p adj
## Touchpad-Mouse               -0.4077 -0.5037 -0.3117     0
## LeapMotionTouchless-Mouse     0.6883  0.5923  0.7843     0
## LeapMotionTouchless-Touchpad  1.0960  1.0000  1.1920     0
```

```r
# Anova for TRE
aov.TRE <- aov(TRE~Device, data=aggData.noLearn)
#aov.TRE
summary(aov.TRE)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2    8.8    4.40     203 <2e-16 ***
## Residuals   747   16.2    0.02                   
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
##                                 diff      lwr     upr  p adj
## Touchpad-Mouse               0.04027 0.009352 0.07118 0.0065
## LeapMotionTouchless-Mouse    0.24720 0.216285 0.27811 0.0000
## LeapMotionTouchless-Touchpad 0.20693 0.176019 0.23785 0.0000
```

```r
# Anova for MDC
aov.MDC <- aov(MDC~Device, data=aggData.noLearn)
#aov.MDC
summary(aov.MDC)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2   2834    1417     521 <2e-16 ***
## Residuals   747   2032       3                   
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
##                                 diff     lwr      upr  p adj
## Touchpad-Mouse               -0.3808 -0.7272 -0.03437 0.0271
## LeapMotionTouchless-Mouse     3.9203  3.5738  4.26669 0.0000
## LeapMotionTouchless-Touchpad  4.3011  3.9546  4.64749 0.0000
```

```r
# Anova for ODC
aov.ODC <- aov(ODC~Device, data=aggData.noLearn)
#aov.ODC
summary(aov.ODC)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2   1909     954     528 <2e-16 ***
## Residuals   747   1350       2                   
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
##                                 diff     lwr    upr  p adj
## Touchpad-Mouse               -0.1472 -0.4296 0.1352 0.4392
## LeapMotionTouchless-Mouse     3.3083  3.0259 3.5907 0.0000
## LeapMotionTouchless-Touchpad  3.4555  3.1731 3.7379 0.0000
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
##              Df Sum Sq Mean Sq F value Pr(>F)
## Device        2     34    16.9    0.38   0.68
## Residuals   747  33040    44.2
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
##                                 diff     lwr    upr  p adj
## Touchpad-Mouse               -0.4764 -1.8734 0.9205 0.7026
## LeapMotionTouchless-Mouse    -0.0569 -1.4538 1.3400 0.9950
## LeapMotionTouchless-Touchpad  0.4195 -0.9774 1.8165 0.7605
```

```r
# Anova for ME
aov.ME <- aov(ME~Device, data=aggData.noLearn)
#aov.ME
summary(aov.ME)
```

```
##              Df Sum Sq Mean Sq F value  Pr(>F)    
## Device        2    676     338    12.1 6.6e-06 ***
## Residuals   747  20842      28                    
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
##                                 diff    lwr     upr  p adj
## Touchpad-Mouse               -1.8605 -2.970 -0.7510 0.0003
## LeapMotionTouchless-Mouse    -2.1387 -3.248 -1.0292 0.0000
## LeapMotionTouchless-Touchpad -0.2782 -1.388  0.8313 0.8262
```

```r
# Anova for MO
aov.MO <- aov(MO~Device, data=aggData.noLearn)
#aov.MO
summary(aov.MO)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)
## Device        2      4     2.1    0.07   0.94
## Residuals   747  24046    32.2
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
##                                 diff    lwr   upr  p adj
## Touchpad-Mouse               0.09373 -1.098 1.285 0.9814
## LeapMotionTouchless-Mouse    0.18481 -1.007 1.377 0.9295
## LeapMotionTouchless-Touchpad 0.09108 -1.101 1.283 0.9824
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
## -2.265 -0.752 -0.123  0.819  2.529 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.1437     0.0525    78.9   <2e-16 ***
## TRE          -3.6906     0.1943   -19.0   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.971 on 748 degrees of freedom
## Multiple R-squared:  0.325,	Adjusted R-squared:  0.325 
## F-statistic:  361 on 1 and 748 DF,  p-value: <2e-16
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
## -1.750 -0.743 -0.223  0.828  2.391 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.2336     0.0482    87.8   <2e-16 ***
## ODC          -0.3679     0.0157   -23.4   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.899 on 748 degrees of freedom
## Multiple R-squared:  0.422,	Adjusted R-squared:  0.421 
## F-statistic:  546 on 1 and 748 DF,  p-value: <2e-16
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
## -1.841 -0.737 -0.168  0.788  2.589 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.1143     0.0880    58.1   <2e-16 ***
## MDC          -0.2826     0.0134   -21.0   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.937 on 748 degrees of freedom
## Multiple R-squared:  0.372,	Adjusted R-squared:  0.371 
## F-statistic:  443 on 1 and 748 DF,  p-value: <2e-16
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
## -1.732 -0.715 -0.175  0.800  2.388 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.7354     0.0896   52.83  < 2e-16 ***
## ODC          -0.2496     0.0236  -10.56  < 2e-16 ***
## MDC          -0.1271     0.0193   -6.57  9.4e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.875 on 747 degrees of freedom
## Multiple R-squared:  0.453,	Adjusted R-squared:  0.452 
## F-statistic:  310 on 2 and 747 DF,  p-value: <2e-16
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
## -0.5779 -0.2558 -0.0556  0.2159  1.1965 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.55883    0.03267   17.11  < 2e-16 ***
## TRE          0.31053    0.09379    3.31  0.00097 ***
## ODC          0.14358    0.01013   14.17  < 2e-16 ***
## MDC          0.08045    0.00706   11.39  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.318 on 746 degrees of freedom
## Multiple R-squared:  0.727,	Adjusted R-squared:  0.725 
## F-statistic:  661 on 3 and 746 DF,  p-value: <2e-16
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
## -2.288 -0.749 -0.154  0.819  2.502 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.3095     0.1072   40.20   <2e-16 ***
## TRE          -3.4095     0.2505  -13.61   <2e-16 ***
## TAC          -0.1262     0.0712   -1.77    0.077 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.97 on 747 degrees of freedom
## Multiple R-squared:  0.328,	Adjusted R-squared:  0.326 
## F-statistic:  183 on 2 and 747 DF,  p-value: <2e-16
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
