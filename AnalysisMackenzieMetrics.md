

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
#data <- data[ data$UserId != 5,]

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
```

```
## Error: not all arguments have the same length
```

```r
diag(p.t.test$p.value) 
```

```
## Error: object 'p.t.test' not found
```

```r
diag(p.t.test$p.value) < 0.05
```

```
## Error: object 'p.t.test' not found
```

```r
# We will consider only blocks 4 to 8 in the rest of the analysis
filenameprefix <- "blocks4-7"
aggData.noLearn <- aggData[as.numeric(aggData$Block) > 3 & as.numeric(aggData$Block) < 8,]

# Drop unused block levels
aggData.noLearn$Block <- factor(aggData.noLearn$Block)
```

The results indicate a clear learning effect in blocks 1 to 3, but also indicate a significant different between blocks 6 and 7, suggesting that participants were still learning after block 6. However, in our following analysis we discard only blocks 1 to 3, since those represent the most significant learning effect.


```
## Error: not all arguments have the same length
```

```
## Error: not all arguments have the same length
```



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
<!-- Wed Aug 27 21:56:25 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> vars </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> skew </TH> <TH> kurtosis </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> Mouse.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 4.98 </TD> <TD align="right"> 0.57 </TD> <TD align="right"> 4.91 </TD> <TD align="right"> 4.96 </TD> <TD align="right"> 0.57 </TD> <TD align="right"> 3.68 </TD> <TD align="right"> 6.49 </TD> <TD align="right"> 2.82 </TD> <TD align="right"> 0.23 </TD> <TD align="right"> -0.32 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> Mouse.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.03 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 1.17 </TD> <TD align="right"> 1.20 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> Mouse.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 0.82 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.81 </TD> <TD align="right"> 0.81 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.61 </TD> <TD align="right"> 1.10 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> Mouse.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.08 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> -0.53 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> Mouse.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 1.62 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 1.60 </TD> <TD align="right"> 1.61 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 2.60 </TD> <TD align="right"> 1.80 </TD> <TD align="right"> 0.27 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> Mouse.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 1.12 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.08 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 2.47 </TD> <TD align="right"> 2.40 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> -0.60 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> Mouse.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 4.32 </TD> <TD align="right"> 0.86 </TD> <TD align="right"> 4.20 </TD> <TD align="right"> 4.26 </TD> <TD align="right"> 0.89 </TD> <TD align="right"> 2.47 </TD> <TD align="right"> 6.87 </TD> <TD align="right"> 4.40 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> -0.08 </TD> <TD align="right"> 0.06 </TD> </TR>
  <TR> <TD align="right"> Mouse.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 21.79 </TD> <TD align="right"> 7.21 </TD> <TD align="right"> 20.64 </TD> <TD align="right"> 21.36 </TD> <TD align="right"> 8.04 </TD> <TD align="right"> 9.51 </TD> <TD align="right"> 48.22 </TD> <TD align="right"> 38.72 </TD> <TD align="right"> 0.61 </TD> <TD align="right"> 0.12 </TD> <TD align="right"> 0.51 </TD> </TR>
  <TR> <TD align="right"> Mouse.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 19.40 </TD> <TD align="right"> 5.72 </TD> <TD align="right"> 18.89 </TD> <TD align="right"> 19.16 </TD> <TD align="right"> 6.32 </TD> <TD align="right"> 8.82 </TD> <TD align="right"> 41.02 </TD> <TD align="right"> 32.19 </TD> <TD align="right"> 0.46 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> 0.40 </TD> </TR>
  <TR> <TD align="right"> Mouse.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> -2.26 </TD> <TD align="right"> 6.37 </TD> <TD align="right"> -1.75 </TD> <TD align="right"> -2.22 </TD> <TD align="right"> 5.83 </TD> <TD align="right"> -18.62 </TD> <TD align="right"> 21.81 </TD> <TD align="right"> 40.43 </TD> <TD align="right"> 0.19 </TD> <TD align="right"> 0.91 </TD> <TD align="right"> 0.45 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 2.79 </TD> <TD align="right"> 0.55 </TD> <TD align="right"> 2.75 </TD> <TD align="right"> 2.77 </TD> <TD align="right"> 0.55 </TD> <TD align="right"> 1.59 </TD> <TD align="right"> 4.21 </TD> <TD align="right"> 2.61 </TD> <TD align="right"> 0.26 </TD> <TD align="right"> -0.46 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 0.11 </TD> <TD align="right"> 0.12 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.60 </TD> <TD align="right"> 0.60 </TD> <TD align="right"> 1.41 </TD> <TD align="right"> 2.16 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 1.68 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 1.53 </TD> <TD align="right"> 1.60 </TD> <TD align="right"> 0.34 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.12 </TD> <TD align="right"> 3.13 </TD> <TD align="right"> 1.79 </TD> <TD align="right"> 3.83 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 0.35 </TD> <TD align="right"> 0.25 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.32 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 1.27 </TD> <TD align="right"> 1.13 </TD> <TD align="right"> 1.04 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 1.90 </TD> <TD align="right"> 0.64 </TD> <TD align="right"> 1.80 </TD> <TD align="right"> 1.84 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 3.87 </TD> <TD align="right"> 0.98 </TD> <TD align="right"> 1.12 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 3.60 </TD> <TD align="right"> 1.72 </TD> <TD align="right"> 3.20 </TD> <TD align="right"> 3.36 </TD> <TD align="right"> 1.24 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 11.00 </TD> <TD align="right"> 10.20 </TD> <TD align="right"> 1.65 </TD> <TD align="right"> 3.59 </TD> <TD align="right"> 0.12 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 7.42 </TD> <TD align="right"> 2.42 </TD> <TD align="right"> 6.87 </TD> <TD align="right"> 7.13 </TD> <TD align="right"> 1.98 </TD> <TD align="right"> 3.47 </TD> <TD align="right"> 17.07 </TD> <TD align="right"> 13.60 </TD> <TD align="right"> 1.17 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 25.89 </TD> <TD align="right"> 12.96 </TD> <TD align="right"> 21.92 </TD> <TD align="right"> 23.82 </TD> <TD align="right"> 7.86 </TD> <TD align="right"> 8.53 </TD> <TD align="right"> 88.30 </TD> <TD align="right"> 79.77 </TD> <TD align="right"> 1.78 </TD> <TD align="right"> 3.82 </TD> <TD align="right"> 0.92 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> 20.96 </TD> <TD align="right"> 10.44 </TD> <TD align="right"> 18.36 </TD> <TD align="right"> 19.26 </TD> <TD align="right"> 5.77 </TD> <TD align="right"> 7.14 </TD> <TD align="right"> 80.19 </TD> <TD align="right"> 73.05 </TD> <TD align="right"> 2.48 </TD> <TD align="right"> 8.50 </TD> <TD align="right"> 0.74 </TD> </TR>
  <TR> <TD align="right"> LeapMotion.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 200.00 </TD> <TD align="right"> -1.65 </TD> <TD align="right"> 7.78 </TD> <TD align="right"> -2.07 </TD> <TD align="right"> -2.11 </TD> <TD align="right"> 5.41 </TD> <TD align="right"> -25.27 </TD> <TD align="right"> 39.38 </TD> <TD align="right"> 64.65 </TD> <TD align="right"> 1.30 </TD> <TD align="right"> 5.80 </TD> <TD align="right"> 0.55 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.Throughput </TD> <TD align="right">   1 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 2.29 </TD> <TD align="right"> 0.52 </TD> <TD align="right"> 2.22 </TD> <TD align="right"> 2.26 </TD> <TD align="right"> 0.41 </TD> <TD align="right"> 1.19 </TD> <TD align="right"> 3.75 </TD> <TD align="right"> 2.56 </TD> <TD align="right"> 0.57 </TD> <TD align="right"> 0.03 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ErrorRate </TD> <TD align="right">   2 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.09 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 1.42 </TD> <TD align="right"> 2.24 </TD> <TD align="right"> 0.01 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MovementTime </TD> <TD align="right">   3 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 1.95 </TD> <TD align="right"> 0.52 </TD> <TD align="right"> 1.88 </TD> <TD align="right"> 1.91 </TD> <TD align="right"> 0.40 </TD> <TD align="right"> 1.04 </TD> <TD align="right"> 4.10 </TD> <TD align="right"> 3.06 </TD> <TD align="right"> 0.96 </TD> <TD align="right"> 1.54 </TD> <TD align="right"> 0.04 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.TRE </TD> <TD align="right">   4 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 0.36 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 2.53 </TD> <TD align="right"> 2.53 </TD> <TD align="right"> 2.64 </TD> <TD align="right"> 14.25 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.TAC </TD> <TD align="right">   5 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 2.24 </TD> <TD align="right"> 0.66 </TD> <TD align="right"> 2.13 </TD> <TD align="right"> 2.18 </TD> <TD align="right"> 0.59 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.33 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 1.20 </TD> <TD align="right"> 2.95 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ODC </TD> <TD align="right">   6 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 4.24 </TD> <TD align="right"> 2.30 </TD> <TD align="right"> 3.87 </TD> <TD align="right"> 4.02 </TD> <TD align="right"> 2.27 </TD> <TD align="right"> 1.07 </TD> <TD align="right"> 15.67 </TD> <TD align="right"> 14.60 </TD> <TD align="right"> 1.29 </TD> <TD align="right"> 3.15 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MDC </TD> <TD align="right">   7 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 8.41 </TD> <TD align="right"> 2.76 </TD> <TD align="right"> 8.20 </TD> <TD align="right"> 8.17 </TD> <TD align="right"> 2.67 </TD> <TD align="right"> 3.80 </TD> <TD align="right"> 22.40 </TD> <TD align="right"> 18.60 </TD> <TD align="right"> 1.15 </TD> <TD align="right"> 2.81 </TD> <TD align="right"> 0.20 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MV </TD> <TD align="right">   8 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 22.09 </TD> <TD align="right"> 7.13 </TD> <TD align="right"> 20.25 </TD> <TD align="right"> 21.10 </TD> <TD align="right"> 4.96 </TD> <TD align="right"> 11.89 </TD> <TD align="right"> 51.25 </TD> <TD align="right"> 39.36 </TD> <TD align="right"> 1.66 </TD> <TD align="right"> 3.41 </TD> <TD align="right"> 0.52 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.ME </TD> <TD align="right">   9 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> 17.91 </TD> <TD align="right"> 5.61 </TD> <TD align="right"> 16.76 </TD> <TD align="right"> 17.06 </TD> <TD align="right"> 3.95 </TD> <TD align="right"> 9.71 </TD> <TD align="right"> 43.75 </TD> <TD align="right"> 34.04 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 5.34 </TD> <TD align="right"> 0.41 </TD> </TR>
  <TR> <TD align="right"> LeapMotionTouchless.MO </TD> <TD align="right">  10 </TD> <TD align="right"> 189.00 </TD> <TD align="right"> -2.02 </TD> <TD align="right"> 5.28 </TD> <TD align="right"> -2.06 </TD> <TD align="right"> -1.96 </TD> <TD align="right"> 4.47 </TD> <TD align="right"> -22.26 </TD> <TD align="right"> 14.59 </TD> <TD align="right"> 36.85 </TD> <TD align="right"> -0.31 </TD> <TD align="right"> 1.92 </TD> <TD align="right"> 0.38 </TD> </TR>
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
<!-- Wed Aug 27 21:56:35 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Throughput </TH> <TH> TRE </TH> <TH> TAC </TH> <TH> ODC </TH> <TH> MDC </TH> <TH> MV </TH> <TH> ME </TH> <TH> MO </TH>  </TR>
  <TR> <TD align="right"> Throughput </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.54 </TD> <TD align="right"> -0.47 </TD> <TD align="right"> -0.71 </TD> <TD align="right"> -0.75 </TD> <TD align="right"> -0.14 </TD> <TD align="right"> -0.03 </TD> <TD align="right"> -0.04 </TD> </TR>
  <TR> <TD align="right"> TRE </TD> <TD align="right"> -0.54 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.71 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 0.73 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 0.25 </TD> <TD align="right"> 0.11 </TD> </TR>
  <TR> <TD align="right"> TAC </TD> <TD align="right"> -0.47 </TD> <TD align="right"> 0.71 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.69 </TD> <TD align="right"> 0.71 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 0.17 </TD> <TD align="right"> 0.12 </TD> </TR>
  <TR> <TD align="right"> ODC </TD> <TD align="right"> -0.71 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 0.69 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 0.42 </TD> <TD align="right"> 0.32 </TD> <TD align="right"> 0.02 </TD> </TR>
  <TR> <TD align="right"> MDC </TD> <TD align="right"> -0.75 </TD> <TD align="right"> 0.73 </TD> <TD align="right"> 0.71 </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD align="right"> MV </TD> <TD align="right"> -0.14 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 0.30 </TD> <TD align="right"> 0.42 </TD> <TD align="right"> 0.20 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.94 </TD> <TD align="right"> 0.07 </TD> </TR>
  <TR> <TD align="right"> ME </TD> <TD align="right"> -0.03 </TD> <TD align="right"> 0.25 </TD> <TD align="right"> 0.17 </TD> <TD align="right"> 0.32 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.94 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.01 </TD> </TR>
  <TR> <TD align="right"> MO </TD> <TD align="right"> -0.04 </TD> <TD align="right"> 0.11 </TD> <TD align="right"> 0.12 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> -0.01 </TD> <TD align="right"> 1.00 </TD> </TR>
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
## group   2    1.73   0.18
##       586
```

```r
leveneTest(ErrorRate ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    43.5 <2e-16 ***
##       586                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(TAC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value  Pr(>F)    
## group   2    22.7 3.1e-10 ***
##       586                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(TRE ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    47.9 <2e-16 ***
##       586                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(MDC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    57.3 <2e-16 ***
##       586                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(ODC ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)    
## group   2    78.3 <2e-16 ***
##       586                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(ME ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value  Pr(>F)    
## group   2    15.9 1.9e-07 ***
##       586                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(MV ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value  Pr(>F)    
## group   2    28.1 2.3e-12 ***
##       586                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
leveneTest(MO ~ Device,data=aggData.noLearn, center="mean")
```

```
## Levene's Test for Homogeneity of Variance (center = "mean")
##        Df F value Pr(>F)  
## group   2    4.54  0.011 *
##       586                 
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
## [1] "Mouse:  0.328280523307287  Touchpad:  NA  LeapMotionTouchless:  0.272412023407061  Max/Min:  NA"
## [1] "Variances for  ErrorRate"
## [1] "Mouse:  0.00204924623115578  Touchpad:  NA  LeapMotionTouchless:  0.00865723541846473  Max/Min:  NA"
## [1] "Variances for  MovementTime"
## [1] "Mouse:  0.0097213829787828  Touchpad:  NA  LeapMotionTouchless:  0.267845210916232  Max/Min:  NA"
## [1] "Variances for  TRE"
## [1] "Mouse:  0.00629771077610274  Touchpad:  NA  LeapMotionTouchless:  0.0892929065505898  Max/Min:  NA"
## [1] "Variances for  TAC"
## [1] "Mouse:  0.111870351758794  Touchpad:  NA  LeapMotionTouchless:  0.42988504884486  Max/Min:  NA"
## [1] "Variances for  ODC"
## [1] "Mouse:  0.283066890005583  Touchpad:  NA  LeapMotionTouchless:  5.29992119779354  Max/Min:  NA"
## [1] "Variances for  MDC"
## [1] "Mouse:  0.737200558347292  Touchpad:  NA  LeapMotionTouchless:  7.63381840469311  Max/Min:  NA"
## [1] "Variances for  MV"
## [1] "Mouse:  51.9772119281089  Touchpad:  NA  LeapMotionTouchless:  50.8688729704054  Max/Min:  NA"
## [1] "Variances for  ME"
## [1] "Mouse:  32.7190723593315  Touchpad:  NA  LeapMotionTouchless:  31.5091018579891  Max/Min:  NA"
## [1] "Variances for  MO"
## [1] "Mouse:  40.6256630122052  Touchpad:  NA  LeapMotionTouchless:  27.8664566755512  Max/Min:  NA"
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
## Device     1    0.1    0.05    0.01   0.94
## Residuals  8   74.3    9.28               
## 
## Error: factor(UserId):Device
##           Df Sum Sq Mean Sq F value Pr(>F)    
## Device     2    805     402     162  3e-12 ***
## Residuals 18     45       2                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: Within
##            Df Sum Sq Mean Sq F value Pr(>F)
## Residuals 559   55.7  0.0997
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
## Device        2   37.6   18.82    59.4 <2e-16 ***
## Residuals   586  185.6    0.32                   
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
## LeapMotion-Mouse               0.2860 0.1538 0.4182     0
## LeapMotionTouchless-Mouse      0.6221 0.4880 0.7562     0
## LeapMotionTouchless-LeapMotion 0.3361 0.2020 0.4702     0
```

```r
# Anova for TRE
aov.TRE <- aov(TRE~Device, data=aggData.noLearn)
#aov.TRE
summary(aov.TRE)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2    8.8    4.40    85.5 <2e-16 ***
## Residuals   586   30.1    0.05                   
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
##                                   diff     lwr     upr  p adj
## LeapMotion-Mouse               0.25200  0.1987 0.30529 0.0000
## LeapMotionTouchless-Mouse      0.26406  0.2100 0.31812 0.0000
## LeapMotionTouchless-LeapMotion 0.01206 -0.0420 0.06612 0.8595
```

```r
# Anova for MDC
aov.MDC <- aov(MDC~Device, data=aggData.noLearn)
#aov.MDC
summary(aov.MDC)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2   1790     895     191 <2e-16 ***
## Residuals   586   2751       5                   
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
##                                  diff    lwr   upr p adj
## LeapMotion-Mouse               3.0987 2.5896 3.608     0
## LeapMotionTouchless-Mouse      4.0913 3.5749 4.608     0
## LeapMotionTouchless-LeapMotion 0.9927 0.4762 1.509     0
```

```r
# Anova for ODC
aov.ODC <- aov(ODC~Device, data=aggData.noLearn)
#aov.ODC
summary(aov.ODC)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)    
## Device        2   1069     535     191 <2e-16 ***
## Residuals   586   1639       3                   
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
##                                 diff    lwr   upr p adj
## LeapMotion-Mouse               2.484 2.0914 2.877 0e+00
## LeapMotionTouchless-Mouse      3.118 2.7197 3.517 0e+00
## LeapMotionTouchless-LeapMotion 0.634 0.2354 1.033 6e-04
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
## Device        2   2075    1038    11.4 1.4e-05 ***
## Residuals   586  53355      91                    
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
##                                  diff    lwr    upr  p adj
## LeapMotion-Mouse                4.099  1.857  6.341 0.0001
## LeapMotionTouchless-Mouse       0.295 -1.979  2.569 0.9501
## LeapMotionTouchless-LeapMotion -3.804 -6.079 -1.530 0.0003
```

```r
# Anova for ME
aov.ME <- aov(ME~Device, data=aggData.noLearn)
#aov.ME
summary(aov.ME)
```

```
##              Df Sum Sq Mean Sq F value  Pr(>F)    
## Device        2    907     454    7.79 0.00046 ***
## Residuals   586  34119      58                    
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
## LeapMotion-Mouse                1.561 -0.2315  3.3544 0.1022
## LeapMotionTouchless-Mouse      -1.493 -3.3116  0.3261 0.1316
## LeapMotionTouchless-LeapMotion -3.054 -4.8730 -1.2354 0.0003
```

```r
# Anova for MO
aov.MO <- aov(MO~Device, data=aggData.noLearn)
#aov.MO
summary(aov.MO)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)
## Device        2     38    18.8    0.43   0.65
## Residuals   586  25376    43.3
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
## LeapMotion-Mouse                0.6090 -0.9372 2.155 0.6244
## LeapMotionTouchless-Mouse       0.2432 -1.3254 1.812 0.9295
## LeapMotionTouchless-LeapMotion -0.3658 -1.9344 1.203 0.8475
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
## -2.350 -0.804 -0.026  0.849  4.019 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.1075     0.0651    63.1   <2e-16 ***
## TRE          -2.7062     0.1742   -15.5   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.09 on 587 degrees of freedom
## Multiple R-squared:  0.291,	Adjusted R-squared:  0.29 
## F-statistic:  241 on 1 and 587 DF,  p-value: <2e-16
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
## -2.263 -0.626 -0.008  0.565  3.346 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.6452     0.0636    73.0   <2e-16 ***
## ODC          -0.4290     0.0174   -24.7   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.905 on 587 degrees of freedom
## Multiple R-squared:  0.509,	Adjusted R-squared:  0.508 
## F-statistic:  608 on 1 and 587 DF,  p-value: <2e-16
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
## -2.139 -0.622 -0.099  0.594  3.365 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.7006     0.0919    62.0   <2e-16 ***
## MDC          -0.3479     0.0127   -27.4   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.855 on 587 degrees of freedom
## Multiple R-squared:  0.561,	Adjusted R-squared:  0.561 
## F-statistic:  751 on 1 and 587 DF,  p-value: <2e-16
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
## -1.983 -0.578 -0.063  0.547  3.960 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.4821     0.0926   59.22  < 2e-16 ***
## ODC          -0.1932     0.0258   -7.49  2.5e-13 ***
## MDC          -0.2297     0.0199  -11.53  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.818 on 586 degrees of freedom
## Multiple R-squared:   0.6,	Adjusted R-squared:  0.598 
## F-statistic:  439 on 2 and 586 DF,  p-value: <2e-16
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
## -0.7947 -0.1725 -0.0206  0.1257  1.1935 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.23095    0.02977    7.76  3.9e-14 ***
## TRE         -0.28351    0.07027   -4.03  6.2e-05 ***
## ODC          0.15305    0.00949   16.12  < 2e-16 ***
## MDC          0.12964    0.00645   20.11  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.256 on 585 degrees of freedom
## Multiple R-squared:  0.845,	Adjusted R-squared:  0.844 
## F-statistic: 1.06e+03 on 3 and 585 DF,  p-value: <2e-16
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
## -2.311 -0.808 -0.082  0.839  3.856 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    4.642      0.162   28.58  < 2e-16 ***
## TRE           -2.077      0.246   -8.44  2.5e-16 ***
## TAC           -0.368      0.103   -3.58  0.00037 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.08 on 586 degrees of freedom
## Multiple R-squared:  0.306,	Adjusted R-squared:  0.304 
## F-statistic:  129 on 2 and 586 DF,  p-value: <2e-16
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
