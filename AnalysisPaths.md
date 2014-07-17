Title
========================================================
 
This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **Help** toolbar button for more details on using R Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```r
# This script generates the graphics with the various selection paths

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
GENERATE_INDIVIDUAL_PATHS <- FALSE
GENERATE_CIRCLEID_CHARTS <- FALSE
GENERATE_SEQUENCE_CHARTS <- TRUE

############################ data


files <- list.files(path="data", pattern="transformed.txt")
files
```

```
## [1] "transformed.txt"
```

```r
dataTransformed <- data.frame()
for (file in files) {
  print (file)
  dat = read.csv(paste("data/", file, sep=""), sep="", head = TRUE)
  print(nrow(dat))
  dataTransformed <- rbind(dataTransformed, dat)
}
```

```
## [1] "transformed.txt"
## [1] 1220113
```

```r
colnames(dataTransformed)
```

```
##  [1] "NumberDevice"      "UserId"            "Block"            
##  [4] "Sequence"          "NumberClicks"      "NumberCircles"    
##  [7] "CircleID"          "DistanceCenter"    "PixelStartCircleX"
## [10] "PixelStartCircleY" "PixelEndCircleX"   "PixelEndCircleY"  
## [13] "TargetWidth"       "ElapsedTime"       "MouseX"           
## [16] "MouseY"            "targetx"           "targety"          
## [19] "rx"                "ry"                "inside"           
## [22] "percentpath"       "speeds"            "accels"           
## [25] "displacement"      "distance"          "insidecount"      
## [28] "sampleid"
```

```r
#dataTransformed <- read.csv(file="data/transformed.txt", head=TRUE, sep="")

# change column name to get a nicer chart
colnames(dataTransformed)[colnames(dataTransformed)=="NumberDevice"] <- "Device"


# convert the column to factor and name the levels
dataTransformed$Device <- as.factor(dataTransformed$Device)
levels(dataTransformed$Device) <- list( LeapMotion=c(0),  Mouse=c(1), Touchpad=c(2), LeapMotionTouchless=c(4))
#levels(dataTransformed$Device) <- c("Mouse")


# Analyse only blocks after learning effect
dataTransformed <- dataTransformed[dataTransformed$Block>3,]
dataTransformed$cuts<-cut(dataTransformed$percentpath, 100)



#calculate the maximum and minimum y and x coords for setting the plots' scales
minX <- min(dataTransformed$rx)
maxX <- max(dataTransformed$rx)
minY <- min(dataTransformed$ry)
maxY <- max(dataTransformed$ry)
print ( paste("X scale: ", minX, maxX, " Y scale: ", minY, maxY))
```

```
## [1] "X scale:  -248.92007648789 1147.69479875111  Y scale:  -840.847746173676 611.567719242866"
```


```r
# plot the paths for each user and device. a single plot aggregates one entire sequence
if (GENERATE_SEQUENCE_CHARTS == TRUE) {
for (device in unique(dataTransformed$Device) ) {
  for (user in unique(dataTransformed$UserId)) {
    p <- ggplot(dataTransformed[dataTransformed$Device==device &
                                  dataTransformed$UserId==user,], 
                aes(x=rx, y=ry, group=Device, colour=Device )) +
      geom_path() +
      coord_cartesian(xlim = c(minX, maxX), ylim=c(minY, maxY)) +
      facet_grid(Block ~ Sequence) +
      ylab("Block") +
      xlab("Sequence") +
      theme(legend.position="none") +
      ggtitle(paste("Device: ", device, " User: ", user))
    p
    filename <- paste("charts/paths/byblocksequence", device, "-user-", user, ".pdf", sep="")
    print( filename  )
    ggsave(file = filename, width=21/2.54, height=29/2.54, dpi=100)
  }
}
}
```

```
## [1] "charts/paths/byblocksequenceMouse-user-0.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-1.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-10.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-11.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-2.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-3.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-4.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-5.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-6.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-7.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-8.pdf"
```

```
## [1] "charts/paths/byblocksequenceMouse-user-9.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-0.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-1.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-10.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-11.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-2.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-3.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-4.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-5.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-6.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-7.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-8.pdf"
```

```
## [1] "charts/paths/byblocksequenceTouchpad-user-9.pdf"
```

```
## [1] "charts/paths/byblocksequenceLeapMotionTouchless-user-0.pdf"
```

```
## Error: undefined columns selected
```


```r
# plot the paths for each user and device. a single plot aggregates one circleid
if (GENERATE_CIRCLEID_CHARTS == TRUE) {
for (device in unique(dataTransformed$Device) ) {
  for (user in unique(dataTransformed$UserId)) {
    p <- ggplot(dataTransformed[dataTransformed$Device==device &
                                  dataTransformed$UserId==user,], 
                aes(x=rx, y=ry, group=Device, colour=Device )) +
      geom_path() +
      coord_cartesian(xlim = c(minX, maxX), ylim=c(minY, maxY)) +
      facet_grid(CircleID ~ .) +
      ylab("Circle ID") +
      xlab("x") +
      theme(legend.position="none") +
      ggtitle(paste("Device: ", device, " User: ", user))
    p
    filename <- paste("charts/paths/bycircleid-", device, "-user-", user, ".pdf", sep="")
    print( filename )
    ggsave(file = filename, width=21/2.54, height=29/2.54, dpi=100)
  }
}
}
```


```r
# plot INDIVIDUAL paths for each user and device. 
# PRODUCES LOTS OF FILES
if (GENERATE_INDIVIDUAL_PATHS == TRUE) {
  for (device in unique(dataTransformed$Device) ) {
    for (user in unique(dataTransformed$UserId)) {
      for (block in unique(dataTransformed[dataTransformed$Device==device &
                                             dataTransformed$UserId==user,]$Block)) { 
        p <- ggplot(dataTransformed[dataTransformed$Device==device &
                                      dataTransformed$Block==block &
                                      dataTransformed$UserId==user,], 
                    aes(x=rx, y=ry, group=Device, colour=Device )) +
          geom_path() +
          coord_cartesian(xlim = c(minX, maxX), ylim=c(minY, maxY)) +
          facet_grid(CircleID ~ Sequence) +
          ylab("Circle Id") +
          xlab("Block") +
          theme(legend.position="none") +
          ggtitle(paste("Device: ", device, " User: ", user, " Block: ", block))
        p
        filename <- paste("charts/paths/individual-", device, "-user-", user, "-block-", block,".pdf", sep="")
        print( filename )
        ggsave(file = filename, width=29/2.54, height=35/2.54, dpi=100)
      }
    }
  }
}
```


```r
#http://www.sigchi.org/chi96/proceedings/papers/Mithal/Akm_txt.htm
# (a) (b) (c) (d)
# Figure 3. Path variations. (a) target re-entry (b) task axis crossing (c) movement direction change (d) orthogonal direction change
# An example where target re-entry was not used, yet may have helped, is Akamatsu et al.???s evaluation of a mouse with tactile feedback [2]. This
# study found a main effect on fine
# positioning time ??? the time to select the target after the pointer entered the target region. With tactile feedback, users exhibited a lower fine positioning time than under the no feedback, auditory feedback, and colour feedback conditions. A measure such as target re-entry may also serve to reveal differences among
# on-target feedback conditions, for
# example.
# http://www.yorku.ca/mack/Ergonomics.html




# Descriptive stats
describeBy(dataTransformed[, c("speeds", "accels", "insidecount", "displacement", "distance")], dataTransformed[, c("Device")])
```

```
## group: LeapMotion
## NULL
## -------------------------------------------------------- 
## group: Mouse
##              vars      n   mean       sd median trimmed     mad     min
## speeds          1 154349 865.34  1606.72  204.0  463.42  302.39       0
## accels          2 154349  24.83 60186.76    0.0 -185.66 9958.42 -776076
## insidecount     3 154349   8.76     5.69    8.0    8.25    2.97       0
## displacement    4 154349 455.95   275.08  607.2  479.67  119.18       0
## distance        5 154349 488.91   311.84  619.9  497.01  181.81       0
##                 max   range  skew kurtosis     se
## speeds        20522   20522  3.17    12.62   4.09
## accels       623353 1399430  0.09    13.04 153.20
## insidecount     145     145 15.71   361.11   0.01
## displacement   1151    1151 -0.77    -1.11   0.70
## distance       2616    2616 -0.29    -0.05   0.79
## -------------------------------------------------------- 
## group: Touchpad
##              vars      n   mean       sd median trimmed     mad     min
## speeds          1 230530 586.17  1417.23   40.0   222.8   59.30       0
## accels          2 230530   2.19 53510.05    0.0  -245.3 2372.16 -941578
## insidecount     3 230530  17.59     9.29   16.0    16.5    4.45       1
## displacement    4 230530 466.92   280.00  633.0   493.5   89.59       0
## distance        5 230530 507.98   326.41  645.8   514.8  172.28       0
##                  max   range  skew kurtosis     se
## speeds         26446   26446  4.22    23.47   2.95
## accels       1057850 1999428  0.18    27.60 111.45
## insidecount      151     150  6.82    77.25   0.02
## displacement    1026    1026 -0.86    -1.02   0.58
## distance        2422    2422 -0.23     0.22   0.68
## -------------------------------------------------------- 
## group: LeapMotionTouchless
##              vars      n   mean       sd median trimmed     mad      min
## speeds          1 319537 394.85   808.14  56.57  203.88   83.87        0
## accels          2 319537   4.52 32499.28   0.00 -144.11 3354.74 -1524617
## insidecount     3 319537  30.13    17.48  25.00   27.67   11.86        1
## displacement    4 319537 488.48   264.73 646.31  521.85   62.30        0
## distance        5 319537 611.79   572.63 686.02  576.04  166.64        0
##                  max   range  skew kurtosis    se
## speeds         39052   39052  7.03   157.80  1.43
## accels       1562072 3086689  0.73   303.79 57.49
## insidecount      165     164  1.79     5.16  0.03
## displacement    1148    1148 -1.00    -0.68  0.47
## distance       13576   13576  8.38   144.59  1.01
```

```r
agg <- aggregate(dataTransformed[, c("insidecount", "speeds", "accels")], dataTransformed[, c("Device", "UserId", "Block", "Sequence", "CircleID")], mean)
View(agg)
describeBy(agg$x*25, agg$Device)
```

```
## Error: arguments must have same length
```

```r
agg <- aggregate(dataTransformed[, c("displacement", "distance")], dataTransformed[, c("Device", "UserId", "Block", "Sequence", "CircleID")], max)
View(agg)
describeBy(agg$x*25, agg$Device)
```

```
## Error: arguments must have same length
```

```r
d <- dataTransformed[dataTransformed$Device=="Mouse" & dataTransformed$Block==4 & dataTransformed$UserId ==1 & dataTransformed$Sequence==1 & dataTransformed$CircleID==1, ]

d <- dataTransformed[dataTransformed$Device=="Mouse" & dataTransformed$Block==4 & dataTransformed$UserId ==1 & dataTransformed$Sequence==1, ]

#View()

#plot(d$speeds)
ggplot(dataTransformed, aes(x=sampleid*25, y=speeds, group=Device, colour=Device)) +
    geom_path() +
    geom_point()
```

![plot of chunk analysis](figure/analysis1.png) 

```r
speed<-aggregate(dataTransformed$speeds, dataTransformed[,c("sampleid", "Device")], mean)
ggplot(speed, aes(x=sampleid, y=x, group=Device, colour=Device)) +
    #geom_smooth() +
    geom_path()
```

![plot of chunk analysis](figure/analysis2.png) 

```r
speed<-aggregate(dataTransformed$speeds, dataTransformed[,c("cuts", "Device")], mean)
ggplot(speed, aes(x=cuts, y=x, group=Device, colour=Device)) +
    #geom_smooth() +
    geom_path()
```

![plot of chunk analysis](figure/analysis3.png) 

```r
accel<-aggregate(dataTransformed$accels, dataTransformed[,c("cuts", "Device")], mean)
ggplot(accel, aes(x=cuts, y=x, group=Device, colour=Device)) +
    #geom_smooth() +
    geom_path()
```

![plot of chunk analysis](figure/analysis4.png) 

