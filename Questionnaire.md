Subjective questionnaire
========================================================




```r
#install.packages(c("ggplot2", "doBy"))
require(ggplot2)
```

```
## Loading required package: ggplot2
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
require(psych)
```

```
## Loading required package: psych
## 
## Attaching package: 'psych'
## 
## The following object is masked from 'package:ggplot2':
## 
##     %+%
```

```r
library(reshape2)
require(xtable)
```

```
## Loading required package: xtable
```

```r
data <- read.csv(file="data/Resultados.csv", head=TRUE, skip=1, sep=",")
names(data)
```

## Demographics


```r
s <- describe(data[,c(4:9)])
s <- cbind(Device=rownames(s), s)

rownames(s)<-NULL

print(xtable(s), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Thu Aug 28 14:17:59 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Device </TH> <TH> vars </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> skew </TH> <TH> kurtosis </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Gender* </TD> <TD align="right">   1 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.44 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.44 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.19 </TD> <TD align="right"> -2.17 </TD> <TD align="right"> 0.18 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Age* </TD> <TD align="right">   2 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.33 </TD> <TD align="right"> 0.87 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 2.33 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 0.46 </TD> <TD align="right"> -0.76 </TD> <TD align="right"> 0.29 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> How.often.do.you.use.a.computer.* </TD> <TD align="right">   3 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.11 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.11 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.07 </TD> <TD align="right"> 2.63 </TD> <TD align="right"> 0.11 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> How.often.do.you.use.a.computer.mouse.* </TD> <TD align="right">   4 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.11 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.11 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.07 </TD> <TD align="right"> 2.63 </TD> <TD align="right"> 0.11 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> How.often.do.you.use.a.Leap.Motion.* </TD> <TD align="right">   5 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right">  </TD> <TD align="right">  </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> Timestamp.1* </TD> <TD align="right">   6 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 2.74 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 2.97 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 8.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> -1.60 </TD> <TD align="right"> 0.91 </TD> </TR>
   </TABLE>

```r
write.table(s, file = "tables/questionnaire-demographics-summary.csv", sep=",", row.names=FALSE)
```


```r
s <- describe(data[,c(10:21)])
s <- cbind(Device=rownames(s), s)

rownames(s)<-NULL
print(xtable(s), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Thu Aug 28 14:17:59 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Device </TH> <TH> vars </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> skew </TH> <TH> kurtosis </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Force.required.for.actuation </TD> <TD align="right">   1 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.78 </TD> <TD align="right"> 0.44 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.78 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -1.12 </TD> <TD align="right"> -0.80 </TD> <TD align="right"> 0.15 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Smoothness.during.operation </TD> <TD align="right">   2 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.44 </TD> <TD align="right"> 0.73 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.44 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> -0.70 </TD> <TD align="right"> -1.03 </TD> <TD align="right"> 0.24 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Effort.required.for.operation </TD> <TD align="right">   3 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 0.71 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> -0.42 </TD> <TD align="right"> -1.22 </TD> <TD align="right"> 0.24 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> Accuracy </TD> <TD align="right">   4 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.67 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.67 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.59 </TD> <TD align="right"> -1.81 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Operation.Speed </TD> <TD align="right">   5 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.44 </TD> <TD align="right"> 1.01 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.44 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -1.47 </TD> <TD align="right"> 0.82 </TD> <TD align="right"> 0.34 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> General.comfort </TD> <TD align="right">   6 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.56 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.56 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.19 </TD> <TD align="right"> -2.17 </TD> <TD align="right"> 0.18 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> Overall.operation.of.input.device </TD> <TD align="right">   7 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.89 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.89 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -2.07 </TD> <TD align="right"> 2.63 </TD> <TD align="right"> 0.11 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> Finger.fatigue </TD> <TD align="right">   8 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.89 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.89 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -2.07 </TD> <TD align="right"> 2.63 </TD> <TD align="right"> 0.11 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> Wrist.fatigue </TD> <TD align="right">   9 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.67 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.67 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -0.59 </TD> <TD align="right"> -1.81 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD> Arm.fatigue </TD> <TD align="right">  10 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.22 </TD> <TD align="right"> 1.09 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.22 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -0.89 </TD> <TD align="right"> -0.78 </TD> <TD align="right"> 0.36 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> Shoulder.fatigue </TD> <TD align="right">  11 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.78 </TD> <TD align="right"> 0.44 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.78 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> -1.12 </TD> <TD align="right"> -0.80 </TD> <TD align="right"> 0.15 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD> Neck.fatigue </TD> <TD align="right">  12 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right">  </TD> <TD align="right">  </TD> <TD align="right"> 0.00 </TD> </TR>
   </TABLE>

```r
write.table(s, file = "tables/questionnaire-mouse-summary.csv", sep=",", row.names=FALSE)
```


```r
s <- describe(data[,c(23:34)])
s <- cbind(Device=rownames(s), s)

rownames(s)<-NULL
print(xtable(s), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Thu Aug 28 14:17:59 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Device </TH> <TH> vars </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> skew </TH> <TH> kurtosis </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Force.required.for.actuation.1 </TD> <TD align="right">   1 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.78 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.78 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 0.34 </TD> <TD align="right"> -1.40 </TD> <TD align="right"> 0.49 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Smoothness.during.operation.1 </TD> <TD align="right">   2 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 1.12 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> -1.44 </TD> <TD align="right"> 0.37 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Effort.required.for.operation.1 </TD> <TD align="right">   3 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.11 </TD> <TD align="right"> 0.78 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 2.11 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> -0.15 </TD> <TD align="right"> -1.54 </TD> <TD align="right"> 0.26 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> Accuracy.1 </TD> <TD align="right">   4 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.89 </TD> <TD align="right"> 1.17 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.89 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 0.18 </TD> <TD align="right"> -0.88 </TD> <TD align="right"> 0.39 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Operation.Speed.1 </TD> <TD align="right">   5 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.22 </TD> <TD align="right"> 1.09 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 3.22 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.38 </TD> <TD align="right"> -0.26 </TD> <TD align="right"> 0.36 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> General.comfort.1 </TD> <TD align="right">   6 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.89 </TD> <TD align="right"> 0.78 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.89 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 0.15 </TD> <TD align="right"> -1.54 </TD> <TD align="right"> 0.26 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> Overall.operation.of.input.device.1 </TD> <TD align="right">   7 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.56 </TD> <TD align="right"> 0.88 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.56 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -0.15 </TD> <TD align="right"> -1.04 </TD> <TD align="right"> 0.29 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> Finger.fatigue.1 </TD> <TD align="right">   8 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.11 </TD> <TD align="right"> 1.36 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.11 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -1.22 </TD> <TD align="right"> 0.15 </TD> <TD align="right"> 0.45 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> Wrist.fatigue.1 </TD> <TD align="right">   9 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.56 </TD> <TD align="right"> 1.33 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 3.56 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.39 </TD> <TD align="right"> -1.03 </TD> <TD align="right"> 0.44 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD> Arm.fatigue.1 </TD> <TD align="right">  10 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.33 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 2.33 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 0.07 </TD> <TD align="right"> -1.37 </TD> <TD align="right"> 0.33 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> Shoulder.fatigue.1 </TD> <TD align="right">  11 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.78 </TD> <TD align="right"> 1.39 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.78 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> -1.54 </TD> <TD align="right"> 0.46 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD> Neck.fatigue.1 </TD> <TD align="right">  12 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.78 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.78 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.69 </TD> <TD align="right"> -1.18 </TD> <TD align="right"> 0.49 </TD> </TR>
   </TABLE>

```r
write.table(s, file = "tables/questionnaire-leapmotionscreentap-summary.csv", sep=",", row.names=FALSE)
```


```r
s <- describe(data[,c(36:47)])
s <- cbind(Device=rownames(s), s)

rownames(s)<-NULL
print(xtable(s), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Thu Aug 28 14:17:59 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Device </TH> <TH> vars </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> skew </TH> <TH> kurtosis </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Force.required.for.actuation.2 </TD> <TD align="right">   1 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.67 </TD> <TD align="right"> 1.50 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.67 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.48 </TD> <TD align="right"> -1.43 </TD> <TD align="right"> 0.50 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Smoothness.during.operation.2 </TD> <TD align="right">   2 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 1.41 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.29 </TD> <TD align="right"> -1.56 </TD> <TD align="right"> 0.47 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Effort.required.for.operation.2 </TD> <TD align="right">   3 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.78 </TD> <TD align="right"> 1.20 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.78 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -0.40 </TD> <TD align="right"> -1.56 </TD> <TD align="right"> 0.40 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> Accuracy.2 </TD> <TD align="right">   4 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 1.12 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -0.48 </TD> <TD align="right"> -1.44 </TD> <TD align="right"> 0.37 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Operation.Speed.2 </TD> <TD align="right">   5 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.78 </TD> <TD align="right"> 1.39 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.78 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -0.39 </TD> <TD align="right"> -1.86 </TD> <TD align="right"> 0.46 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD> General.comfort.2 </TD> <TD align="right">   6 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 2.67 </TD> <TD align="right"> 1.32 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 2.67 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -0.32 </TD> <TD align="right"> -1.81 </TD> <TD align="right"> 0.44 </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> Overall.operation.of.input.device.2 </TD> <TD align="right">   7 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 1.58 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 2.97 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.32 </TD> <TD align="right"> -1.53 </TD> <TD align="right"> 0.53 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD> Finger.fatigue.2 </TD> <TD align="right">   8 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 1.58 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.84 </TD> <TD align="right"> -1.15 </TD> <TD align="right"> 0.53 </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> Wrist.fatigue.2 </TD> <TD align="right">   9 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.89 </TD> <TD align="right"> 1.76 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 3.89 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.82 </TD> <TD align="right"> -1.29 </TD> <TD align="right"> 0.59 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD> Arm.fatigue.2 </TD> <TD align="right">  10 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 1.66 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.33 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> -0.32 </TD> <TD align="right"> -1.74 </TD> <TD align="right"> 0.55 </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> Shoulder.fatigue.2 </TD> <TD align="right">  11 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 3.67 </TD> <TD align="right"> 1.22 </TD> <TD align="right"> 4.00 </TD> <TD align="right"> 3.67 </TD> <TD align="right"> 1.48 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -0.16 </TD> <TD align="right"> -1.75 </TD> <TD align="right"> 0.41 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD> Neck.fatigue.2 </TD> <TD align="right">  12 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 4.33 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 5.00 </TD> <TD align="right"> 3.00 </TD> <TD align="right"> -1.26 </TD> <TD align="right"> 0.41 </TD> <TD align="right"> 0.33 </TD> </TR>
   </TABLE>

```r
write.table(s, file = "tables/questionnaire-leapmotionhandgrab-summary.csv", sep=",", row.names=FALSE)
```


```r
s <- describe(data[,c(49:53)])
s <- cbind(Device=rownames(s), s)

rownames(s)<-NULL
print(xtable(s), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-3 package -->
<!-- Thu Aug 28 14:18:00 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Device </TH> <TH> vars </TH> <TH> n </TH> <TH> mean </TH> <TH> sd </TH> <TH> median </TH> <TH> trimmed </TH> <TH> mad </TH> <TH> min </TH> <TH> max </TH> <TH> range </TH> <TH> skew </TH> <TH> kurtosis </TH> <TH> se </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Which.was.your.favorite.device.* </TD> <TD align="right">   1 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right">  </TD> <TD align="right">  </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> Of.the.used.gestures..which.one.did.you.find.more.comfortable.* </TD> <TD align="right">   2 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.22 </TD> <TD align="right"> 0.44 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.22 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.12 </TD> <TD align="right"> -0.80 </TD> <TD align="right"> 0.15 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Of.the.used.gestures..which.one.did.you.find.more.intuitive.* </TD> <TD align="right">   3 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.33 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.33 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.59 </TD> <TD align="right"> -1.81 </TD> <TD align="right"> 0.17 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD> Of.the.used.gestures..which.one.did.you.find.more.accurate.* </TD> <TD align="right">   4 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.44 </TD> <TD align="right"> 0.53 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.44 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.19 </TD> <TD align="right"> -2.17 </TD> <TD align="right"> 0.18 </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Of.the.used.gestures..which.one.did.you.liked.most.* </TD> <TD align="right">   5 </TD> <TD align="right"> 9.00 </TD> <TD align="right"> 1.11 </TD> <TD align="right"> 0.33 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 1.11 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.00 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 2.07 </TD> <TD align="right"> 2.63 </TD> <TD align="right"> 0.11 </TD> </TR>
   </TABLE>

```r
write.table(s, file = "tables/questionnaire-devicepreference-summary.csv", sep=",", row.names=FALSE)
```



```r
#####
m <- NULL
for (i in 10:21) {
  print(colnames(data)[i])
  c1 <- colnames(data)[i]
  c2 <- paste(c1,".1", sep="")
  c3 <- paste(c1,".2", sep="")
  
  m1 <- melt(data[,c("ID", c1, c2, c3)], id=c("ID"))
  colnames(m1) <- c("ID", "Device", c1) 
  levels(m1$Device) <- list( Mouse=c(c1),  LeapMotionScreenTap=c(c2), LeapMotionHandGrab=c(c3) )
  if ( is.null(m) ) {
    m <- m1
  } else{
    m<-cbind(m, c=m1[,c(c1)] )  
    colnames(m)[colnames(m) == "c"] <- c1
  }
  
}
```

```
## [1] "Force.required.for.actuation"
## [1] "Smoothness.during.operation"
## [1] "Effort.required.for.operation"
## [1] "Accuracy"
## [1] "Operation.Speed"
## [1] "General.comfort"
## [1] "Overall.operation.of.input.device"
## [1] "Finger.fatigue"
## [1] "Wrist.fatigue"
## [1] "Arm.fatigue"
## [1] "Shoulder.fatigue"
## [1] "Neck.fatigue"
```

```r
m<-melt(m, id=c("ID", "Device"))


ggplot(m, aes(Device, value, colour=Device)) + 
  geom_boxplot() +
  #coord_flip() +
  facet_wrap(  ~ variable,ncol=3)
```

![plot of chunk boxplots](figure/boxplots.png) 

```r
ggsave(file = "charts/questionnaire/iso-boxplot.pdf", width=21/2.54, height=29/2.54, dpi=100)
```


```r
describe <- describeBy(m$value, list(m$Device,m$variable), mat=TRUE)
colnames(describe)[colnames(describe)=="group1"] <- "Device"
colnames(describe)[colnames(describe)=="group2"] <- "variable"
#Reorder levels for chart
levels(describe$Device) <- list( Mouse=c("Mouse"), LeapMotionScreenTap=c("LeapMotionScreenTap"), LeapMotionHandGrab=c("LeapMotionHandGrab"))
    


#dfc <- summarySE(m, measurevar="value", groupvars=c("Device","variable"))

ggplot(describe, aes(Device, mean, colour=Device, fill=Device)) + 
  #stat_summary(fun.y="mean", geom="bar") + 
 geom_bar(stat="identity") +
  #coord_flip() +
  geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), colour="Black",
               width=.2,                    # Width of the error bars
               position=position_dodge(.9)) +
  facet_wrap(  ~ variable,ncol=3) +
  theme(legend.position="top", 
        legend.direction="horizontal") 
```

![plot of chunk barplot](figure/barplot.png) 

```r
ggsave(file = "charts/questionnaire/iso-barplot.pdf", width=16/2.54, height=20/2.54, dpi=100)
```



```r
describe$l <- ifelse(describe$Device=="Mouse", as.character(describe$variable), NA)

ggplot(describe, aes(x=variable, y=mean, group=Device, colour=Device, fill=Device, label=l) ) + 
    #stat_summary(fun.y="mean", geom="bar") + 
    geom_bar(stat="identity", width=.5, position = position_dodge(width=0.5)) +
    geom_text(angle=70, colour="Black", y=1.05,
              hjust=0, size=4) + 
    #geom_bar() +
    #coord_flip() +
    coord_cartesian(ylim=c(1.0, 5)) +
    ylab("Average score") +
    xlab("") +
    geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), colour="Black",
                  width=.2,                    # Width of the error bars
                  size = .1,
                  position=position_dodge(.5)) +
    #facet_wrap(  ~ variable,ncol=3) +
    theme(legend.position=c(.5,-0.1), legend.direction="horizontal", 
          axis.text.x = element_blank()) + 
    scale_fill_brewer(palette="Set1") + 
    scale_colour_brewer(palette="Set1")
```

```
## Warning: Removed 24 rows containing missing values (geom_text).
```

![plot of chunk barplotall](figure/barplotall.png) 

```r
    ggsave(file = "charts/questionnaire/iso-barplot-side.pdf", width=30/2.54, height=7/2.54, dpi=100)
```

```
## Warning: Removed 24 rows containing missing values (geom_text).
```


