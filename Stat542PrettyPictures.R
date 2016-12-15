getwd()
setwd("/Users/stevenhurwitt/Documents/Illinois/Stat542/FINAL/")

#save.image("/Users/stevenhurwitt/Documents/Illinois/Stat542/FINAL/finalworkspace.Rdata")
load("/Users/stevenhurwitt/Documents/Illinois/Stat542/FINAL/finalworkspace.Rdata")

library(ggplot2)
library(corrplot)

#######"Data Polishing"
descript = c(".avg", ".sd", ".min", ".max", ".med")
n = length(descript)

var = c("TC", "NC24hr", "NCT1T2", "NCafterpub", "24hrT1diff",
        "TC.L", "NC24hr.L", "NCT1T2.L", "NCafterpub.L", "24hrT1diff.L")
next.var = c("time", "length")
m = length(var)

new.var = NULL
for (i in 1:m){
  sub.i = paste(var[i], descript, sep = "")
  new.var = c(new.var,sub.i)
}

new.var = c(new.var, next.var)
new.var = c(new.var, colnames(blog.train)[63:281])

colnames(blog.train) = new.var
colnames(blog.train)[272:281] = c(seq(from=1, to=9, by=1), "y")
colnames(blog.test) = colnames(blog.train)

######Exploratory Data Analysis

#function to plot histograms w/ ggplot using training data
#inputs: variable of interest as a vector (var), 
#a name to use for the title and X axis, also as a character string,
#the number bin width, & the dataset to use
#output: prints the final histogram, to be exported as .png for latex

gg.hist = function(var, var.name, bin.width, my.data){
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  rando.color = sample(cbPalette, size = 1)
  
  gg.hist.plot = ggplot() + geom_histogram(aes(x = var), binwidth = bin.width, fill = rando.color, data = my.data)
  gg.hist.plot = gg.hist.plot + ggtitle(paste("Histogram of ", var.name)) + xlab(var.name) + ylab("Count")
  gg.hist.plot = gg.hist.plot + theme(legend.position = "none")
  print(gg.hist.plot)
}

#look at averages before & after
gg.hist(blog.train$TC.avg, "Avgerage Comments Before", 30, blog.train)
gg.hist(blog.train$NC24hr.avg, "Avgerage Comments 24 hrs Before", 10, blog.train)
gg.hist(blog.train$NCT1T2.avg, "Average Comments 48-24 hrs Before", 10, blog.train)

#avg of comments after publication
gg.hist(blog.train$NCafterpub.avg, "Average Comments After Publication", 40, blog.train)

#...medians
gg.hist(blog.train$TC.med, "Median Comments Before", 70, blog.train)
gg.hist(blog.train$NC24hr.med, "Median Comments 24 hrs Before", 30, blog.train)
gg.hist(blog.train$NCafterpub.med, "Median Comments After Publication", 35, blog.train)
gg.hist(blog.train$NCT1T2.med, "Average Comments 48-24 hrs Before", 30, blog.train)


#...medians (without outliers)
new.med = blog.train$TC.med[blog.train$TC.med < 400]
new.data = blog.train[blog.train$TC.med %in% new.med,]

new.med2 = blog.train$NC24hr.med[blog.train$NC24hr.med < 150]
new.data2 = blog.train[blog.train$NC24hr.med %in% new.med2,]

new.med3 = blog.train$NCafterpub.med[blog.train$NCafterpub.med < 200]
new.data3 = blog.train[blog.train$NCafterpub.med %in% new.med3,]

new.med4 = blog.train$NCT1T2.med[blog.train$NCT1T2.med < 100]
new.data4 = blog.train[blog.train$NCT1T2.med %in% new.med4,]

gg.hist(new.med, "Median Comments Before", 20, new.data)
gg.hist(new.med2, "Median Comments 24 hrs Before", 3, new.data2)
gg.hist(new.med3, "Median Comments After Publication", 10, new.data3)
gg.hist(new.med4, "Median Comments 48-24 hrs Before", 2, new.data4)


#sd's of TC, NC24hr, NCT1T2, NCafterpub
gg.hist(blog.train$TC.sd, "Standard Deviation of Comments Before", 20, blog.train)
gg.hist(blog.train$NC24hr.sd, "Standard Deviation of Comments 24 hrs Before", 20, blog.train)
gg.hist(blog.train$NCT1T2.sd, "Standard Deviation of Comments 48-24 hrs Before", 20, blog.train)
gg.hist(blog.train$NCafterpub.sd, "Standard Deviation of Comments After Publication", 20, blog.train)


#####Correlation Matrix
sds = apply(as.matrix(X), 2, FUN = sd)
nonzero.sd.X = subset(X, sds > 0)
X2 = X[,-(63:262)]

cor.mat = cor(X2)
par(cex = .6)
corrplot(cor.mat, type = "lower", method = "color", order = "AOE", tl.col = "black", tl.srt = 45)

#Histogram of Y
gg.hist(blog.train$y, "Comments within 24 Hours (y)", 30, blog.train)

new.y = blog.train$y[blog.train$y < 50]
new.data4 = blog.train[blog.train$y %in% new.y,]
gg.hist(new.y, "Comments within 24 Hours (y)", 1, new.data4)
