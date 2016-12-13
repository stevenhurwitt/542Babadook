#########Computer Stuff
getwd()
setwd("/Users/stevenhurwitt/Documents/Illinois/Stat542/FINAL/")

#save.image("/Users/stevenhurwitt/Documents/Illinois/Stat542/FINAL/finalworkspace.Rdata")
load("/Users/stevenhurwitt/Documents/Illinois/Stat542/FINAL/finalworkspace.Rdata")

library(ggplot2)

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
#a name to use for the title and X axis, also as a character string
#& the number bin width
#output: prints the final histogram, to be exported as .png for latex

gg.hist = function(var, var.name, bin.width){
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  rando.color = sample(cbPalette, size = 1)
  
  gg.hist.plot = ggplot() + geom_histogram(aes(x = var, fill = rando.color), binwidth = bin.width, data = blog.train)
  gg.hist.plot = gg.hist.plot + ggtitle(paste("Histogram of ", var.name)) + xlab(var.name) + ylab("Count")
  gg.hist.plot = gg.hist.plot + guides(fill = F)
  print(gg.hist.plot)
}

gg.hist(TC.max, "Max Comments", 70)

