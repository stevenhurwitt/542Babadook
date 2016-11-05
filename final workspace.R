getwd()
setwd("/Users/stevenhurwitt/Documents/Illinois/Stat 542/FINAL/BlogFeedback")
blog.train = read.csv("blogData_train.csv")

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
