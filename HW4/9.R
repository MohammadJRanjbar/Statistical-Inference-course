df <- read.csv(file = 'Houses.csv')
library(ggplot2)
df$City=factor(df$City)
X <- df[df$City=="London",]  # London group
Y <- df[df$City=="Berlin",]   # Berlin group
ttest <- t.test(X$Area..Meter., Y$Area..Meter.,pool.sd = FALSE)
pval <- ttest$p.value          # p-value
pval
delta <- -diff(ttest$estimate)  # effect size
delta.CI <- ttest$conf.int    # 95% confidence interval 
n <- length(df$City)  # the number of observations to sample
n
B <- 1000  # the number of bootstrap samples
test.stat1 <- abs(mean(df$Area..Meter.[df$City=="London"]) - mean(df$Area..Meter.[df$City=="Berlin"]))  #diff in means
variable <- df$Area..Meter.  # the variable we will resample from

# now, get those bootstrap samples (without loops!)
BootstrapSamples <- matrix( sample(variable, size= n*B, replace=TRUE), 
                            nrow=n, ncol=B)
# let's take a moment to discuss what that code is doing...
dim(BootstrapSamples)

# now, calculate the means (Yc and Ym) for each of the bootstrap samples
#  (the inefficeint, but transparent way...best to start simple, and once
#   working well, then make code more efficent)
# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)
Boot.test.stat2 <- rep(0,B)
# run through a loop, each time calculating the bootstrap test.stat
#  NOTE: could make this faster by writing a "function" and then
#        using "apply" to apply it to columns of the "BootSamples"
for (i in 1:B){
  # calculate the boot-test-stat1 and save it
  Boot.test.stat1[i] <- abs( mean(BootstrapSamples[1:12,i]) - 
                               mean(BootstrapSamples[13:23,i]) )
}


# and, take a look at the first 20 Bootstrap-TEST STATS for 1 and 2
round(Boot.test.stat1[1:20], 1)

# and, let's calculate the bootstrap p-value...
# notice how we can ask R a true/false question...(for the first 20)
(Boot.test.stat1 >= test.stat1)[1:20]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
#...calculate the p-value
mean( Boot.test.stat1 >= test.stat1)

### in a "real-world" what would you want to conclude here
table(d$feed)

# let's take a look at a density plot of all the Bootstrap test-stats, and 
# add in our Observed test stat
plot(density(Boot.test.stat1), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Bootstrap Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)
quantile(Boot.test.stat1, c(.975,.025))

