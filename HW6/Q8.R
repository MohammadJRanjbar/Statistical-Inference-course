df <- read.table("prostate.txt",sep="\t", header=TRUE)
###############################################################################
#a)

plot(df$age,df$lweight, main="Scatter plot")

###############################################################################
#b)
plot(df$age,df$lweight, main="Kernel")
lines(ksmooth(df$age, df$lweight, "normal", bandwidth = 5), col = 3)


###############################################################################
#c)

our_line<-smooth.spline(x= df$age, y = df$lweight)

plot(df$age,df$lweight,col="orange", main="smooth.spline")
lines(our_line,col="blue")


###############################################################################
#d)

ggplot(df, aes(x=age,y=lweight)) + geom_point(alpha=0.25) + geom_smooth(method="loess") +
  labs(title ="Age and lweight")+
  xlab("Age") + ylab("lweight")+theme(plot.title = element_text(hjust = 0.5))

plx<-lowess(df$lweight ~ df$age)
plot(df$lweight~df$age)
lines(plx)


###############################################################################
#e)

plot(df$age,df$lweight, main="kerenl & smooth spline & loess lines")
lines(ksmooth(df$age, df$lweight, "normal", bandwidth = 5), col = "blue")
lines(our_line,col="red")
lines(plx,col="green")
legend(x = 'bottom',cex=.8,col=c("blue","red",'green'),
       legend = c('Kernel', 'smooth.spline', 'loess'),lty=1)

###############################################################################
#f)
library("ggplot2")
library(mgcv)
amod <- gam(lweight ~ s(age,lpsa), data=df)
vis.gam(amod, col="gray", ticktype="detailed",theta=-185)

###############################################################################
#g)

plot(df$age,df$lweight-plx$y,main="Resdual")
abline(h=0,col='red')
