
library(ggplot2)
df <- read.csv(file = 'uswages.csv')

################################################################################
#a)
#using simple dot plot
ggplot(df, aes(x = educ, y = wage))+
  geom_point(alpha = .25,color="#f4a261")+labs(title ="Years of education/ weekly Wage ")+
  xlab("Years of education") + ylab("Weekly Wage")+theme(plot.title = element_text(hjust = 0.5))

#using stat_bin2d plot
ggplot(df, aes(x = educ, y = wage))+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 100))+
  labs(title ="Years of education/ weekly Wage ")+
  xlab("Years of education") + ylab("Weekly Wage")+theme(plot.title = element_text(hjust = 0.5))

#####
#log of responce 

#using simple dot plot
ggplot(df, aes(x = educ, y = log(wage)))+
  geom_point(alpha = .25,color="#f4a261")+labs(title ="Years of education/log (weekly Wage)")+
  xlab("Years of education") + ylab("log (weekly Wage)")+theme(plot.title = element_text(hjust = 0.5))


#using stat_bin2d plot
ggplot(df, aes(x = educ, y = log(wage)))+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 100))+
  labs(title ="Years of education/ log (weekly Wage) ")+
  xlab("Years of education") + ylab("log (weekly Wage)")+theme(plot.title = element_text(hjust = 0.5))

###############################################################################
#b)
our_line<-smooth.spline(x= df$educ, y = df$wage)
plot(df$educ,df$wage,col="orange",main="Years of education/ weekly Wage")
lines(our_line,col="blue")
###############################################################################
#c)

ggplot(df, aes(x=as.numeric(educ),y=wage)) + geom_point(alpha=0.25) + geom_smooth(method="loess") +
  labs(title ="Years of education/log (weekly Wage)")+
  xlab("Years of education") + ylab("log (weekly Wage)")+theme(plot.title = element_text(hjust = 0.5))
###############################################################################
#d)
df$educ<-as.factor(df$educ)
Means_vector<-levels(df$educ)
Median_vector<-levels(df$educ)
for (i in 1:length(Median_vector))
{
  Median_vector[i]<-median(df[df$educ==levels(df$educ)[i],]$wage)
  Means_vector[i]<-mean(df[df$educ==levels(df$educ)[i],]$wage)
}
levels(df$educ)
Means_vector
Median_vector
plot(Means_vector~levels(df$educ),col='red', main="Means and medians",pch=1)
points(Median_vector~levels(df$educ),col='blue',pch=2)
legend(x = 'left',cex=.8,col=c("red","blue"),pch=c(1,2),
       legend = c('Means', 'Medians'))


###############################################################################
#e)
first_qunitel<-levels(df$educ)
second_qunitel<-levels(df$educ)
for (i in 1:length(Median_vector))
{
  first_qunitel[i]<-quantile((df[df$educ==levels(df$educ)[i],]$wage))[2]
 second_qunitel[i]<-quantile((df[df$educ==levels(df$educ)[i],]$wage))[4]
}

plot(as.numeric(df$educ),df$wage,col="cyan", main="quantile and medians",pch=1)
points(Median_vector~levels(df$educ),col='black',pch=2)
points(first_qunitel~levels(df$educ),col='blue',pch=3)
points(second_qunitel~levels(df$educ),col='red',pch=3)
legend(x = 'left',cex=.8,col=c("cyan","black","blue",'red'),pch=c(1,2,3,3),
       legend = c('data','Medians', 'First quantile', 'Second quantile'))


###############################################################################
#f)
ggplot(df, aes(x=as.numeric(educ),y=log(wage))) + geom_point(alpha=0.25) + geom_smooth(method="loess") +
  labs(title ="Years of education/log (weekly Wage)")+
  xlab("Years of education") + ylab("log (weekly Wage)")+theme(plot.title = element_text(hjust = 0.5))

