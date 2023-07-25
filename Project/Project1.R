################################################################################
################################################################################
################################################################################
#Question 0) 

library(ggplot2)
df <- read.csv(file = 'Airbnb_Open_Data.csv')
###############################################################################
#B) number of features and cases
df <- df[ , ! names(df) %in% c("X")]
df<-df[!duplicated(df), ]
df <- df[ , ! names(df) %in% c("NAME","id","host.id","host.name","country")]
n_col<- ncol(df)
n_row<- nrow(df)
n_col
n_row
###############################################################################
#C) number Nan Values
summary(df)

table(df$minimum.nights)
df['minimum.nights'][df['minimum.nights'] < 0]=NA
df['minimum.nights'][df['minimum.nights'] >31]=NA

table(df$availability.365)
df['availability.365'][df['availability.365'] < 0]=NA
df['availability.365'][df['availability.365'] >366]=NA

table(df$reviews.per.month)
df['reviews.per.month'][df['reviews.per.month'] >31]=NA


table(df$neighbourhood.group)
df['neighbourhood.group'][df['neighbourhood.group']== 'manhatan']='Manhattan'
df['neighbourhood.group'][df['neighbourhood.group']== '']=NA

table(df$neighbourhood)[0:5]
df['neighbourhood'][df['neighbourhood']== '']=NA

table(df$cancellation_policy)
df['cancellation_policy'][df['cancellation_policy']== '']=NA

table(df$host_identity_verified)
df['host_identity_verified'][df['host_identity_verified']== '']=NA

table(df$instant_bookable)
df['instant_bookable'][df['instant_bookable']== '']=NA

summary(df)
sum(colSums(is.na(df))>0)
names(which(colSums(is.na(df))>0))

na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))/n_row*100
na_count
#Replacing Nans
#Construction.year Nan values
summary(df$Construction.year)
hist(df$Construction.year, breaks = 7)
df$Construction.year[is.na(df$Construction.year)]<-median(df$Construction.year,na.rm=TRUE)

# Price Nan values
summary(df$price)
hist(df$price)
df$price[is.na(df$price)]<-median(df$price,na.rm=TRUE)

# service fee Nan values
summary(df$service.fee)
hist(df$service.fee)
df$service.fee[is.na(df$service.fee)]<-median(df$service.fee,na.rm=TRUE)
summary(df$service.fee)

# minimum nights Nan values

summary(df$minimum.nights)
hist(df$minimum.nights,breaks=15)
table(df$minimum.nights)[0:5]
df$minimum.nights[is.na(df$minimum.nights)]<-tail(names(sort(table(df$minimum.nights))), 1)
summary(df$minimum.nights)

# minimum nights Nan values
summary(df$number.of.reviews)
hist(df$number.of.reviews)
table(df$number.of.reviews)[0:5]
tail(names(sort(table(df$number.of.reviews))), 1)
df$number.of.reviews[is.na(df$number.of.reviews)]<-0
summary(df$number.of.reviews)

# reviews rate number Nan values
summary(df$review.rate.number)
hist(df$review.rate.number)
df$review.rate.number[is.na(df$review.rate.number)]<-median(df$review.rate.number,na.rm=TRUE)
summary(df$review.rate.number)

# reviews per month Nan values
summary(df$reviews.per.month)
hist(df$reviews.per.month)
tail(names(sort(table(df$reviews.per.month))), 1)
df$reviews.per.month[is.na(df$reviews.per.month)]<-median(df$reviews.per.month,na.rm=TRUE)
summary(df$reviews.per.month)

# calculated host listings count Nan values
summary(df$calculated.host.listings.count)
hist(df$calculated.host.listings.count)
tail(names(sort(table(df$calculated.host.listings.count))), 1)
df$calculated.host.listings.count[is.na(df$calculated.host.listings.count)]<-median(df$calculated.host.listings.count,
                                                                                    na.rm=TRUE)
summary(df$calculated.host.listings.count)

# availability 365 Nan values
summary(df$availability.365)
hist(df$availability.365)
df$availability.365[is.na(df$availability.365)]<-as.integer(tail(names(sort(table(df$availability.365))), 1))
summary(df$availability.365)

# host_identity_verified Nan values
tail(names(sort(table(df$host_identity_verified))), 1)
df$host_identity_verified[is.na(df$host_identity_verified)]<-tail(names(sort(table(df$host_identity_verified))), 1)

# neighbourhood.group Nan values
tail(names(sort(table(df$neighbourhood.group))), 1)
df$neighbourhood.group[is.na(df$neighbourhood.group)]<-tail(names(sort(table(df$neighbourhood.group))), 1)

# neighbourhood Nan values
tail(names(sort(table(df$neighbourhood))), 1)
df$neighbourhood[is.na(df$neighbourhood)]<-tail(names(sort(table(df$neighbourhood))), 1)

# cancellation_policy Nan values
tail(names(sort(table(df$cancellation_policy))), 1)
df$cancellation_policy[is.na(df$cancellation_policy)]<-tail(names(sort(table(df$cancellation_policy))), 1)

# instant_bookable Nan values
tail(names(sort(table(df$instant_bookable))), 1)
df$instant_bookable[is.na(df$instant_bookable)]<-tail(names(sort(table(df$instant_bookable))), 1)

################################################################################
#D)
hist(df$lat)
hist(df$long)
hist(df$Construction.year)
hist(df$price)
hist(df$service.fee)
hist(as.numeric(df$minimum.nights))
hist(df$number.of.reviews)
hist(df$reviews.per.month)
hist(df$review.rate.number)
hist(df$calculated.host.listings.count)
hist(df$availability.365)
################################################################################
################################################################################
################################################################################
#Question 1) 
################################################################################
#A) hisogram
#bin size with Freedman-Diaconis
bw <- 2 * IQR(df$lat) / length(df$lat)^(1/3)
bw<-0.05
ggplot(df, aes(x=lat)) + geom_histogram(aes(y=..density..),binwidth =bw, color="black", fill="#ffff84") +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= c(mean(df$lat),median(df$lat)),size=1,linetype ="dashed",color=c('blue','red'))+ labs(title ="Latitude distribution ")+
  xlab("Latitude") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
################################################################################
#B) mean and median and skewness
summary(df$lat)

################################################################################
#C) Boxplot
PC_plot<-boxplot(df$lat)
#printing the whisker lower reach, Q1,Q2,Q3, and the whisker upper reach
PC_plot$stats
IQR(df$lat)
quantile(df$lat)
################################################################################
#D) Outliers
PC_plot$out
length(PC_plot$out)
#install.packages("sf")
#install.packages("mapview")
library(tidyverse)
library(sf)
library(mapview)
mapview(df, xcol = "long", ycol = "lat", zcol='neighbourhood.group', crs = 4269, grid = FALSE, legend = TRUE)
################################################################################
#E) Statical data
mean(df$lat)
median(df$lat)
var(df$lat)
sd(df$lat)

################################################################################
#F) Pie Chart
Low_lat_values<-df$lat[df$lat<0.5*mean(df$lat)]
low_upper_lat_values<-df$lat[df$lat>0.5*mean(df$lat) & df$lat<mean(df$lat)]
high_lower_lat_values<-df$lat[df$lat>1.5*mean(df$lat) & df$lat>mean(df$lat)]
High_lat_values<-df$lat[df$lat>1.5*mean(df$lat)]

#Possiblity 1 
dat <- df['lat']
dat <- within(dat, {place <- NA 
place[dat$lat<=0.5*mean(dat$lat)] <- "Low"
place[dat$lat>0.5*mean(dat$lat) & dat$lat<=mean(dat$lat)] <- "Low middle"
place[dat$lat<=1.5*mean(dat$lat) & dat$lat>mean(dat$lat)] <- "High middle"
place [dat$lat>1.5*mean(dat$lat)]<- "High"} )
dat$place <- factor(dat$place)
pie( table(dat$place), labels = paste0(round(100 * table(dat$place)/sum(table(dat$place)), 2), "%")
     , main = "Color pie chart", col=rainbow(length(levels(dat$place))))
legend("topright", levels(dat$place), cex = 0.8,fill = rainbow(length(levels(dat$place))))

#Possiblity 2
dat=cut(df$lat,breaks=4)
pie(table(dat),labels = paste0(round(100 *table(dat)/sum(table(dat)), 2), "%"),col=rainbow(length(levels(dat))))
legend("topright", levels(dat), cex = 0.8,fill = rainbow(length(levels(dat))))
################################################################################
#E) Density 
ggplot(df, aes(x=lat)) + 
geom_density()+geom_vline(aes(xintercept= mean(df$lat),
                            linetype = 'Mean'), colour = 'red') +
  
geom_vline(aes(xintercept = median(df$lat), 
                 linetype = 'Median'), colour = 'blue') +
scale_linetype_manual(name = 'lines',
                      values = c('Mean' = 1,
                                 'Median' = 1),
                      guide = guide_legend(override.aes = list(colour = c('red',
                                                                          'blue'))))
################################################################################
################################################################################

#A) hisogram
#bin size with Freedman-Diaconis
bw <- 2 * IQR(df$price) / length(df$price)^(1/3)
ggplot(df, aes(x=price)) + geom_histogram(aes(y=..density..), color="black", fill="#ffff84") +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= c(mean(df$price),median(df$price)),size=1,linetype ="dashed",color=c('blue','red'))+ labs(title ="price distribution ")+
  xlab("price") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
################################################################################
#B) mean and median and skewness
summary(df$price)

################################################################################
#C) Boxplot
PC_plot<-boxplot(df$price)
#printing the whisker lower reach, Q1,Q2,Q3, and the whisker upper reach
PC_plot$stats
IQR(df$price)
quantile(df$price)
################################################################################
#D) Outliers
PC_plot$out
length(PC_plot$out)
################################################################################
#E) Statical data
mean(df$price)
median(df$price)
var(df$price)
sd(df$price)

################################################################################
#F) Pie Chart
Low_lat_values<-df$price[df$price<0.5*mean(df$price)]
low_upper_lat_values<-df$price[df$price>0.5*mean(df$price) & df$price<mean(df$price)]
high_lower_lat_values<-df$price[df$price>1.5*mean(df$price) & df$price>mean(df$price)]
High_lat_values<-df$price[df$price>1.5*mean(df$price)]

#Possiblity 1 
dat <- df['price']
dat <- within(dat, {place <- NA 
place[dat$price<=0.5*mean(dat$price)] <- "Low"
place[dat$price>0.5*mean(dat$price) & dat$price<=mean(dat$price)] <- "Low middle"
place[dat$price<=1.5*mean(dat$price) & dat$price>mean(dat$price)] <- "High middle"
place [dat$price>1.5*mean(dat$price)]<- "High"} )
dat$place <- factor(dat$place)
pie( table(dat$place), labels = paste0(round(100 * table(dat$place)/sum(table(dat$place)), 2), "%")
     , main = "Color pie chart", col=rainbow(length(levels(dat$place))))
legend("topright", levels(dat$place), cex = 0.8,fill = rainbow(length(levels(dat$place))))

#Possiblity 2
dat=cut(df$price,breaks=4)
pie(table(dat),labels = paste0(round(100 *table(dat)/sum(table(dat)), 2), "%"),col=rainbow(length(levels(dat))))
legend("topright", levels(dat), cex = 0.8,fill = rainbow(length(levels(dat))))
################################################################################
#E) Density 
ggplot(df, aes(x=price)) + 
  geom_density()+geom_vline(aes(xintercept= mean(price),
                                linetype = 'Mean'), colour = 'red') +
  
  geom_vline(aes(xintercept = median(price), 
                 linetype = 'Median'), colour = 'blue') +
  scale_linetype_manual(name = 'lines',
                        values = c('Mean' = 1,
                                   'Median' = 1),
                        guide = guide_legend(override.aes = list(colour = c('red',
                                                                            'blue'))))
################################################################################
#Question 2)
################################################################################
#A)frequency table
table(df$neighbourhood.group)
################################################################################

#B)
ggplot(df, aes(x=reorder(neighbourhood.group,neighbourhood.group, function(x)-length(x)),fill=neighbourhood.group)) +
  geom_bar()+ labs(title ="Neighbourhood group frequencies ")+
  xlab("Count") + ylab("Neighbourhood group")+scale_fill_manual(values = c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51"), guide = "none") +
  theme(panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_blank(),
        panel.background = element_rect(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

################################################################################
#C)
meds <- table(df$neighbourhood.group)/sum(table(df$neighbourhood.group)) * 100
bp<-barplot(meds, ylim=c(0,100),
            main="Barplot of neighbourhood group",
            xlab="neighbourhood group",
            ylab="Frequency of neighbourhood group",)
text(bp, 0, paste(round(meds, 2), "%", sep=""), cex=1, pos=3)

################################################################################
#D)

ggplot(df, aes(neighbourhood.group, reviews.per.month,, fill=neighbourhood.group))+
  labs(title="Violin plot for neighbourhood group and reviews per month",
            x="neighbourhood group",
            y="reviews per month")+ geom_violin()+ theme_classic()

ggplot(df, aes(neighbourhood.group, price,, fill=neighbourhood.group))+
  labs(title="Violin plot for neighbourhood group and price",
       x="neighbourhood group",
       y="price")+ geom_violin()+ theme_classic()
################################################################################
################################################################################
################################################################################
#Question 3)
################################################################################
#A)
    plot(df$service.fee,df$price,
         main="Price/Service fee",
         xlab="Service fee",
         ylab="Price")


plot(df$reviews.per.month,df$price,
     main="Price/reviews.per.month",
     xlab="reviews.per.month",
     ylab="Price")
################################################################################
#B)
shape=as.factor(df$neighbourhood.group)
ggplot(df, aes(x = price, y = service.fee,shape = shape,color =shape)) +
  geom_point() + # Points and color by group
  xlab("price") +              # X-axis label
  ylab("service.fee")  +             # Y-axis label
  theme(axis.line = element_line(colour = "black", # Changes the default theme
                                 size = 0.24))+ 
labs(title ="Price/Service fee ",color='neighbourhood.group',shape='neighbourhood.group')+
  theme(plot.title = element_text(hjust = 0.5))


################################################################################
#C)
val<-cor.test(df$price,df$service.fee)
val
################################################################################
#D) 

#install.packages("ggExtra")
#install.packages("hexbin")
#library(hexbin)
library(ggExtra)

hexbin <- ggplot(df, aes(x = price, y=service.fee))+
  geom_point()+
  geom_hex(bins=60 )+ 
  geom_smooth(method = "gam", color = "red", formula = y ~ s(x, bs = "cs"))+ 
  labs(x = "Price", y= "service.fee")+
  ggtitle("Hexbin plot")+
  scale_fill_continuous(type = "viridis")

ggMarginal(hexbin,
           type = 'histogram',
           margins = 'both',
           size = 5,
           colour = 'black',
           fill = '#7B68EE')
################################################################################
#E) 
ggplot(df, aes(x=price, y=service.fee) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle("2D density plot for price and service fee")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

################################################################################
################################################################################
################################################################################
#Question 4)
################################################################################
#A)
#install.packages("corrplot")
#install.packages("ggcorrplot")       # Install ggcorrplot package
library(corrplot)
#library(ggcorrplot) 
airbnb_cor <- df[, sapply(df, is.numeric)]
airbnb_cor <- airbnb_cor[complete.cases(airbnb_cor), ]
correlation_matrix <- cor(airbnb_cor)

p.mat <- cor_pmat(correlation_matrix)
corrplot(correlation_matrix, p.mat = p.mat,method = "color", insig = "p-value",
         col= colorRampPalette(c("blue","white", "red"))(20))


ggcorrplot(correlation_matrix, method = "circle", 
           colors = c("blue", "white", "red"), 
           lab = TRUE, p.mat=p.mat,)
################################################################################
#B)
#for all the variables
dev.new(width = 1000, height = 1000, unit = "px")
plot(df,pch='.')
#only for numericals
################################################################################
#C)
#install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
library(plotly)
col_names<-c('reviews.per.month','number.of.reviews','price')
df$neighbourhood.group = as.factor(df$neighbourhood.group)
shapes = c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51")
shapes <- shapes[as.numeric(df$neighbourhood.group)]
scatterplot3d(df[col_names],color=shapes)

plot_ly(x=df$reviews.per.month, y=df$number.of.reviews, z=df$price, type="scatter3d",
        mode="markers", color=df$neighbourhood.group)%>%
  layout(scene = list(xaxis = list(title = "reviews per month"), 
                      yaxis = list(title = "number of reviews"),
                      zaxis = list(title = "price")))
################################################################################
################################################################################
################################################################################
#Question 5)
################################################################################
#A)
addmargins(table(df$neighbourhood.group,df$review.rate.number))


################################################################################
#B) 
ggplot(df, aes(fill=neighbourhood.group, x=review.rate.number)) + 
  geom_bar(position=position_dodge(), stat="count")+ 
  geom_text(stat='count', aes(label=..count..),position=position_dodge(0.9),vjust=-0.4,size=3)+
  labs(title ="neighbourhood.group/review.rate.number fee ")+
  theme(plot.title = element_text(hjust = 0.5))

  ################################################################################
#C) 
ggplot(df, aes(fill=neighbourhood.group, x=review.rate.number)) + 
  geom_bar(position="stack", stat="count")+ 
  geom_text(stat='count', aes(label=..count..),position = position_stack(vjust = 0.5))+
  labs(title ="neighbourhood.group/review.rate.number fee ")+
  theme(plot.title = element_text(hjust = 0.5))

################################################################################
#D) 
v <- aggregate(df$neighbourhood.group~df$review.rate.number + neighbourhood.group, data = df, FUN = length)
colnames(v) <- c('review.rate.number','neighbourhood.group','counts')
v<-transform(v, rel1 = round(ave(counts, review.rate.number, FUN = prop.table),digit=4))
v<-transform(v, grpsize = aggregate(v$counts, by=list(review.rate.number =v$review.rate.number ), FUN=sum))
colnames(v) <- c('review.rate.number','neighbourhood.group','counts','rel1','test','grpSize')
names=c('lv hypertrophy'="",'normal'="",'st-t abnormality'="")
graphics.off()
ggplot(v, aes(x=review.rate.number,y=rel1,fill=neighbourhood.group,width = grpSize)) +
  geom_bar(stat='identity') +
  scale_x_discrete(expand = c(0, 0)) +
  facet_grid(~review.rate.number, scales = "free", space = "free",labeller = as_labeller(names))+
  geom_text(aes(label = paste(round(100*rel1, 2), "%", sep="")),size=3.34,position = position_stack(vjust = 0.5)) +
  labs(title ="Mosaic plot of review.rate.number and neighbourhood.group ")+
  xlab("review.rate.number") + ylab("Proportion")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 1))  
################################################################################
################################################################################
################################################################################
#Question 6) 
#install.packages("moments")
#library(moments)

#skewness(log(df$lat))

Our_sample <- df[sample(nrow(df), 25, replace = FALSE), ]
################################################################################
#B)
#First_probabilty

data_no_outlier<-df


ggplot(data_no_outlier, aes(x=lat)) + geom_histogram(aes(y=..density..), color="black", fill="#ffff84") +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= c(mean(data_no_outlier$lat),median(data_no_outlier$lat)),size=1,linetype ="dashed",color=c('blue','red'))+ labs(title ="Latitude distribution ")+
  xlab("Latitude") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data_no_outlier, aes(x=long)) + geom_histogram(aes(y=..density..), color="black", fill="#ffff84") +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= c(mean(data_no_outlier$long),median(data_no_outlier$long)),size=1,linetype ="dashed",color=c('blue','red'))+ labs(title ="Latitude distribution ")+
  xlab("Longitude") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
x<-data_no_outlier$lat
y<-data_no_outlier$long
t.test(x,y, mu =0, conf.lev=.95)

#second_probabilty
Bronx<-df[df$neighbourhood.group=='Bronx',]
StatIsland<-df[df$neighbourhood.group=='Staten Island',]
Bronx <- Bronx[sample(nrow(Bronx), 25, replace = FALSE), ]
StatIsland <- StatIsland[sample(nrow(StatIsland), 25, replace = FALSE), ]

x<-Bronx$lat
y<-StatIsland$lat
t.test(x,y, mu =0, conf.lev=.95)
################################################################################
################################################################################
################################################################################
#Question 7)
data_no_outlier<-df
IQR <- IQR(data_no_outlier$lat)
quartiles <- quantile(data_no_outlier$lat, probs=c(.25, .75), na.rm = FALSE)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data_no_outlier, data_no_outlier$lat > Lower & data_no_outlier$lat < Upper)
boxplot(data_no_outlier$lat)

ggplot(data_no_outlier, aes(x=lat)) + geom_histogram(aes(y=..density..),binwidth =0.05, color="black", fill="#ffff84") +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= c(mean(df$lat),median(df$lat)),size=1,linetype ="dashed",color=c('blue','red'))+ labs(title ="Latitude distribution ")+
  xlab("Latitude") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

#A)
Our_sample<-data_no_outlier[sample(nrow(df),100, replace = TRUE),]
Our_sample<-Our_sample$lat
sample_mean<-mean(Our_sample)
sample_sigma<-sd(Our_sample)
Me<-sample_sigma/sqrt(length(Our_sample))
upper_limit<-sample_mean+qnorm((1-0.98)/2)*Me
lower_limit<-sample_mean-qnorm((1-0.98)/2)*Me
################################################################################
#C)
ggplot(data_no_outlier, aes(x=lat)) + geom_histogram( color="black", fill="#ffff84") +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= c(mean(data_no_outlier$lat),lower_limit,upper_limit),size=1,linetype ="dashed",color=c('blue','red','red'))+ labs(title ="Latitude distribution ")+
  xlab("Latitude") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
################################################################################
#D)
z<-abs(sample_mean-40.7)/(sample_sigma/sqrt(length(Our_sample)))
x<-pnorm(z,lower.tail = FALSE)
x
################################################################################
#F)
z<-abs(qnorm(0.01))-x
pnorm(z,lower.tail = FALSE)


################################################################################
################################################################################
################################################################################
#Question 8)
#A)
boxplot(df$number.of.reviews)

ggplot(df, aes(x=number.of.reviews)) + geom_histogram( color="black", fill="#ffff84") +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= c(mean(df$number.of.reviews),quantile(df$number.of.reviews,0.025),quantile(df$number.of.reviews,0.975)),size=1,linetype ="dashed",color=c('blue','red','red'))+ labs(title ="number.of.reviews distribution ")+
  xlab("number.of.reviews") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

################################################################################
#B)

Our_sample<-df[sample(nrow(df),20, replace = FALSE),]
x<-Our_sample$number.of.reviews

boot_means <- rep(NA, 500)
for (i in 1:500){
  boot_sample <- sample(x, 20, replace = TRUE)
  boot_means[i] <- mean(boot_sample)
}
boots<-data.frame(boot_means)
ggplot(boots,aes(x=boot_means)) + geom_dotplot( binwidth = 2,dotsize = 0.05) 


Our_sample<-boot_means
sample_mean<-mean(Our_sample)
sample_sigma<-sd(Our_sample)
Me<-sample_sigma/sqrt(length(Our_sample))
upper_limit<-sample_mean+qnorm((1-0.95)/2)*Me
lower_limit<-sample_mean-qnorm((1-0.95)/2)*Me

bootdf<- data.frame(Our_sample)

ggplot(boots, aes(x=boot_means)) + geom_dotplot( binwidth = 2,dotsize = 0.05) +
  geom_vline(xintercept= c(lower_limit,upper_limit),size=1,linetype ="dashed",color=c('red','red'))+ labs(title ="Latitude distribution ")+
  xlab("number.of.reviews") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                      panel.background = element_blank(), axis.line = element_line(colour = "black"))
################################################################################
#C)


ggplot(boots, aes(x=boot_means)) + geom_dotplot( binwidth = 2,dotsize = 0.3) +
  geom_vline(xintercept= c(lower_limit,upper_limit,quantile(df$number.of.reviews,0.025),
                           quantile(df$number.of.reviews,0.975))
             ,size=1,linetype ="dashed",color=c('red','red','blue','blue'))+ 
  labs(title ="Latitude distribution ")+
  xlab("number.of.reviews") +
  ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))
################################################################################
#B)

################################################################################
################################################################################
################################################################################
#Question 9)       
#latitude
#A)
df$neighbourhood.group <- factor(df$neighbourhood.group)
result <- aov( lat~neighbourhood.group, data = data_no_outlier)
summary(result)
boxplot(data_no_outlier$lat~data_no_outlier$neighbourhood.group)
#B)
pairwise.t.test(data_no_outlier$lat, data_no_outlier$neighbourhood.group, p.adj = "none", pool.sd = FALSE)
pairwise.t.test(data_no_outlier$lat, data_no_outlier$neighbourhood.group, p.adj = "bonf", pool.sd = FALSE)

#Price
df$neighbourhood.group <- factor(df$neighbourhood.group)
result <- aov( price~neighbourhood.group, data = df)
summary(result)
boxplot(df$price~df$neighbourhood.group)
#B)
pairwise.t.test(df$price, df$neighbourhood.group, p.adj = "none", pool.sd = FALSE)
pairwise.t.test(df$price, df$neighbourhood.group, p.adj = "bonf", pool.sd = FALSE)
###############################################################################
