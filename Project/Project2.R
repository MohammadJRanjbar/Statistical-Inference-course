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


###############################################################################
###############################################################################
###############################################################################
###############################################################################

#Question 1
#-1
df$neighbourhood.group=as.factor(df$review.rate.number)
levels(df$review.rate.number)

df$instant_bookable=as.factor(df$instant_bookable)
levels(df$instant_bookable)

set.seed(4220)
sub_df<-df[sample(nrow(df), 500), ]

tb<-addmargins(table(sub_df$review.rate.number,sub_df$instant_bookable))
tb
p<-c(1:5)
for (i in 1:5)
{
  p[i]<- tb[i,2]/tb[i,3]
}
Ci1.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[2]*(1-p[2])/tb[2,3])
Ci1.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[2]*(1-p[2])/tb[2,3])

Ci2.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[3]*(1-p[3])/tb[3,3])
Ci2.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[3]*(1-p[3])/tb[3,3])

Ci3.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[4]*(1-p[4])/tb[4,3])
Ci3.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[4]*(1-p[4])/tb[4,3])

Ci4.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[5]*(1-p[5])/tb[5,3])
Ci4.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[5]*(1-p[5])/tb[5,3])

Ci5.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[3]*(1-p[3])/tb[3,3])
Ci5.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[3]*(1-p[3])/tb[3,3])

Ci6.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[4]*(1-p[4])/tb[4,3])
Ci6.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[4]*(1-p[4])/tb[4,3])

Ci7.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[5]*(1-p[5])/tb[5,3])
Ci7.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[5]*(1-p[5])/tb[5,3])


Ci8.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[4]*(1-p[4])/tb[4,3])
Ci8.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[4]*(1-p[4])/tb[4,3])

Ci9.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[5]*(1-p[5])/tb[5,3])
Ci9.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[5]*(1-p[5])/tb[5,3])

Ci10.up<-p[1]-p[2]+qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[5]*(1-p[5])/tb[5,3])
Ci10.low<-p[1]-p[2]-qnorm(0.025)*sqrt(p[1]*(1-p[1])/tb[1,3]+p[5]*(1-p[5])/tb[5,3])
###############################################################################
#b) 
chisq.test(table(df$neighbourhood.group,df$instant_bookable))


tbl <- table(df$neighbourhood.group, df$instant_bookable)
tbl2<-addmargins(table(df$neighbourhood.group, df$instant_bookable))
tbl3<-tbl2
#calcuating expcted values
for (i in 1:5)
{
  for (j in 1:3)
  {
    tbl3[i,j]<-tbl2[i,3]*tbl2[5,j]/tbl2[5,3]
    
  }
}
tbl3
X<-0
#calculating X2
for (i in 1:5)
{
  for (j in 1:3)
  {
    X=X+ ((tbl2[i,j]-tbl3[i,j])^2)/tbl3[i,j]
  }
}
X
degree_f<-(4-1)*(3-1)
#cacluating the p-value
pchisq(X,degree_f,lower.tail = FALSE)
#########################################################
#second method
#with using the function
chisq.test(tbl)
###############################################################################
###############################################################################
###############################################################################
###############################################################################

#Question 2
library(mosaic)
library(data.table)
table(df$host_identity_verified)
set.seed(120)
SP<-sample(df$host_identity_verified,15)

pi <- 0.5   # probability of success for each toss
n <- as.numeric(table(SP)[1]+table(SP)[2])   # Number of times we toss the penny (sample size)
trials <- 1000   # Number of trials (number of samples)

observed <- table(SP)[2]  # Observed number of heads 

phat = observed / n   # p-hat - the observed proportion of heads

data.sim <- do(1000) * rflip(n, prob = pi )

histogram(~prop, data = data.sim, 
          v = phat, 
          width = 0.025,
          xlab = "Proportion", 
          main = "Null hypothesis distribution",
          groups = prop >= phat) 

if(observed > pi * n) {
  pvalue <- sum(data.sim$prop >= phat) / trials
} else {
  pvalue <- sum(data.sim$prop <= phat) / trials
}

paste("One-sided p-value is", pvalue)
###############################################################################
###############################################################################
###############################################################################
###############################################################################

#Question 3
table(df$cancellation_policy)
df$cancellation_policy=as.factor(df$cancellation_policy)

set.seed(5900)
sub_df<-df[sample(nrow(df), 100), ]
random_sample<-sub_df$cancellation_policy
table(random_sample)
biased_sample <- sample(levels(df$cancellation_policy),100,prob = c(0.6,0.1,0.3),replace = T)
table(biased_sample)


####
#random sample
random_sample<-as.numeric(table(random_sample))
Percent_population<-as.numeric(table(df$cancellation_policy))/sum(as.numeric(table(df$cancellation_policy)))
#calculating the expcted values
expctec<-round(Percent_population*100)
expctec[2]<-expctec[2]+1
Percent_population<-expctec/100
expctec
#calcualting the X2
X<-0
epoch<-length(random_sample)
for (i in 1:epoch)
{
  X<- X+((random_sample[i]-expctec[i])^2)/expctec[i]
}
X
degree_f<-length(random_sample)-1
pchisq(X,degree_f,lower.tail = FALSE)

#second method
chisq.test(random_sample,p=Percent_population)

####
#Biased sample
biased_sample<-as.numeric(table(biased_sample))
Percent_population<-as.numeric(table(df$cancellation_policy))/sum(as.numeric(table(df$cancellation_policy)))
#calculating the expcted values
expctec<-round(Percent_population*100)
expctec[2]<-expctec[2]+1
Percent_population<-expctec/100
expctec
#calcualting the X2
X<-0
epoch<-length(biased_sample)
for (i in 1:epoch)
{
  X<- X+((biased_sample[i]-expctec[i])^2)/expctec[i]
}
X
degree_f<-length(biased_sample)-1
pchisq(X,degree_f,lower.tail = FALSE)

#second method
chisq.test(biased_sample,p=Percent_population)
################################################################################
#b) 

set.seed(5900)
sub_df <- df[sample(nrow(df), 200), ]
table(sub_df$review.rate.number)

tbl <- table(sub_df$review.rate.number,sub_df$cancellation_policy)
tbl2<-addmargins(table(sub_df$review.rate.number,sub_df$cancellation_policy))
tbl2
tbl3<-tbl2
#calcuating expcted values
for (i in 1:6)
{
  for (j in 1:4)
  {
    tbl3[i,j]<-tbl2[i,4]*tbl2[6,j]/tbl2[6,4]
    
  }
}
tbl3
X<-0
#calculating X2
for (i in 1:6)
{
  for (j in 1:4)
  {
    X=X+ ((tbl2[i,j]-tbl3[i,j])^2)/tbl3[i,j]
  }
}
X
degree_f<-(5-1)*(3-1)

#cacluating the p-value
pchisq(X,degree_f,lower.tail = FALSE)

#second method
chisq.test(tbl)
###############################################################################
###############################################################################
###############################################################################
###############################################################################

#Question 4
#A)

ggplot(df, aes(x=reviews.per.month, y=number.of.reviews)) + 
  geom_point(color='#2980B9')


ggplot(df, aes(x=long, y=number.of.reviews)) + 
  geom_point(color='#2980B9') 


###############################################################################
#B.a.1)
library(ggplot2)

ggplot(df, aes(x=reviews.per.month, y=number.of.reviews)) + 
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, color='#2C3E50')

my_model_rev <- lm( number.of.reviews~reviews.per.month, data = df)
summary(my_model_rev)

#normal resduials
plot(my_model_rev, 2)

#constant variablity
plot(my_model_rev, 1)
plot(my_model_rev, 3)


my_model_rev$coefficients[1]

my_model_rev$coefficients[2]

plot(df$reviews.per.month,df$number.of.reviews)
abline(my_model_rev, lty = 2,col="red")

####################################


#without the outliers
set.seed(4000)
sub_df <- df[df$number.of.reviews<200,]
sub_df <- sub_df[sub_df$reviews.per.month<10,]
ggplot(sub_df, aes(x=reviews.per.month, y=number.of.reviews)) + 
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, color='#2C3E50')


my_model_rev <- lm( number.of.reviews~reviews.per.month, data = sub_df)
summary(my_model_rev)

#normal resduials
plot(my_model_rev, 2)

#constant variablity
plot(my_model_rev, 1)
plot(my_model_rev, 3)

my_model_rev$coefficients[1]

my_model_rev$coefficients[2]

plot(df$reviews.per.month,df$number.of.reviews)
abline(my_model_rev, lty = 2,col="red")
###############################################################################
#B.a.2)
ggplot(df, aes(x=long, y=number.of.reviews)) + 
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, color='#2C3E50')

my_model_long <- lm( number.of.reviews~long, data = df)
summary(my_model_long)

#normal resduials
plot(my_model_long, 2)

#constant variablity
plot(my_model_long, 1)
plot(my_model_long, 3)

my_model_long$coefficients[1]

my_model_long$coefficients[2]

plot(df$long,df$number.of.reviews)
abline(my_model_long, lty = 2,col="red")

#############
#B.b.1)
my_model_rev <- lm( number.of.reviews~reviews.per.month, data = df)
summary(my_model_rev)

ggplot(df, aes(x=reviews.per.month, y=number.of.reviews)) + 
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, color='#2C3E50')

#############
#B.b.2)
my_model_long <- lm( number.of.reviews~long, data = df)
ggplot(df, aes(x=long, y=number.of.reviews)) + 
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, color='#2C3E50')

#############
#B.c.1)

my_model_rev$coefficients[1]

my_model_rev$coefficients[2]

#############
#B.c.2)


summary(my_model_long)

my_model_long$coefficients[1]

my_model_long$coefficients[2]
###############################################################################
#B.d.1)

ggplot(df, aes(x=reviews.per.month, y=number.of.reviews)) + 
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, color='#2C3E50',linetype="dashed")

###############################################################################
#B.d.2)

ggplot(df, aes(x=long, y=number.of.reviews)) + 
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, color='#2C3E50',linetype="dashed")

###############################################################################
#D)
summary(my_model_rev)
summary(my_model_long)

  
###############################################################################
#F.a)

set.seed(5900)
sub_df <- df[sample(nrow(df), 100), ]
train<-sub_df[sample(nrow(sub_df), 90), ]
test<-sub_df[-sample(nrow(sub_df), 90), ]

my_model <- lm(number.of.reviews ~ reviews.per.month , data = train)
summary(my_model)

my_model2 <- lm(number.of.reviews ~ long , data = train)
summary(my_model2)
##################
#F.b)
confint(my_model)
confint(my_model2)
##################
#F.c)
predict(my_model,test)
predict(my_model2,test)
##################
#F.c)
data.frame(RMSE= RMSE(predict(my_model,test), test$number.of.reviews),
           R2= R2(predict(my_model,test), test$number.of.reviews),
           MAE= MAE(predict(my_model,test), test$number.of.reviews))

data.frame(RMSE= RMSE(predict(my_model2,test), test$number.of.reviews),
           R2= R2(predict(my_model2,test), test$number.of.reviews),
           MAE= MAE(predict(my_model2,test), test$number.of.reviews))
###############################################################################
###############################################################################
###############################################################################
###############################################################################
#Question 5

#A)

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor)
}
my_cols <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51")
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[df$neighbourhood.group])
}
# Customize upper panel

# Create the plots
pairs(df[,c('number.of.reviews','reviews.per.month','price','lat','long','service.fee','availability.365','calculated.host.listings.count')], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


#df <- read.csv(file = 'AB_NYC_2019.csv')
#pairs(df[,c('number_of_reviews','reviews_per_month','price','latitude','longitude','availability_365','calculated_host_listings_count')], 
      #lower.panel = panel.cor)

my_model <- lm(number.of.reviews ~ reviews.per.month+availability.365 , data = df)
summary(my_model)
###############################################################################
#c)
library("plot3D")
#install.packages("plot3D")
x <- df$reviews.per.month
y <- df$availability.365
z <- df$number.of.reviews

# Compute the linear regression 
fit <- lm(z ~ x + y)

# create a grid from the x and y values (min to max) and predict values for every point
# this will become the regression plane
grid.lines = 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# create the fitted points for droplines to the surface
fitpoints <- predict(fit)

# scatter plot with regression plane
scatter3D(x, y, z, pch = 19, cex = 1,colvar = NULL, col="red", 
          theta = 20, phi = 40, bty="b",
          xlab = "reviews.per.month", ylab = "reviews.per.month", zlab = "number.of.reviews",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = TRUE, fit = fitpoints,
                      col=ramp.col (col = c("dodgerblue3","seagreen2"),
                                    n = 300, alpha=0.9), border="black"), main = "Regression")

###############################################################################
#d)
library(tidyverse)
set.seed(123) 
sub_df<-df[,c('number.of.reviews','reviews.per.month')]
train <- sub_df[sample(nrow(sub_df),floor(nrow(sub_df)*0.8)),]
test <- sub_df[-sample(nrow(sub_df),floor(nrow(sub_df)*0.8)),]

my_model <- lm(number.of.reviews ~., data = train )
summary(my_model)


predictions <- my_model %>% predict(train)

data.frame(RMSE= RMSE(predictions, train$number.of.reviews),
R2= R2(predictions, train$number.of.reviews),
MAE= MAE(predictions, train$number.of.reviews))

predictions <- my_model %>% predict(test)

data.frame(RMSE= RMSE(predictions, test$number.of.reviews),
           R2= R2(predictions, test$number.of.reviews),
           MAE= MAE(predictions, test$number.of.reviews))
###############################################################################
#E)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
model <- train(number.of.reviews ~., data = train, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

###############################################################################
#f)
#normal resduials

plot(my_model, 2)

#constant variablity
plot(my_model, 1)
plot(my_model, 3)


my_model$coefficients[1]

my_model$coefficients[2]


###############################################################################
#g)
summary(fit)
print(model)
###############################################################################
###############################################################################
###############################################################################
###############################################################################
#Question 6
#a)
df$instant_bookable<-as.factor(df$instant_bookable)
df$neighbourhood.group<-as.factor(df$neighbourhood.group)
df$cancellation_policy<-as.factor(df$cancellation_policy)
df$room.type<-as.factor(df$room.type)
my_model <- glm(instant_bookable ~ price +
                +room.type+cancellation_policy+host_identity_verified
                +neighbourhood.group, data = df,family ="binomial" )
summary(my_model)
exp(coef(my_model))
#install.packages("pROC")
###############################################################################
#b)
library(pROC)
library(caret)
#install.packages("caret")
set.seed(364)
sub_df<-df[,c('instant_bookable','cancellation_policy', 'price'
              ,'room.type',"neighbourhood.group","host_identity_verified")]
train <- sub_df[sample(nrow(sub_df),floor(nrow(sub_df)*0.8)),]
test <- sub_df[-sample(nrow(sub_df),floor(nrow(sub_df)*0.8)),]

my_model <- glm(instant_bookable ~., data = train,family ="binomial" )
summary(my_model)

pred_resp <- predict(my_model,type="response")

table(train$instant_bookable, (pred_resp > 0.5)*1, dnn=c("Truth","Predicted"))

pred_glm0_train<- predict(my_model, type="response")

library(ROCR)
#install.packages('ROCR')
pred <- prediction(pred_glm0_train, train$instant_bookable)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))


pred_glm0_test<- predict(my_model, newdata = test, type="response")
pred <- prediction(pred_glm0_test, test$instant_bookable)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

###############################################################################
#c)

hist(predict(my_model),type="response")
#install.packages('sjPlot')
library(sjPlot)
plot_model(my_model)

###############################################################################
#D)
exp(confint(my_model))


###############################################################################
###############################################################################
###############################################################################
###############################################################################
#Question 7
#a)

###############################################################################
#b)
plot_model(my_model)
###############################################################################
#c)

my_model <- glm(instant_bookable ~ room.type, data = df,family ="binomial" )
summary(my_model)
plot_model(my_model)
###############################################################################
#d)

sub_df<-df[,c('instant_bookable','room.type')]
train <- sub_df[sample(nrow(sub_df),floor(nrow(sub_df)*0.8)),]
test <- sub_df[-sample(nrow(sub_df),floor(nrow(sub_df)*0.8)),]

my_model <- glm(instant_bookable ~., data = train,family ="binomial" )
summary(my_model)

pred_resp <- predict(my_model,type="response")

table(train$instant_bookable, (pred_resp > 0.5)*1, dnn=c("Truth","Predicted"))

pred_glm0_train<- predict(my_model, type="response")

library(ROCR)
#install.packages('ROCR')
pred <- prediction(pred_glm0_train, train$instant_bookable)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))


pred_glm0_test<- predict(my_model, newdata = test, type="response")
pred <- prediction(pred_glm0_test, test$instant_bookable)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

