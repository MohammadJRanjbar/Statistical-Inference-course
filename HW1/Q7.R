


#a) Loading data for the classes and showing important attributes for the day class
###########################################################################################

dtest_score <- c(99,56,78,55.5,32,90,80,81,56,59,45,77,84.5,84,70,72,68,32,79,90)
summary(dtest_score)
#Evening class
ntest_score <- c(98,78,68,83,81,89,88,76,65,45,98,90,80,84.5,85,79,78,98,90,79,81,25.5)
summary(ntest_score)
###########################################################################################
#b) Outliers
###########################################################################################
# Get IQR for the day class
IQR <- IQR(dtest_score)
# Calculating max upper and min lower whisker reach
Tmin <- quantile(dtest_score, 0.25) -(1.5*IQR)
Tmax <- quantile(dtest_score, 0.75)+(1.5*IQR)
# Calculating the outliers 
dtest_score[which(dtest_score < max(Tmin,min(dtest_score)) | dtest_score > min(Tmax,max(dtest_score)))]

#For the evening class
eIQR <- IQR(ntest_score)
TNmin <- quantile(ntest_score, 0.25) -(1.5*eIQR)
TNmax <- quantile(ntest_score, 0.75)+(1.5*eIQR)
ntest_score[which(ntest_score < max(TNmin,min(ntest_score)) | ntest_score > min(TNmax,max(ntest_score)))]
###########################################################################################

#c)
###########################################################################################
boxplot(ntest_score, dtest_score,names = c("Evening class","Day class"),horizontal = TRUE)
###########################################################################################

