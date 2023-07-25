

#a) Printing names and type of attributes in the diamonds dataset
###########################################################################################
#load ggplot2
library(ggplot2)
names(diamonds)
sapply(diamonds,class)
###########################################################################################


#b) histogram for frequency of each clarity group
###########################################################################################
plot(diamonds$clarity, main = "Clarity distribution",xlab = "Clarity", ylab="Frequency")
###########################################################################################


#c) histogram the price distribution
###########################################################################################
hist(diamonds$price, main = "Price distribution",xlab = "Price", ylab="Frequency")
###########################################################################################



#d) histogram for price and clarity relationship
###########################################################################################
PC_plot<-boxplot(diamonds$price ~ diamonds$clarity ,main = "Price/Clarity distribution"
                 ,xlab = "Price", ylab="Clarity",horizontal = TRUE)
#printing the whisker lower reach, Q1,Q2,Q3, and the whisker upper reach
PC_plot$stats
###########################################################################################



#e) Pie chart of distribution of each color
###########################################################################################
pie( table(diamonds$color), labels = paste0(round(100 * table(diamonds$color)/sum(table(diamonds$color)), 2), "%")
     , main = "Color pie chart", col=rainbow(length(levels(diamonds$color))))

legend("topright", levels(diamonds$color), cex = 0.8,fill = rainbow(length(levels(diamonds$color))))
###########################################################################################



#f) plot for price and depth relationship
###########################################################################################
plot(diamonds$depth , diamonds$price ,main = "Depth & Price ",xlab = "Price", ylab="Depth")
###########################################################################################

