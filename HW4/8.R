df <- read.csv(file = 'Diet.csv')
###################################################################################################################
#a) 
# weights before and after
boxplot(df[,c(5,7)], boxfill = NA, border = NA) #invisible boxes - only axes and plot area
boxplot(df[df$Diet=="1", c(5,7)], xaxt = "n", add = TRUE, boxfill="red", 
        boxwex=0.25, at = 1:ncol(df[,c(5,7)]) - 0.3) #shift these left by -0.15
boxplot(df[df$Diet=="2", c(5,7)], xaxt = "n", add = TRUE, boxfill="blue", 
        boxwex=0.25, at = 1:ncol(df[,c(5,7)])  ) #shift to the right by +0.15
boxplot(df[df$Diet=="3", c(5,7)], xaxt = "n", add = TRUE, boxfill="green", 
        boxwex=0.25, at = 1:ncol(df[,c(5,7)]) + 0.3) #shift these left by -0.15
legend(x = "topleft",legend=c("Diet 1", "Diet 2", "Diet 3"), fill = c("red","blue","green"))
# Weight loss differnece in each group
df['difference']<-df$pre.weight-df$weight6weeks
boxplot(df$difference ~ df$Diet ,main = "Weight loss in each group boxplot" ,xlab = "Weight loss", ylab="Diet",horizontal = TRUE)

#Height and age in each group
boxplot(df$Height ~ df$Diet ,main = "Diet/Height boxplot" ,xlab = "Height", ylab="Diet",horizontal = TRUE)
boxplot(df$Age ~ df$Diet ,main = "Diet/Age boxplot" ,xlab = "Age", ylab="Diet",horizontal = TRUE)
###################################################################################################################

#b)
df$Diet <- factor(df$Diet)
result <- aov( difference~Diet, data = df)
summary(result)
pairwise.t.test(df$difference, df$Diet, p.adj = "none", pool.sd = FALSE)
pairwise.t.test(df$difference, df$Diet, p.adj = "bonf", pool.sd = FALSE)




