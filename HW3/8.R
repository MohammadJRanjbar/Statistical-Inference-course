df <- read.csv(file = 'Galton.csv')
########################################################################################
#a)
student_mean<-mean(df$child)
intervals_with_mean<-0
n_samples<-60
CI<-0.97
n_intervals<-20000
for (x in 1:n_intervals)
{
  temp_samples<-sample(df$child,n_samples)
  temp_mean<-mean(temp_samples)
  temp_sd<-sd(temp_samples)/sqrt(n_samples)
  high_tresh<-temp_mean+abs(qnorm((1-CI)/2))*temp_sd
  low_tresh<-temp_mean-abs(qnorm((1-CI)/2))*temp_sd
  if(student_mean<high_tresh & student_mean>low_tresh)
    {
    intervals_with_mean<-intervals_with_mean+1
    }
}
print(intervals_with_mean/n_intervals)
########################################################################################
#b)
student_mean<-mean(df$child)
intervals_with_mean<-0
n_samples<-10
CI<-0.9
n_intervals<-10000
degree_f<-n_samples-1
for (x in 1:n_intervals)
{
  temp_samples<-sample(df$child,n_samples)
  temp_mean<-mean(temp_samples)
  temp_sd<-sd(temp_samples)/sqrt(n_samples)
  high_tresh<-temp_mean+abs(qt((1-CI)/2,degree_f))*temp_sd
  low_tresh<-temp_mean-abs(qt((1-CI)/2,degree_f))*temp_sd
  if(student_mean<high_tresh & student_mean>low_tresh)
  {
    intervals_with_mean<-intervals_with_mean+1
  }
}
print(intervals_with_mean/n_intervals)
########################################################################################
#c)
parent_mean<-mean(df$parent)
n_samples<-70
alpha<-0.05
H0<-60
CI<-1-alpha
temp_samples<-sample(df$parent,n_samples)
temp_mean<-mean(temp_samples)
temp_sd<-sd(temp_samples)/sqrt(n_samples)
high_tresh<-temp_mean+abs(qnorm((1-CI)/2))*temp_sd
low_tresh<-temp_mean-abs(qnorm((1-CI)/2))*temp_sd
if(H0<high_tresh & H0>low_tresh)
{
  cat("the real mean with the value of: ", parent_mean," is between", low_tresh,
      " and ",high_tresh, ". Therefore, The H0 is not rejcted")
} else
{
  cat("the real mean with the value of: ", parent_mean,
        " is between", low_tresh, " and ",high_tresh, ". Therefore, The H0 is rejcted")
}
cat("the power is equal to:", 1-pnorm(qnorm(alpha,0,1,lower.tail =TRUE) - (parent_mean-H0)/temp_sd))

########################################################################################
#d)

parent_mean<-mean(df$parent)
n_samples<-10
alpha<-0.05
CI<-1-alpha
degree_f<-n_samples-1
H0<-60
temp_samples<-sample(df$parent,n_samples)
temp_mean<-mean(temp_samples)
temp_sd<-sd(temp_samples)/sqrt(n_samples)
p_value<-2*pt((temp_mean-H0)/temp_sd,degree_f)
if(p_value>alpha)
{
  cat("the real mean with the value of: ", parent_mean," is between", low_tresh, " and ",high_tresh, ". Therefore, The H0 is not rejcted")
} else
{
  cat("the real mean with the value of: ", parent_mean,
      " is between", low_tresh, " and ",high_tresh, ". Therefore, The H0 is rejcted")
}
cat("the power is equal to:", 1-pt(qt(alpha,df=degree_f,lower.tail =TRUE) - (parent_mean-H0)/temp_sd,df=degree_f))

