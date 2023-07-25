#library
library(MASS)
data(survey)
#first methid without the function
#table
tbl <- table(survey$Smoke, survey$Exer)
tbl2<-addmargins(table(survey$Smoke,survey$Exer))
tbl3<-tbl2
#calcuating expcted values
for (i in 1:4)
  {
    for (j in 1:3)
    {
      tbl3[i,j]<-tbl2[i,4]*tbl2[5,j]/tbl2[5,4]

    }
}
tbl3
X<-0
#calculating X2
for (i in 1:4)
{
  for (j in 1:3)
  {
    X=X+ ((tbl2[i,j]-tbl3[i,j])^2)/tbl3[i,j]
  }
}
X
df<-(4-1)*(3-1)
#cacluating the p-value
pchisq(X,df,lower.tail = FALSE)
#########################################################
#second method
#with using the function
chisq.test(tbl)
