
#loading data
absence_number<-c(18,16,10,10,16)
X<-0
#loading expcted values
expctec<-c(14,14,14,14,14)
#calcualting the X2
for (i in 1:5)
{
  X<-X+((absence_number[i]-expctec[i])^2)/expctec[i]
}
X
pchisq(X,4,lower.tail = FALSE)

#########################################################
#second method
chisq.test(absence_number)
