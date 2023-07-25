library(ggplot2)
#loading data
df <- read.csv(file = 'Heart.csv')

#############################################################################################################################
#a)
#binwdith based on 5 year age gap
ggplot(df, aes(x=age)) + geom_histogram(aes(y=..density..),binwidth=5, color="black", fill="#ffff84") +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= quantile(df$age,c(0.975,0.025)),size=1,linetype = "dashed")+ labs(title ="Age distribution ")+
  xlab("Age") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  

#############################################################################################################################
#a) bins are choosen based on Freedman–Diaconis 
#bandwidth is based on 
bw <- 2 * IQR(df$age) / length(df$age)^(1/3)
ggplot(df, aes(x=age)) + geom_histogram(aes(y=..density..), color="black", fill="#ffff84",binwidth =bw) +
  geom_density(aes(y=stat(density)),alpha = .2, fill= "yellow", color = '#760002',size=1)+
  geom_vline(xintercept= quantile(df$age,c(0.975,0.025)),size=1,linetype = "dashed")+ labs(title ="Age distribution ")+
  xlab("Age") + ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black"))  
#############################################################################################################################
#b)
qplot(sample = thalch,data=df, colour =sex,shape=sex)+
  stat_qq_line()+
  scale_shape_manual(values=c(10,4))+ labs(title ="Thalch distribution ",colour='Gender',shape='Gender')+
  xlab("Quantile") + ylab("Thalch")+  scale_color_manual(values=c("#d25010", "#208d64"))+ 
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

############################################################################################################################                      
#c)
ggplot(df, aes(x=reorder(origin,origin, function(x)+length(x)),fill=origin)) +
  geom_bar()+coord_flip()+ labs(title ="Place of study frequencies ")+
  xlab("Count") + ylab("Country")+scale_fill_manual(values = c("#ffa500","#00e5ee","#ff6eb4","#43cd80"), guide = "none") +
  theme(panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

#############################################################################################################################
#d)
ggplot(df, aes(x=trestbps, y=cp,fill=cp)) + 
  geom_boxplot()+ coord_flip()+ labs(title ="Chest pain and Resting blood pressure box plot ")+
  xlab("Resting blood pressure") + ylab("Chest pain")+scale_fill_manual(values = c("#ff6eb4","#e066ff","#cd9b1d","#00e5ee")
                                                                        , guide = "none")+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

#############################################################################################################################
#e)
#removing data that does not have value for restecg and exang
df2 <- with(df, df[!(restecg == "" | is.na(restecg)), ])
df2 <- with(df2, df2[!(exang == "" | is.na(exang)), ])
v <- aggregate(df2$exang~df2$restecg + exang, data = df2, FUN = length)
colnames(v) <- c('restecg','exang','counts')
v<-transform(v, rel1 = round(ave(counts, restecg, FUN = prop.table),digit=4))
v<-transform(v, grpsize = aggregate(v$counts, by=list(restecg =v$restecg ), FUN=sum))
colnames(v) <- c('restecg','exang','counts','rel1','test','grpSize')
names=c('lv hypertrophy'="",'normal'="",'st-t abnormality'="")
graphics.off()
ggplot(v, aes(x=restecg,y=rel1,fill=exang,width = grpSize)) +
  geom_bar(stat='identity') +
  scale_x_discrete(expand = c(0, 0)) +
  facet_grid(~restecg, scales = "free", space = "free",labeller = as_labeller(names))+
  geom_text(aes(label = paste(round(100*rel1, 2), "%", sep="")),size=3.34,position = position_stack(vjust = 0.5)) +
  labs(title ="Mosaic plot of restecg and exang ")+
  xlab("Restecg") + ylab("Proportion")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          strip.text = element_text(size = 1))

