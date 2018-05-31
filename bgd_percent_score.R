




reality<-c(0:100)
target<-c(0:100)
seq(0,100,5) %>% length
# columns: target
# rows: reality
reality<-matrix(seq(0,100,5),21,21)
reality<-reality[nrow(reality):1,]
target<-matrix(seq(0,100,5),21,21,byrow = T)

colnames(score)<-seq(0,100,5)
rownames(score)<-rev(seq(0,100,5))
score<-(reality-target)/target
revscore<-1-score

(revscore<1) %>% raster %>% plot(legend=F,col=c("black","grey"),asp=1,xlab="target",ylab="observed",main="negative score?")
abline(0,1,0,1,col="red")

(abs(revscore)<0.00001) %>% raster %>% plot
sink("revscore.txt")
revscore %>% round(2) %>%  kable
sink()
raster(revscore) %>% plot











score %>% round(3) -> score
score[nrow(score):1,] -> score
score

max(score %>% hasdata,na.rm = T)


hist(score)


plot(raster(score))



score %>% write.csv("score.csv")
