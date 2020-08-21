options(stringsAsFactors = FALSE)
options(max.print = 10000)

library(rvest)
library(dplyr)
library(stringr)
library(xml2)

login<-"https://www.pieandbovril.com/forum/index.php?/login/"
pgsession<-html_session(login)
pgform<-html_form(pgsession)[[3]]
auth=auth #username/email
password = password #your actual password
filled_form<-set_values(pgform, auth=auth, password = password)
submit_form(pgsession, filled_form)

dpber<-"a_n_other" # P&B member
DottingLink = "https://www.pieandbovril.com/forum/index.php?/profile/1-a_n_other/reputation/"
DottingPageCount = 100 # should really work out a scrape for this but it's from the 'posts' link on the reputation page


DottingTimes<-data.frame() 
dots<-data.frame()
imgs<-data.frame()

for(i in seq(from = 1, to = DottingPageCount, by=1)){
  url <- paste0(DottingLink,"&type=forums_topic_post&page=",i) # need to sign in to see it.
  page<-jump_to(pgsession,url)
  timepage <-html_nodes(page,"time")
  DottingTimes1<-data.frame("time"=substr(timepage,17,35))
  DottingTimes1<-data.frame("time"=DottingTimes1[-c(1,2),])# removes 'joined', 'last visited'
  DottingTimes<-rbind(DottingTimes,DottingTimes1)
  imgpage<-page %>% html_nodes ("li") %>% html_nodes("img") 
  imgpage<-imgpage[grep("reactions",imgpage)]
  for(line in 1:length(imgpage)){
    entry<-as.character(imgpage[line])
    imgs1<-data.frame("sign"=entry)
    imgs<-rbind(imgs,imgs1)
  }
  datapage<- page %>% html_nodes("li") %>% html_nodes("span")
  for (line in 1:length(datapage)){
    entry<-as.character(datapage[line])
    dots1<-data.frame("reaction"=entry)
    dots<-rbind(dots,dots1)
  }
}

DottingTimes2<-strsplit(as.character(DottingTimes$time), "T")  

DottingTimes3<-data.frame(
  "date"=rep(NA,length(DottingTimes2)),
  "time"=rep(NA,length(DottingTimes2))
)

for (i in 1:length(DottingTimes3[,1])){
  DottingTimes3$date[i]<-DottingTimes2[[i]][1]
  DottingTimes3$time[i]<-DottingTimes2[[i]][2]
}

DottingTimes3$year<-substr(DottingTimes3$date,1,4)

DottingTimes3$date<-as.Date(DottingTimes3$date,format="%Y-%m-%d")
DottingTimes3$hour<-as.integer(substr(DottingTimes3$time,1,2))

for (i in 1:length(DottingTimes3[,1])){
  month<-as.integer(format(DottingTimes3$date[i],"%m"))
  if(month >3 & month <10){
    if((DottingTimes3$hour[i]+1)>23){
      DottingTimes3$hour[i]<-(DottingTimes3$hour[i]+1)-24
    }else{
      DottingTimes3$hour[i]<-DottingTimes3$hour[i]+1
    }
  }
}

##### DOTS #####
dots2<-as.list(dots)

dots3<-dots2$reaction[grep("<strong>",dots2$reaction)]

#gregexpr(">(.*?)<",x)

dots4<-data.frame()

for (i in 1:length(dots3)){
re1<-gregexpr(">(.*?)<",dots3[i],perl=TRUE)
firstindex<-3
  startchar<-attributes(re1[[1]])$capture.start[firstindex]
  matchlength<-attributes(re1[[1]])$capture.length[firstindex]
  stopchar<-startchar + matchlength-1
  x<-substr(dots3[i], startchar, stopchar)
secondindex<-4
  startchar<-attributes(re1[[1]])$capture.start[secondindex]
  matchlength<-attributes(re1[[1]])$capture.length[secondindex]
  stopchar<-startchar + matchlength-1
  y<-substr(dots3[i], startchar, stopchar)
tempr<-cbind(x,y)
dots4<-rbind(dots4,tempr)
}

#images of up or down arrow to figure out if it was a green or red dot

likes<-grep("Like",imgs$sign)
dislikes<-grep("Dislike",imgs$sign)
imgs2<-data.frame("sign"=rep(NA,length(imgs$sign)))

imgs2$sign[likes]<-"Like"
imgs2$sign[dislikes]<-"Dislike"

CombinedDotting<-cbind(DottingTimes3,dots4,imgs2)
names(CombinedDotting)<-c("Date","Time","Year","Hour","Direction","Target","Sign")

#fans
fans<-CombinedDotting[-which(CombinedDotting$Direction=="reacted"),]
fans<-fans[which(fans$Sign=="Like"),]
fans<-aggregate(fans$Sign,by=list(fans$Target),length)
names(fans)<-c("ID","count")
fans<-fans[order(fans$count),]

#enemies
enemies<-CombinedDotting[-which(CombinedDotting$Direction=="reacted"),]
enemies<-enemies[which(enemies$Sign=="Dislike"),]
enemies<-aggregate(enemies$Sign,by=list(enemies$Target),length)
names(enemies)<-c("ID","count")
enemies<-enemies[order(enemies$count),]

#most liked
mostliked<-CombinedDotting[which(CombinedDotting$Direction=="reacted"),]
mostliked<-mostliked[which(mostliked$Sign=="Like"),]
mostliked<-aggregate(mostliked$Sign,by=list(mostliked$Target),length)
names(mostliked)<-c("ID","count")
mostliked<-mostliked[order(mostliked$count),]


#most disliked
mostdisliked<-CombinedDotting[which(CombinedDotting$Direction=="reacted"),]
mostdisliked<-mostdisliked[which(mostdisliked$Sign=="Dislike"),]
mostdisliked<-aggregate(mostdisliked$Sign,by=list(mostdisliked$Target),length)
names(mostdisliked)<-c("ID","count")
mostdisliked<-mostdisliked[order(mostdisliked$count),]

height = 12
width = 24

png(paste0("pb posts/",dpber,"Fans and Enemies.png"),res=100,units="cm",height=height,width=width,pointsize=10)
par(mfrow=c(1,2),mar=c(4,10,0,0),oma=c(1,1,1,1))
dataset<-fans
colour<-"seagreen"
firstID<-length(dataset$ID)
lastID<-length(dataset$ID)-9
barplot(height=dataset$count[lastID:firstID],names.arg=dataset$ID[lastID:firstID],horiz=TRUE,las=2,cex.names = 0.8,xlim=c(0,max(dataset$count)+20),col=colour)
box()
legend("bottomright",pch=22,pt.bg=colour,pt.cex=2,legend="Fans",bty="n")

dataset<-enemies
colour<-"red"
firstID<-length(dataset$ID)
lastID<-length(dataset$ID)-9
barplot(height=dataset$count[lastID:firstID],names.arg=dataset$ID[lastID:firstID],horiz=TRUE,las=2,cex.names = 0.8,xlim=c(0,max(dataset$count)+20),col=colour)
box()
legend("bottomright",pch=22,pt.bg=colour,pt.cex=2,legend="Enemies",bty="n")
dev.off()

png(paste0("pb posts/",dpber,"Liked and Hated.png"),res=100,units="cm",height=height,width=width,pointsize=10)
par(mfrow=c(1,2),mar=c(4,10,0,0),oma=c(1,1,1,1))
dataset<-mostliked
colour<-"steelblue"
firstID<-length(dataset$ID)
lastID<-length(dataset$ID)-9
barplot(height=dataset$count[lastID:firstID],names.arg=dataset$ID[lastID:firstID],horiz=TRUE,las=2,cex.names = 0.8,xlim=c(0,max(dataset$count)+20),col=colour)
box()
legend("bottomright",pch=22,pt.bg=colour,pt.cex=2,legend="Favourite P&Bers",bty="n")

dataset<-mostdisliked
colour<-"yellow"
firstID<-length(dataset$ID)
lastID<-length(dataset$ID)-9
barplot(height=dataset$count[lastID:firstID],names.arg=dataset$ID[lastID:firstID],horiz=TRUE,las=2,cex.names = 0.8,xlim=c(0,max(dataset$count)+20),col=colour)
box()
legend("bottomright",pch=22,pt.bg=colour,pt.cex=2,legend="Most Hated P&Bers",bty="n")
dev.off()

FanCount<-length(fans$ID)
BiggestFan<-fans[which(fans$count==max(fans$count)),]
EnemyCount<-length(enemies$count)
BiggestEnemy<-enemies[which(enemies$count==max(enemies$count)),]
LikedCount<-length(mostliked$count)
MostLikedPBer<-mostliked[which(mostliked$count==max(mostliked$count)),]
HatedCount<-length(mostdisliked$count)
MostHatedPBer<-mostdisliked[which(mostdisliked$count==max(mostdisliked$count)),]
