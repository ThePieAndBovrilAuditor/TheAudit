options(stringsAsFactors = FALSE)
options(max.print = 10000)

library(rvest)
library(dplyr)
library(stringr)
library(xml2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

customcol1<-rgb(255/255,140/255,51/255,alpha=0.7)
customcol2<-rgb(51/255,51/255,51/255,alpha=0.1)
customcol3<-rgb(255/255,0/255,0/255,alpha=0.3)


timezonecorrector<-function(x,tz){
  if((x-tz)<0){
  x<-(24+x)-(tz)
  }else{
    x<-x-tz
  }
  return(x)
}

pber<-"a_n_other"
PostingLink = "https://www.pieandbovril.com/forum/index.php?/profile/1-a_n_other/content/"
PostingPageCount = 100


times<-data.frame("time"=numeric(0))
forums<-data.frame("forum"=numeric(0))

for(i in seq(from = 1, to = PostingPageCount, by=1)){
  url <- paste0(PostingLink,"page/",i,"/&type=forums_topic_post")
  h <- read_html(url)
  timepage <-html_nodes(h,"time")
  times1<-data.frame("time"=substr(timepage,17,35))
  editpage <- h %>% html_nodes("strong") %>% html_nodes("time")
  edits1<-data.frame("time"=substr(editpage,17,35))
  ifelse(length(edits1$time==0),
         times1<-data.frame("time"=times1[-c(1,2),]), # removes 'joined', 'last visited'
         times1<-data.frame("time"=times1[-c(1,2,which(times1$time==edits1$time)),])) # removes 'joined', 'last visited' and any edits.
  times<-rbind(times,times1)
  datapage<- h %>% html_nodes("p") %>% html_nodes("a")
  for (line in 1:length(datapage)){
    entry<-as.character(datapage[line])
    forums1<-data.frame("forum"=entry)
    forums<-rbind(forums,forums1)
  }
}

times2<-strsplit(as.character(times$time), "T")  
forums2<-as.list(forums)

PostingTimes<-data.frame(
  "date"=rep(NA,length(times2)),
  "time"=rep(NA,length(times2))
)

for (i in 1:length(PostingTimes[,1])){
  PostingTimes$date[i]<-times2[[i]][1]
  PostingTimes$time[i]<-times2[[i]][2]
}

PostingTimes$year<-substr(PostingTimes$date,1,4)

PostingTimes$date<-as.Date(PostingTimes$date,format="%Y-%m-%d")
PostingTimes$hour<-as.integer(substr(PostingTimes$time,1,2))

#daylight saving correction
for (i in 1:length(PostingTimes[,1])){
  month<-as.integer(format(PostingTimes$date[i],"%m"))
  if(month >3 & month <10){
    if((PostingTimes$hour[i]+1)>23){
      PostingTimes$hour[i]<-(PostingTimes$hour[i]+1)-24
    }else{
      PostingTimes$hour[i]<-PostingTimes$hour[i]+1
    }
  }
}


# if need a timezone correction
# I had both east and west of GMT versions but overwrote them so this is just
# a west of GMT version, need to revise sometime
# for(i in 1:length(PostingTimes$hour)){
#   PostingTimes$hour[i]<-timezonecorrector(PostingTimes$hour[i],6)
# }


PostingTimes$minute<-PostingTimes$hour*60+as.integer(substr(PostingTimes$time,4,5))

forums3<-forums2$forum[grep("https://www.pieandbovril.com/forum/index.php[\\?]/forum/",forums2$forum)]

forums4<-rep(NA,length(forums3))
for (i in 1:length(forums4)){
  re1<-regexpr(">(.*?)<",forums3[i])
  startchar<-re1[1]+1
  stopchar<-re1[1] + attributes(regexpr(">(.*?)<",forums3[i]))$match.length-2
  forums4[i]<-substr(forums3[i],startchar,stopchar)
}

forums5<-data.frame("forum"=forums4)

forums6<-aggregate(forums5,by=list(forums5$forum),length)
names(forums6)<-c("forum","count")
forums6<-forums6[order(forums6$count),]

png(paste0("pb posts/",pber,"Posts.png"),height=15,width=20,res=100,units="cm")
par(mar=c(4,10,0,0),oma=c(2,2,2,2))
barplot(height=forums6$count,names.arg=forums6$forum,horiz=TRUE,las=2,cex.names = 0.6,xlim=c(0,max(forums6$count)+100),col=customcol1)
box()
dev.off()

minyear<-format(min(PostingTimes$date),"%Y")
maxyear<-format(max(PostingTimes$date),"%Y")

mindate<-as.Date(paste(minyear,"01","01",sep="-"),format="%Y-%m-%d")
maxdate<-as.Date(paste(maxyear,"12","31",sep="-"),format="%Y-%m-%d")

png(paste0("pb posts/",pber,"PostingTimes.png"),res=100,units="cm",height=15,width=20)
plot(PostingTimes$minute~PostingTimes$date,col=customcol2,pch=16,ylab="Hour of Day",xlab="Date",axes=FALSE)
axis.Date(1,PostingTimes$date,at=seq(mindate,maxdate,by="years"),cex.axis=0.8)
axis(2,las=2,cex.axis=0.75,at=seq(0,1440,60),labels=seq(0,24,1))
box()
dev.off()

#average posts per year
PostCount<-aggregate(PostingTimes$time,by=list(PostingTimes$year),length)
PostMean<-mean(PostCount$x)

#most common posting time
PostTime<-aggregate(PostingTimes$hour,by=list(PostingTimes$year),getmode)


