library(igraph)

#n1<-read_graph('higgs-mention_network.edgelist')
#n2<-read_graph('higgs-reply_network.edgelist')
#n3<-read_graph('higgs-retweet_network.edgelist')
n4<-read_graph('higgs-social_network.edgelist')
#class(n4)

start_time=as.numeric(as.POSIXct('2012/07/04 08:00:00',tz='GMT'))    #1341388800
end_time=as.numeric(as.POSIXct('2012/07/05 08:00:00',tz='GMT'))      #1341475200
#as.numeric(as.POSIXct('2012/07/03 00:00'))            #1341298800
#as.numeric(as.POSIXct('2012/07/04 00:00'))            #1341385200
#as.numeric(as.POSIXct('2012/07/01 00:00'))            #1341126000
#as.numeric(as.POSIXct('2012/07/02 00:00'))            #1341212400
W=read.table('higgs-activity_time.txt',col.names=c('V1','V2','time','action'))
#print(W[1,1])

W1=W[(W$action=='RT')&(W$time>=start_time),]    
W2=W1[(W1$action=='RT')&(W1$time<=end_time),]   
activity_after_start=W[(W$time>=start_time),] 
activity_after_start2=activity_after_start[(activity_after_start$time<=end_time),] 
mention_record=activity_after_start2[(activity_after_start2$action=='MT'),]
data_index<-rep(0,456626*24)
indegrees<-rep(0,456626*24)
b<-rep(0,456626*24)
s<-rep(0,24*456626)
fr<-rep(0,24*456626)
fm<-rep(0,24*456626)
for (i in 1:456626)
{
  data_index[((i-1)*24+1):(24*i)]<-i+1
  indegrees[((i-1)*24+1):(24*i)]<-length(neighbors(n4,i+1,mode='in'))
  b[((i-1)*24+1):(24*i)]<-length(neighbors(n4,i+1,mode='out'))
}

for (i in 1:456626)
{ 
  friends_of_that_person<-neighbors(n4,i+1,mode='in')
  WV<-W2[W2$V1==i,]  #the_record_of_the_vertex_retweet
  WF<-W2[(W2$V1+1) %in% friends_of_that_person,]  #the_record_of_friend_retweet
  WM<-mention_record[(mention_record$V2+1) %in% friends_of_that_person,]  #the record of friends mentioned
  for (j in 1:24)
  {
    if (length(WV$time[WV$time<=(start_time+j*3600)])!=0)
    {
      s[((i-1)*24+j):((i-1)*24+24)]<-1
      break
    }
  }
  for (j in 1:24)
  {
    fr[(i-1)*24+j]<-length(WF$time[WF$time<=(start_time+3600*j)])
    fm[(i-1)*24+j]<-length(WM$time[WM$time<=(start_time+3600*j)])
  }
}

record_data<-data.frame(vertex=data_index,indegree=indegrees,states=s,times_of_friends_retweet=fr,no_of_friends_mention=fm,centralities=b)
