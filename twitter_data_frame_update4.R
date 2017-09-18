library(igraph)

#n1<-read_graph('higgs-mention_network.edgelist')
#n2<-read_graph('higgs-reply_network.edgelist')
#n3<-read_graph('higgs-retweet_network.edgelist')
n4<-read_graph('higgs-social_network.edgelist')
#class(n4)

#z=1341101183
#as.Date(as.POSIXct(z,origin='1970-01-01'))
#as.numeric(as.POSIXct('2012/07/04 08:00'))            #1341414000
#as.numeric(as.POSIXct('2012/07/05 00:00'))            #1341471600
W=read.table('higgs-activity_time.txt',col.names=c('V1','V2','time','action'))
#print(W[1,1])

W1=W[(W$action=='RT')&(W$time>=1341414000),]    #retweet after official announancement.
W2=W1[(W1$action=='RT')&(W1$time<=1341471600),]  #retweet from official announcement to 00:00, 5th July. 
activity_after_official_annouancement=W[(W$time>=1341414000),] 
activity_after_official_annouancement2=activity_after_official_annouancement[(activity_after_official_annouancement$time<=1341471600),] #activity from annouancement to 00:00, 5th July.
mention_record=activity_after_official_annouancement2[(activity_after_official_annouancement2$action=='MT'),]
data_index<-rep(0,456626*16)
indegrees<-rep(0,456626*16)
b<-rep(0,456626*16)
s<-rep(0,16*456626)
fr<-rep(0,16*456626)
fm<-rep(0,16*456626)
for (i in 1:456626)
{
  data_index[((i-1)*16+1):(16*i)]<-i+1
  indegrees[((i-1)*16+1):(16*i)]<-length(neighbors(n4,i+1,mode='in'))
  b[((i-1)*16+1):(16*i)]<-length(neighbors(n4,i+1,mode='out'))
}

for (i in 1:456626)
{ 
  friends_of_that_person<-neighbors(n4,i+1,mode='in')
  WV<-W2[W2$V1==i,]  #the_record_of_the_vertex_retweet
  WF<-W2[(W2$V1+1) %in% friends_of_that_person,]  #the_record_of_friend_retweet
  WM<-mention_record[(mention_record$V2+1) %in% friends_of_that_person,]  #the record of friends mentioned
  for (j in 1:16)
  {
    if (length(WV$time[WV$time<=(1341414000+j*3600)])!=0)
    {
      s[((i-1)*16+j):((i-1)*16+16)]<-1
      break
    }
  }
  for (j in 1:16)
  {
    fr[(i-1)*16+j]<-length(WF$time[WF$time<=(1341414000+3600*j)])
    fm[(i-1)*16+j]<-length(WM$time[WM$time<=(1341414000+3600*j)])
  }
}

record_data<-data.frame(vertex=data_index,indegree=indegrees,states=s,times_of_friends_retweet=fr,no_of_friends_mention=fm,centralities=b)
