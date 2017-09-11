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

#The vertices in the social network are indexed by igraph as 2:456627.     
#80895 is the number of rows of W2.
valid_vertices<-2:456627
total_number_of_valid_vertices<-456626
vector_of_No_of_times_friends_retweet_for_each_vertex <- rep(0,total_number_of_valid_vertices)   #number of times friends retweet before the vertex retweet
                                                                                                 #or the total times of friends' retweet if the vertex doesn't retweet.
vector_of_whether_the_vertex_retweet<-rep(0,total_number_of_valid_vertices)

amount_calculated<-0      #update in the following for loop.
for (i in valid_vertices)   
{ 
  amount_calculated<-amount_calculated+1
  friend_list<-neighbors(n4,i,mode='in')
  retweet<-0
  n<-0
  
  #whether the vertex has retweeted.
  if (!(nrow(W2[W2[,1]==(i-1),])==0))
  {
    retweet<-1
  }
  
  if (retweet==0)   #calculate the total number of times the vertex's friends retweet if the vertex doesn't retweet at all.
  {
   n<-nrow(W2[(W2[,1]+1) %in% friend_list,])
    }
      
  if (!(retweet==0))  #the number of times friends retweet before the vertex's first retweet.
   {
    n<-nrow(W2[((W2[,1]+1) %in% friend_list) & (W2[,3]<W2[W2[,1]==(i-1),][1,3]),])
    }
  
  vector_of_No_of_times_friends_retweet_for_each_vertex[amount_calculated]<-n
  vector_of_whether_the_vertex_retweet[amount_calculated]<-retweet
}

different_No_of_times_friends_retweet<-sort(unique(vector_of_No_of_times_friends_retweet_for_each_vertex),decreasing=TRUE)

#calculate how many vertices have certain amount of friends' retweet.
no_of_vertices_for_different_No_of_times_of_friends_retweet<-rep(0,length(different_No_of_times_friends_retweet))

#calculate how many vertices with certain amount of friends' retweet also retweet.
total_no_of_vertex_retweet_for_this_amount_of_friend_retweet<-rep(0,length(different_No_of_times_friends_retweet))

#find which No_of_friends'_retweet sub-group the vertex should belong to.
for (j in 1:length(different_No_of_times_friends_retweet))
   {
    qualified_vertices<-valid_vertices[vector_of_No_of_times_friends_retweet_for_each_vertex[valid_vertices-1]==different_No_of_times_friends_retweet[j]]
    no_of_vertices_for_different_No_of_times_of_friends_retweet[j]<-length(qualified_vertices)
    total_no_of_vertex_retweet_for_this_amount_of_friend_retweet[j]<-sum(vector_of_whether_the_vertex_retweet[qualified_vertices-1])
    }

threshold=300   #calculate the probability of retweet only for the sub-groups with number of vertices equal or larger than 300.
number_of_groups_considered<-length(no_of_vertices_for_different_No_of_times_of_friends_retweet[no_of_vertices_for_different_No_of_times_of_friends_retweet>=threshold])

groups_with_different_number_of_friends_retweet_that_are_considered<-rep(0,number_of_groups_considered)
probability_of_retweet_for_different_amount_of_friend_retweet<-rep(0,number_of_groups_considered)
index_in_groups<-0
for(i in 1:length(different_No_of_times_friends_retweet))
{
  if (no_of_vertices_for_different_No_of_times_of_friends_retweet[i]>=threshold)
  {
    index_in_groups<-index_in_groups+1
    groups_with_different_number_of_friends_retweet_that_are_considered[index_in_groups]<-different_No_of_times_friends_retweet[i]
    probability_of_retweet_for_different_amount_of_friend_retweet[index_in_groups]<-total_no_of_vertex_retweet_for_this_amount_of_friend_retweet[i]/no_of_vertices_for_different_No_of_times_of_friends_retweet[i]
  }
}

#plot the relationship between the amount of friends' retweet before the vertex retweets and a vertex's probability of retweeting.
plot(groups_with_different_number_of_friends_retweet_that_are_considered,probability_of_retweet_for_different_amount_of_friend_retweet,type='o')