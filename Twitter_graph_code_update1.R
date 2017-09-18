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
no_of_follower_for_each_vertex<-rep(0,456626)
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
  if (length(W2$V1[W2$V1==(i-1)])!=0)
  {
    retweet<-1
  }
  
  if (retweet==0)   #calculate the total number of times the vertex's friends retweet if the vertex doesn't retweet at all.
  {
    n<-length(W2$V1[(W2$V1+1) %in% friend_list])
  }
  
  if (retweet!=0)  #the number of times friends retweet before the vertex's first retweet.
  {
    n<-nrow(W2[((W2[,1]+1) %in% friend_list) & (W2[,3]<W2[W2[,1]==(i-1),][1,3]),])
  }
  
  vector_of_No_of_times_friends_retweet_for_each_vertex[amount_calculated]<-n
  vector_of_whether_the_vertex_retweet[amount_calculated]<-retweet
  no_of_follower_for_each_vertex[amount_calculated]<-length(neighbors(n4,i,mode='out'))
  }

different_No_of_times_friends_retweet<-sort(unique(vector_of_No_of_times_friends_retweet_for_each_vertex),decreasing=TRUE)
different_No_of_follower<-sort(unique(no_of_follower_for_each_vertex),decreasing=TRUE)

#choose approximately 25% vertices with highest number of followers and 25% vertices with lowest number of followers.
vertices_reordered<-valid_vertices[order(no_of_follower_for_each_vertex)]
vertices_with_more_follower<-vertices_reordered[342471:456626]   #group1
vertices_with_less_follower<-vertices_reordered[1:114156]    #group2

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

#prepare for the plots for the two groups.
different_No_of_times_friends_retweet_group1<-sort(unique(vector_of_No_of_times_friends_retweet_for_each_vertex[vertices_with_more_follower-1]),decreasing=TRUE)
different_No_of_times_friends_retweet_group2<-sort(unique(vector_of_No_of_times_friends_retweet_for_each_vertex[vertices_with_less_follower-1]),decreasing=TRUE)

no_of_vertices_for_different_No_of_times_of_friends_retweet_group1<-rep(0,length(different_No_of_times_friends_retweet_group1))
no_of_vertices_for_different_No_of_times_of_friends_retweet_group2<-rep(0,length(different_No_of_times_friends_retweet_group2))

total_no_of_vertex_retweet_for_this_amount_of_f_r_group1<-rep(0,length(different_No_of_times_friends_retweet_group1))
total_no_of_vertex_retweet_for_this_amount_of_f_r_group2<-rep(0,length(different_No_of_times_friends_retweet_group2))

#group1
#find which No_of_friends' retweet sub-group the vertex should belong to.
for (j in 1:length(different_No_of_times_friends_retweet_group1))
{
  qualified_vertices_group1<-vertices_with_more_follower[vector_of_No_of_times_friends_retweet_for_each_vertex[vertices_with_more_follower-1]==different_No_of_times_friends_retweet_group1[j]]
  no_of_vertices_for_different_No_of_times_of_friends_retweet_group1[j]<-length(qualified_vertices_group1)
  total_no_of_vertex_retweet_for_this_amount_of_f_r_group1[j]<-sum(vector_of_whether_the_vertex_retweet[qualified_vertices_group1-1])
}

threshold=300   #calculate the probability of retweet only for the sub-groups with number of vertices equal or larger than 300.
number_of_groups_considered1<-length(no_of_vertices_for_different_No_of_times_of_friends_retweet_group1[no_of_vertices_for_different_No_of_times_of_friends_retweet_group1>=threshold])

groups_with_different_number_of_friends_retweet_that_are_considered1<-rep(0,number_of_groups_considered1)
probability_of_retweet_for_different_amount_of_friend_retweet1<-rep(0,number_of_groups_considered1)
index_in_groups<-0
for(i in 1:length(different_No_of_times_friends_retweet_group1))
{
  if (no_of_vertices_for_different_No_of_times_of_friends_retweet_group1[i]>=threshold)
  {
    index_in_groups<-index_in_groups+1
    groups_with_different_number_of_friends_retweet_that_are_considered1[index_in_groups]<-different_No_of_times_friends_retweet_group1[i]
    probability_of_retweet_for_different_amount_of_friend_retweet1[index_in_groups]<-total_no_of_vertex_retweet_for_this_amount_of_f_r_group1[i]/no_of_vertices_for_different_No_of_times_of_friends_retweet_group1[i]
  }
}

#group2
#find which No_of_friends' retweet sub-group the vertex should belong to.
for (j in 1:length(different_No_of_times_friends_retweet_group2))
{
  qualified_vertices_group2<-vertices_with_less_follower[vector_of_No_of_times_friends_retweet_for_each_vertex[vertices_with_less_follower-1]==different_No_of_times_friends_retweet_group2[j]]
  no_of_vertices_for_different_No_of_times_of_friends_retweet_group2[j]<-length(qualified_vertices_group2)
  total_no_of_vertex_retweet_for_this_amount_of_f_r_group2[j]<-sum(vector_of_whether_the_vertex_retweet[qualified_vertices_group2-1])
}

threshold=300   #calculate the probability of retweet only for the sub-groups with number of vertices equal or larger than 300.
number_of_groups_considered2<-length(no_of_vertices_for_different_No_of_times_of_friends_retweet_group2[no_of_vertices_for_different_No_of_times_of_friends_retweet_group2>=threshold])

groups_with_different_number_of_friends_retweet_that_are_considered2<-rep(0,number_of_groups_considered2)
probability_of_retweet_for_different_amount_of_friend_retweet2<-rep(0,number_of_groups_considered2)
index_in_groups<-0
for(i in 1:length(different_No_of_times_friends_retweet_group2))
{
  if (no_of_vertices_for_different_No_of_times_of_friends_retweet_group2[i]>=threshold)
  {
    index_in_groups<-index_in_groups+1
    groups_with_different_number_of_friends_retweet_that_are_considered2[index_in_groups]<-different_No_of_times_friends_retweet_group2[i]
    probability_of_retweet_for_different_amount_of_friend_retweet2[index_in_groups]<-total_no_of_vertex_retweet_for_this_amount_of_f_r_group2[i]/no_of_vertices_for_different_No_of_times_of_friends_retweet_group2[i]
  }
}

par(mfrow=c(2,2))
#plot the relationship between the amount of friends' retweet before the vertex retweets and a vertex's probability of retweeting.
plot(groups_with_different_number_of_friends_retweet_that_are_considered,probability_of_retweet_for_different_amount_of_friend_retweet,type='o')
#plot for group of people with more follower
plot(groups_with_different_number_of_friends_retweet_that_are_considered1,probability_of_retweet_for_different_amount_of_friend_retweet1,type='o')
#plot for group of people with less follower
plot(groups_with_different_number_of_friends_retweet_that_are_considered2,probability_of_retweet_for_different_amount_of_friend_retweet2,type='o')

