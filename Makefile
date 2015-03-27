all:
	make compile
	make edgelist
	make distribution
	make adjacencylist
	make hitsscore
	make cellfeature
	make coherence
	make cellcoherence
	make jetplot

compile:
	javac CatchSync.java

edgelist:
	java -Xmx160g CatchSync graph_to_edgelist data socialgraph edgelist nodemap 

distribution:
	java -Xmx160g CatchSync edgelist_to_dc data edgelist nodemap xindoutd outd2c ind2c 

adjacencylist:
	java -Xmx160g CatchSync edgelist_to_adjacencylist data edgelist adjacencylist_u2vs adjacencylist_v2us 

hitsscore:
	java -Xmx160g CatchSync hits_degree_adjacency data adjacencylist_u2vs adjacencylist_v2us nodemap xindoutd xu1outd xv1ind 10 

cellfeature:
	java -Xmx160g CatchSync scatter_to_cell_N data xu1outd xcu1coutdN100 cellu1outdN100 log 100 
	java -Xmx160g CatchSync scatter_to_cell_N data xv1ind xcv1cindN100 cellv1indN100 log 100 
	java -Xmx160g CatchSync scatter_to_cell data xu1outd xcu1coutdP2 cellu1outdP2 2 
	java -Xmx160g CatchSync scatter_to_cell data xv1ind xcv1cindP2 cellv1indP2 2 

coherence:
	java -Xmx160g CatchSync normalcy_coherence data nodemap adjacencylist_u2vs xcv1cindP2 cellv1indP2 unorcohoutdP2 
	java -Xmx160g CatchSync normalcy_coherence data nodemap adjacencylist_v2us xcu1coutdP2 cellu1outdP2 vnorcohindP2 

cellcoherence:
	java -Xmx160g CatchSync scatter_to_cell_N data unorcohoutdP2 xcunorcucohD20 cellunorcohD20 lin 100 20 
	java -Xmx160g CatchSync scatter_to_cell_N data vnorcohindP2 xcvnorcvcohD20 cellvnorcohD20 lin 100 20 

jetplot:
	R --no-save < CatchSync.R jet data/cellv1indN100 plot/followee_feature_space.png authoritativeness in-degree log 100 
	R --no-save < CatchSync.R jet data/cellunorcohD20 plot/follower_synchronicity_normality.png normality synchronicity log 100 

