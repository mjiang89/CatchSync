Edited and Copyright by Meng Jiang
Last update: Oct 29, 2014
	1st: Feb 22, 2014
	2nd: Sep 27, 2014
	3rd: Oct  8, 2014
	4th: Oct 20, 2014

CatchSync: Catch Synchronized Behavior in Large Directed Graphs (KDD 2014 Best Paper Final List)
Authors: Meng Jiang, Peng Cui, Alex Beutel, Christos Faloutsos and Shiqiang Yang

Given a directed graph with millions of nodes, how can we automatically spot anomalies?
Suspicious graph patterns show up in many applications,
from Twitter users who buy fake followers,
manipulating the social network,
to botnet members performing distributed denial of service attacks,
disturbing the network traffic graph.
 
Input: node pairs (edge list) of a directed graph;
Output:
  1. Feature space plots: in-degree vs. authority for target nodes
                          (out-degree vs. hubness for source nodes);
  2. Synchronicity (coherence) vs. normality: for source nodes.

Installation:
	Python >= 2.6
	R >= 3.0.1

Data: see ./data/socialgraph
	A directed graph (sample data). Two columns {node1,node2}: node1 follows (connects to) node2.
	Injected users: 'badXXXX'; normal users: 'userXXXX'.

Please run "make" in command line.

Result:
	see ./plot/*.png
		followee_feature_space.png: A feature space created by followees' features (in-degree and authority).
		follower_synchronicity_normality.png: Synchronicity vs. Normality for followers. Followers with too
			high synchronicity are suspicious.

----- CITING THE WORK -----

If your work uses or refers to CatchSync, please cite the papers using the following bibtex entries:

@inproceedings{jiang2014catchsync,
  title={CatchSync: catching synchronized behavior in large directed graphs},
  author={Jiang, Meng and Cui, Peng and Beutel, Alex and Faloutsos, Christos and Yang, Shiqiang},
  booktitle={Proceedings of the 20th ACM SIGKDD international conference on Knowledge discovery and data mining},
  pages={941--950},
  year={2014},
  organization={ACM}
}

This work is supported by
	1) National Natural Science Foundation of China Project No. 61370022, No. 61210008 and No. 61303075;
	2) International Science and Technology Cooperation Program of China, No. 2013DFG12870;
	3) National Program on Key Basic Research Project, No. 2011CB302206
that are conducted by
	Tsinghua University (Department of Computer Science and Technology)
	– Tencent Technology (Shenzhen) Co., Ltd. Joint Laboratory
	for Internet Innovation Technology.

If you use CatchSync for research or commercial purposes, please let us know
your institution(company) and whether it's ok to mention it among the users of CatchSync.

----- READ ME (Makefile) -----

compile:
	Complile java code CatchSync.java => CatchSync.class

edgelist: [graph_to_edgelist]
	Mapping a directed graph into a edge list with nodes starting from 0,1...; and a nodemap from old id to new id
	Input: "data" (directory name), socialgraph
	Output: edgelist, nodemap

distribution: [edgelist_to_dc]
	Calculating data distributions: node's degrees, out-degree and in-degree distributions
	Input: "data" (directory name), edgelist, nodemap
	Output: xindoutd, outd2c, ind2c

adjacencylist: [edgelist_to_adjacencylist]
	Changing edge list into two adjacency lists (source node to a list of targets, target node to a list of sources)
	Input: "data" (directory name), edgelist
	Output: adjacencylist_u2vs, adjacencylist_v2us

hitsscore: [hits_degree_adjacency]
	Computing HITS score (U1 value = hubness, V1 value = authority) in 10 iterations
	Input: "data" (directory name), adjacencylist_u2vs, adjacencylist_v2us, nodemap, xindoutd, 10
	Output: xu1outd, xv1ind

cellfeature: [scatter_to_cell] or [scatter_to_cell_N]
	Mapping scatter plots into cell plots: 1. by log(a), a=2; 2. N*N, 100*100
	Input: "data" (directory name), xu1outd/xv1ind/..., "log 100"/"2"
	Output: xcu1coutdN100, cellu1outdN100

coherence: [normalcy_coherence]
	Computing synchronicity (coherence) and normality (normalcy).
	Input: "data" (directory name), adjacencylist_u2vs, xcv1cindP2, cellv1indP2
	Output: unorcohoutdP2

cellcoherence:
	Mapping scatter plots into cell plots: linear 100*100 deg>=20
	Input: "data" (directory name), unorcohoutdP2, lin 100 20
	Output: xcunorcucohD20, cellunorcohD20
	
jetplot:
	Plot the feature space and synchronicity-normality.
	Input: cellv1indN100/cellunorcohD20
	Output: followee_feature_space.png/follower_synchronicity_normality.png
 
