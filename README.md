CatchSync
=========

Catching Synchronized Behavior in Large Directed Graphs (KDD 2014). See www.meng-jiang.com.

    CatchSync: Catch Synchronized Behavior in Large Directed Graphs
    Authors: Meng Jiang, Peng Cui, Alex Beutel, Christos Faloutsos and Shiqiang Yang
    Date: 2014
	Version:
		0.1: Aug 2014
		0.2 (current): Oct 2014 (+readme, +licence)

Installation:
	Python 2.6 or 2.7
	R 3.0.1

Data: see ./data/socialgraph
	A directed graph (sample data). 2 columns {node1,node2}: node1 follows (connects to) node2.
	Injected users: 'badXXXX'; normal users: 'userXXXX'.

Please double-click runCatchSync.bat.

Plots: see ./plot/*.png
followee_feature_space.png: A feature space created by followees' features (in-degree and authority).
follower_synchronicity_normality.png: Synchronicity vs. Normality for followers. Followers with too
	high synchronicity are suspicious.

CITING THE WORK ----------------------------------

If your work uses or refers to CatchSync, please cite the papers using the following bibtex entries:

@inproceedings{jiang2014catchsync,
  title={CatchSync: catching synchronized behavior in large directed graphs},
  author={Jiang, Meng and Cui, Peng and Beutel, Alex and Faloutsos, Christos and Yang, Shiqiang},
  booktitle={Proceedings of the 20th ACM SIGKDD international conference on Knowledge discovery and data mining},
  pages={941--950},
  year={2014},
  organization={ACM}
}


If you use CatchSync for research or commercial purposes, please let us know
your institution(company) and whether it's ok to mention it among the users of CatchSync.
