/**
* Given a directed graph with millions of nodes, how can we automatically spot anomalies?
* Suspicious graph patterns show up in many applications,
* from Twitter users who buy fake followers,
* manipulating the social network,
* to botnet members performing distributed denial of service attacks,
* disturbing the network traffic graph.
* 
* Input: node pairs (edge list) of a directed graph;
* Output:
*   1. Feature space plots: in-degree vs. authority for target nodes
*                           (out-degree vs. hubness for source nodes);
*   2. Synchronicity (coherence) vs. normality: for source nodes.
*
* Edited and Copyright by Meng Jiang
* Last update: Oct 29, 2014
*
* 1st: Feb 22, 2014
* 2nd: Sep 27, 2014
* 3rd: Oct  8, 2014
* 4th: Oct 20, 2014
*
* CatchSync: Catch Synchronized Behavior in Large Directed Graphs (KDD 2014 Best Paper Final List)
* Meng Jiang, Peng Cui, Alex Beutel, Christos Faloutsos and Shiqiang Yang
*/

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Collections;
import java.util.Map.Entry;
import java.util.Random;
import java.lang.Math;

public class CatchSync {
	
	// The maximum length of elements (string) per record in adjacency list is 1000.
	// You may set the length 1024 when you import the files into sql.
	final static int LEN_ELEMS = 1000;
		
	/** Return the seperator of line. */
	static String sep_of_file(String file_graph) {
		String sep = null, line = null;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_graph)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#' || line.charAt(0) == '%') continue;
				if (line.contains(" ")) sep = " ";
				if (line.contains(",")) sep = ",";
				if (line.contains("\t")) sep = "\t";
				break;
			}
			br.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return sep;
	}
	/** Return number of nodes from [file_nodemap] */
	static int number_of_nodes(String file_nodemap) {
		int N = 0;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_nodemap)));
			String line = br.readLine().substring(1);
			String[] arr = line.split(",");
			N = Integer.valueOf(arr[0]);
			br.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return N;
	}
	/** Solver for ax^3+bx^2+cx+d=0 */
	static double solve(double a, double b, double c, double d) {
		double elem1 = b*c/6/a/a-b*b*b/27/a/a/a-d/2/a;
		double elem2 = c/3/a-b*b/9/a/a;
		double delta2 = elem1*elem1+elem2*elem2*elem2;
		if (delta2 < 0) return 0.0;
		delta2 = Math.sqrt(delta2);
		double delta0 = Math.pow(elem1+delta2, 1.0/3);
		double delta1 = Math.pow(elem1-delta2, 1.0/3);
		return -b/3/a+delta0+delta1;
	}
	
	/**
	 * Generate a random power law graph: alpha = 1.5.
	 */
	static void generate_rplg(String file_edgelist, String file_nodemap, int N, double alpha) {
		double val = Math.pow(0.5, -alpha);
		double D = solve(val+2,-2.0,0.0,-1.0*N);
		if (D == 0) return;
		D = D*D;
		double C = Math.pow(D, alpha);
		HashMap<Integer, Integer> d2c = new HashMap<Integer, Integer>();
		int deg = 1;
		do {
			int c = (int)(Math.pow(deg, -alpha)*C);
			if (c <= 0) break;
			d2c.put(deg, c);
			deg += 1;
		} while (true);
		d2c.put(0, (int)(d2c.get(1)*val));
		int Nc = 0;
		for (int d : d2c.keySet()) Nc += d2c.get(d);
		double p = 1.0*N/Nc;
		HashMap<Integer, Integer> d2cp = new HashMap<Integer, Integer>();
		for (int d : d2c.keySet()) {
			int c = (int)(Math.round(d2c.get(d)*p));
			if (c > 0) d2cp.put(d, c);
		}
		Nc = 0;
		for (int d : d2cp.keySet()) Nc += d2cp.get(d);
		if (Nc <= N) {
			int c = d2cp.get(0);
			d2cp.remove(0);
			d2cp.put(0, c+N-Nc);
		} else {
			int c = d2cp.get(0);
			d2cp.remove(0);
			d2cp.put(0, c+Nc-N);
			/**
			ArrayList<Integer> ds = new ArrayList<Integer>();
			for (int d : d2cp.keySet()) ds.add(d);
			Collections.sort(ds);
			for (int i = ds.size()-1;i > 0;i--) {
				int d = ds.get(i);
				int c = d2cp.get(d);
				if (Nc-c >= N) {
					Nc -= c;
					d2cp.remove(d);
				} else {
					break;
				}
			}
			*/
		}
		int E = 0;
		for (int d : d2cp.keySet()) E += d*d2cp.get(d);
		ArrayList<Integer> outdseq = new ArrayList<Integer>();
		ArrayList<Integer> indseq = new ArrayList<Integer>();
		for (int d : d2cp.keySet()) {
			for (int c = 0;c < d2cp.get(d);c++) {
				outdseq.add(d);
			}
		}
		int[] seq = new int[N];
		for (int i = 0;i < N;i++) seq[i] = i;
		int w;
		Random rand = new Random();
		for (int i = N-1;i > 0;i--) {
			w = rand.nextInt(i);
			int t = seq[i];
			seq[i] = seq[w];
			seq[w] = t;
		}
		for (int i = 0;i < N;i++) indseq.add(outdseq.get(i));
		HashSet<Integer> ers = new HashSet<Integer>();
		HashSet<Integer> ees = new HashSet<Integer>();
		for (int i = 0;i < N;i++) {
			int outd = outdseq.get(i);
			int ind = indseq.get(i);
			if (outd > 0) ers.add(i);
			if (ind > 0) ees.add(i);
		}
		double Ec = 0;
		for (int er : ers) {
			int outd = outdseq.get(er);
			for (int ee : ees) {
				int ind = indseq.get(ee);
				Ec += 1.0*outd*ind/E;
			}
		}
		E = 0;
		try {
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_edgelist));
			for (int er : ers) {
				int outd = outdseq.get(er);
				for (int ee : ees) {
					int ind = indseq.get(ee);
					if (rand.nextDouble() < 1.0*outd*ind/Ec) {
						E += 1;
						bw.write(er+","+ee+"\n");
					}
				}
			}
			bw.close();
			bw = new BufferedWriter(new FileWriter(file_nodemap));
			bw.write("#"+N+","+E+"\n");
			for (int i = 0;i < N;i++) {
				bw.write(i+","+i+"\n");
			}
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Index the node list in [file_graph].
	 * Translate it into edge list [file_edgelist].
	 * Store the index in [file_nodemap].
	 */
	static void graph_to_edgelist(String file_graph, String file_edgelist, String file_nodemap) {
		String sep = sep_of_file(file_graph);
		if (sep == null) {
			System.out.println("The seperator of line in file [" + file_graph + "] should be space/comma/tab!");
			return;
		}
		String line = null;
		int nume = 0;
		HashMap<String, Integer> nodemap = new HashMap<String, Integer>();
		try {
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_edgelist));
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_graph)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#' || line.charAt(0) == '%') continue;
				String[] arr = line.split(sep);
				String node0 = arr[0], node1 = arr[1];
				if (!nodemap.containsKey(node0)) nodemap.put(node0, nodemap.size());
				if (!nodemap.containsKey(node1)) nodemap.put(node1, nodemap.size());
				int elem0 = nodemap.get(node0), elem1 = nodemap.get(node1);
				nume += 1;
				bw.write(elem0+","+elem1+"\n");
			}
			br.close();
			bw.close();
			bw = new BufferedWriter(new FileWriter(file_nodemap));
			bw.write("#"+nodemap.size()+","+nume+"\n");
			for (String node : nodemap.keySet()) bw.write(nodemap.get(node)+","+node+"\n");
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	/**
	 * Inject groups ([injections]) of suspicious fans and idols on edge list of [file_edgelist].
	 * Write down the injected edge list into [file_edgelist_injected].
	 * Store the injected fans into [file_injectedu].
	 * Store the injected idols into [file_injectedv].
	 * For injection, there are two types:
	 *  spike: NUM_FANS, NUM_IDOLS, DEGREE_SPIKE, CAMOUFLAGE_OF_DEGREE
	 *  gap: NUM_FANS, NUM_IDOLS, DEGREE_0 (start), DEGREE_GAP, CAMOUFLAGE_OF_DEGREE
	 */
	static void injected_on_edgelist(String file_edgelist, String file_nodemap,
			String file_edgelist_injected, String file_nodemap_injected,
			String file_injectedu, String file_injectedv, ArrayList<String> injections) {
		Random rand = new Random();
		String line = null;
		try {
			int N0 = number_of_nodes(file_nodemap);
			int nume = 0;
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_edgelist_injected));
			BufferedWriter bwu = new BufferedWriter(new FileWriter(file_injectedu));
			BufferedWriter bwv = new BufferedWriter(new FileWriter(file_injectedv));
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_edgelist)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#' || line.charAt(0) == '%') continue;
				nume += 1;
				bw.write(line+"\n");
			}
			int Nall = N0;
			for (String injection : injections) {
				String[] arr = injection.split(",");
				if (arr[0].equals("spike")) {
					int M = Integer.valueOf(arr[1]), N = Integer.valueOf(arr[2]);
					int Dspike = Integer.valueOf(arr[3]), K = Dspike, Nstart = Nall, i = Nstart;
					double C = Double.valueOf(arr[4]);
					for (int m = 0;m < M;m++) {
						int outd = -1;
						// standard normal distribution
						double v1, v2, w;
						do {
							v1 = rand.nextDouble() * 2.0 - 1.0;
							v2 = rand.nextDouble() * 2.0 - 1.0;
							w = v1 * v1 + v2 * v2;
						} while (w > 1.0);
						outd = (int)(K+v1*Math.sqrt(-2.0*Math.log(w)/w));
						if (outd < 0) outd = K;
						int outdC = (int)(outd*C), outdN = outd - outdC;
						HashSet<Integer> Ns = new HashSet<Integer>(), Cs = new HashSet<Integer>();
						for (int d = 0;d < outdN;d++) Ns.add(Nstart+M+rand.nextInt(N));
						for (int d = 0;d < outdC;d++) Cs.add(rand.nextInt(N0));
						for (int j : Ns) {
							nume += 1;
							bw.write(i+","+j+"\n");
						}
						for (int j : Cs) {
							nume += 1;
							bw.write(i+","+j+"\n");
						}
						i += 1;
					}
					for (i = Nall;i < Nall+M;i++) bwu.write(i+"\n");
					Nall += M;
					for (i = Nall;i < Nall+N;i++) bwv.write(i+"\n");
					Nall += N;					
				}
				if (arr[0].equals("gap")) {
					int M = Integer.valueOf(arr[1]), N = Integer.valueOf(arr[2]);
					int D0 = Integer.valueOf(arr[3]), Dgap = Integer.valueOf(arr[4]);
					double C = Double.valueOf(arr[5]);
					int Md = (int)(1.0*M/(Dgap-D0+1)), Nstart = Nall, i = Nstart;
					for (int outd = D0;outd <= Dgap;outd++) {
						for (int m = 0;m < Md;m++) {
							int outdC = (int)(outd*C), outdN = outd - outdC;
							HashSet<Integer> Ns = new HashSet<Integer>(), Cs = new HashSet<Integer>();
							for (int d = 0;d < outdN;d++) Ns.add(Nstart+M+rand.nextInt(N));
							for (int d = 0;d < outdC;d++) Cs.add(rand.nextInt(N0));
							for (int j : Ns) {
								nume += 1;
								nume += 1;
								bw.write(i+","+j+"\n");
							}
							for (int j : Cs) bw.write(i+","+j+"\n");
							i += 1;
						}
					}
					for (i = Nall;i < Nall+M;i++) bwu.write(i+"\n");
					Nall += M;
					for (i = Nall;i < Nall+N;i++) bwv.write(i+"\n");
					Nall += N;
				}
			}
			br.close();
			bwv.close();
			bwu.close();
			bw.close();
			bw = new BufferedWriter(new FileWriter(file_nodemap_injected));
			bw.write("#"+Nall+","+nume+"\n");
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_nodemap)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#' || line.charAt(0) == '%') continue;
				bw.write(line+"\n");
			}
			br.close();
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_injectedu)));
			while ((line = br.readLine()) != null) {
				bw.write(line+",suspfan"+line+"\n");
			}
			br.close();
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_injectedv)));
			while ((line = br.readLine()) != null) {
				bw.write(line+",suspidol"+line+"\n");
			}
			br.close();
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	/**
	 * For every node [file_nodemap] in edge list [file_edgelist], store its in-degree and out-degree in [file_xindoutd].
	 * Give the out-degree distribution [file_outd2c] and in-degree distribution [file_ind2c].
	 */
	static void edgelist_to_dc(String file_edgelist, String file_nodemap, String file_xindoutd, String file_outd2c, String file_ind2c) {
		String line = null;
		try {
			int N = number_of_nodes(file_nodemap);
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_xindoutd));
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_edgelist)));
			int[][] indoutd = new int[N][2];
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				int elem0 = Integer.valueOf(arr[0]), elem1 = Integer.valueOf(arr[1]);
				indoutd[elem0][1] += 1;
				indoutd[elem1][0] += 1;
			}
			br.close();
			bw.close();
			bw = new BufferedWriter(new FileWriter(file_xindoutd));
			for (int i = 0;i < N;i++) bw.write(i+","+indoutd[i][0]+","+indoutd[i][1]+"\n");
			bw.close();
			HashMap<Integer, Integer> ind2c = new HashMap<Integer, Integer>(), outd2c = new HashMap<Integer, Integer>();
			for (int i = 0;i < N;i++) {
				int ind = indoutd[i][0], outd = indoutd[i][1];
				if (!ind2c.containsKey(ind)) ind2c.put(ind, 0);
				if (!outd2c.containsKey(outd)) outd2c.put(outd, 0);
				ind2c.put(ind, ind2c.get(ind)+1);
				outd2c.put(outd, outd2c.get(outd)+1);
			}
			ArrayList<Integer> ds = new ArrayList<Integer>();
			for (int d : ind2c.keySet()) ds.add(d);
			Collections.sort(ds);
			bw = new BufferedWriter(new FileWriter(file_ind2c));
			for (int d : ds) bw.write(d+","+ind2c.get(d)+"\n");
			bw.close();
			ds.clear();
			for (int d : outd2c.keySet()) ds.add(d);
			Collections.sort(ds);
			bw = new BufferedWriter(new FileWriter(file_outd2c));
			for (int d : ds) bw.write(d+","+outd2c.get(d)+"\n");
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	/**
	 * Translate edge list [file_edgelist] of nodes [file_nodemap] into adjacency list:
	 *  source node to list of destination nodes [file_adjacency_u2vs],
	 *  destination nodes to list of source node [file_adjacency_v2us].
	 * Bucket sort - 1 Gigabytes for a piece of data.
	 */
	static void edgelist_to_adjacencylist(String file_edgelist, String file_adjacency_u2vs, String file_adjacency_v2us) {
		String line = null;
		String temphead = "_temp";
		int MAX_SIZE_SLICE = 1000000000; // 1G
		File file = new File(file_edgelist);
		if (!file.exists()) return;
		int rmod = (int)(1.0+file.length()/MAX_SIZE_SLICE);
		HashSet<String> files_temp = new HashSet<String>();
		if (rmod > 1) {
			try {
				HashMap<Integer, BufferedWriter> r2bw = new HashMap<Integer, BufferedWriter>();
				for (int r = 0;r < rmod;r++) {
					String file_temp = temphead+r;
					files_temp.add(file_temp);
					BufferedWriter bw = new BufferedWriter(new FileWriter(file_temp));
					r2bw.put(r, bw);
				}
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_edgelist)));
				while ((line = br.readLine()) != null) {
					String[] arr = line.split(",");
					int elem0 = Integer.valueOf(arr[0]);
					int r = elem0 % rmod;
					r2bw.get(r).write(line+"\n");
				}
				br.close();
				for (Integer r : r2bw.keySet()) r2bw.get(r).close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			files_temp.add(file_edgelist);
		}
		HashMap<Integer, String> adjacencylist = new HashMap<Integer, String>();
		try {
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_adjacency_u2vs));
			for (String file_temp : files_temp) {
				adjacencylist.clear();
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_temp)));
				while ((line = br.readLine()) != null) {
					String[] arr = line.split(",");
					int elem0 = Integer.valueOf(arr[0]), elem1 = Integer.valueOf(arr[1]);
					if (adjacencylist.containsKey(elem0)) {
						String elems = adjacencylist.get(elem0)+";"+elem1;
						if (elems.length() > LEN_ELEMS) {
							bw.write(elem0+","+elems+"\n");
							adjacencylist.remove(elem0);
						} else adjacencylist.put(elem0, elems);
					} else adjacencylist.put(elem0, String.valueOf(elem1));
				}
				br.close();
				for (int key : adjacencylist.keySet()) bw.write(key+","+adjacencylist.get(key)+"\n");
			}
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (rmod > 1) {
			for (String file_temp : files_temp) {
				if (file_temp.contains(temphead)) {
					file = new File(file_temp);
					if (file.exists()) file.delete();
				}
			}
		}
		files_temp.clear();
		if (rmod > 1) {
			try {
				HashMap<Integer, BufferedWriter> r2bw = new HashMap<Integer, BufferedWriter>();
				for (int r = 0;r < rmod;r++) {
					String file_temp = temphead+r;
					files_temp.add(file_temp);
					BufferedWriter bw = new BufferedWriter(new FileWriter(file_temp));
					r2bw.put(r, bw);
				}
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_edgelist)));
				while ((line = br.readLine()) != null) {
					String[] arr = line.split(",");
					int elem1 = Integer.valueOf(arr[1]);
					int r = elem1 % rmod;
					r2bw.get(r).write(line+"\n");
				}
				br.close();
				for (Integer r : r2bw.keySet()) r2bw.get(r).close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			files_temp.add(file_edgelist);
		}
		try {
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_adjacency_v2us));
			for (String file_temp : files_temp) {
				adjacencylist.clear();
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_temp)));
				while ((line = br.readLine()) != null) {
					String[] arr = line.split(",");
					int elem0 = Integer.valueOf(arr[0]), elem1 = Integer.valueOf(arr[1]);
					if (adjacencylist.containsKey(elem1)) {
						String elems = adjacencylist.get(elem1)+";"+elem0;
						if (elems.length() > LEN_ELEMS) {
							bw.write(elem1+","+elems+"\n");
							adjacencylist.remove(elem1);
						} else adjacencylist.put(elem1, elems);
					} else adjacencylist.put(elem1, String.valueOf(elem0));
				}
				br.close();
				for (int key : adjacencylist.keySet()) bw.write(key+","+adjacencylist.get(key)+"\n");
			}
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (rmod > 1) {
			for (String file_temp : files_temp) {
				if (file_temp.contains(temphead)) {
					file = new File(file_temp);
					if (file.exists()) file.delete();
				}
			}
		}
	}
	
	/**
	 * With adjacency list [file_adjacency_u2vs] and [file_adjacency_v2us],
	 * via HITS algorithm - calculate Hub score u1 and Authority score v1,
	 * with T iterations, store Hub vs. out-degree and Authority vs. in-degree.
	 */
	static void hits_degree_adjacency(String file_adjacency_u2vs, String file_adjacency_v2us, String file_nodemap,
			String file_xindoutd, String file_xu1outd, String file_xv1ind, int T) {
		String line = null;
		double[] u1, v1;
		double sum;
		try {
			int N = number_of_nodes(file_nodemap);
			u1 = new double[N];
			v1 = new double[N];
			for (int i = 0;i < N;i++) v1[i] = 1.0;
			for (int t = 0;t < T;t++) { // each iteration of HITS algorithm
				sum = 0.0;
				for (int i = 0;i < N;i++) sum += v1[i] * v1[i];
				sum = Math.sqrt(sum);
				for (int i = 0;i < N;i++) v1[i] /= sum;
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_adjacency_u2vs)));
				while ((line = br.readLine()) != null) {
					String[] arr = line.split(",");
					int elem0 = Integer.valueOf(arr[0]);
					arr = arr[1].split(";");
					for (String selem : arr) {
						int elem1 = Integer.valueOf(selem);
						u1[elem0] += v1[elem1];
					}
				}
				br.close();
				sum = 0.0;
				for (int i = 0;i < N;i++) sum += u1[i] * u1[i];
				sum = Math.sqrt(sum);
				for (int i = 0;i < N;i++) u1[i] /= sum;
				br = new BufferedReader(new InputStreamReader(new FileInputStream(file_adjacency_v2us)));
				while ((line = br.readLine()) != null) {
					String[] arr = line.split(",");
					int elem0 = Integer.valueOf(arr[0]);
					arr = arr[1].split(";");
					for (String selem : arr) {
						int elem1 = Integer.valueOf(selem);
						v1[elem0] += u1[elem1];
					}
				}
				br.close();
			}
			sum = 0.0;
			for (int i = 0;i < N;i++) sum += v1[i] * v1[i];
			sum = Math.sqrt(sum);
			for (int i = 0;i < N;i++) v1[i] /= sum;
			BufferedWriter bwu = new BufferedWriter(new FileWriter(file_xu1outd));
			BufferedWriter bwv = new BufferedWriter(new FileWriter(file_xv1ind));
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_xindoutd)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				int i = Integer.valueOf(arr[0]), ind = Integer.valueOf(arr[1]), outd = Integer.valueOf(arr[2]);
				bwu.write(i+","+u1[i]+","+outd+"\n");
				bwv.write(i+","+v1[i]+","+ind+"\n");
			}
			br.close();
			bwv.close();
			bwu.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * With edge list [file_edgelist],
	 * via HITS algorithm - calculate Hub score u1 and Authority score v1,
	 * with T iterations, store Hub vs. out-degree and Authority vs. in-degree.
	 */
	static void hits_degree(String file_edgelist, String file_nodemap,
			String file_xindoutd, String file_xu1outd, String file_xv1ind, int T) {
		String line = null;
		double[] u1, v1;
		double sum;
		try {
			int N = number_of_nodes(file_nodemap);
			u1 = new double[N];
			v1 = new double[N];
			for (int i = 0;i < N;i++) v1[i] = 1.0;
			for (int t = 0;t < T;t++) { // each iteration of HITS algorithm
				sum = 0.0;
				for (int i = 0;i < N;i++) sum += v1[i] * v1[i];
				sum = Math.sqrt(sum);
				for (int i = 0;i < N;i++) v1[i] /= sum;
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_edgelist)));
				while ((line = br.readLine()) != null) {
					String[] arr = line.split(",");
					int elem0 = Integer.valueOf(arr[0]);
					int elem1 = Integer.valueOf(arr[1]);
					u1[elem0] += v1[elem1];
				}
				br.close();
				sum = 0.0;
				for (int i = 0;i < N;i++) sum += u1[i] * u1[i];
				sum = Math.sqrt(sum);
				for (int i = 0;i < N;i++) u1[i] /= sum;
				br = new BufferedReader(new InputStreamReader(new FileInputStream(file_edgelist)));
				while ((line = br.readLine()) != null) {
					String[] arr = line.split(",");
					int elem0 = Integer.valueOf(arr[0]);
					int elem1 = Integer.valueOf(arr[1]);
					v1[elem1] += u1[elem0];
				}
				br.close();
			}
			sum = 0.0;
			for (int i = 0;i < N;i++) sum += v1[i] * v1[i];
			sum = Math.sqrt(sum);
			for (int i = 0;i < N;i++) v1[i] /= sum;
			BufferedWriter bwu = new BufferedWriter(new FileWriter(file_xu1outd));
			BufferedWriter bwv = new BufferedWriter(new FileWriter(file_xv1ind));
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_xindoutd)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				int i = Integer.valueOf(arr[0]), ind = Integer.valueOf(arr[1]), outd = Integer.valueOf(arr[2]);
				bwu.write(i+","+u1[i]+","+outd+"\n");
				bwv.write(i+","+v1[i]+","+ind+"\n");
			}
			br.close();
			bwv.close();
			bwu.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * With Power-[P] method, divide scatter plot [file_scatter] into N1xN2 cells.
	 * See the position (cell) of each point in [file_xcell].
	 * See the cell plot in [file_cell]: first two lines are numbers of cells on axis
	 * and then we have N1xN2 numbers - frequency of points in each cell.
	 */
	static void scatter_to_cell(String file_scatter, String file_xcell, String file_cell, double P) {
		String line = null;
		double logP = Math.log(P);
		int minc1 = 1000, maxc1 = -1000;
		int minc2 = 1000, maxc2 = -1000;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_scatter)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double d1 = Double.valueOf(arr[1]);
				double d2 = Double.valueOf(arr[2]);
				if (d1 == 0 || d2 == 0) continue;
				int c1 = (int)(Math.log(d1)/logP);
				int c2 = (int)(Math.log(d2)/logP);
				minc1 = Math.min(minc1, c1);
				maxc1 = Math.max(maxc1, c1);
				minc2 = Math.min(minc2, c2);
				maxc2 = Math.max(maxc2, c2);
			}
			br.close();
			int Nc1 = maxc1-minc1+1, Nc2 = maxc2-minc2+1;
			int[][] cells = new int[Nc1][Nc2];
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_xcell));
			bw.write("#"+minc1+","+maxc1+","+minc2+","+maxc2+"\n");
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_scatter)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double d1 = Double.valueOf(arr[1]);
				double d2 = Double.valueOf(arr[2]);
				if (d1 == 0 || d2 == 0) continue;
				int c1 = (int)(Math.log(d1)/logP);
				int c2 = (int)(Math.log(d2)/logP);
				int nc1 = c1-minc1, nc2 = c2-minc2;
				bw.write(arr[0]+","+nc1+","+nc2+"\n");
				cells[nc1][nc2] += 1;
			}
			br.close();
			bw.close();
			bw = new BufferedWriter(new FileWriter(file_cell));
			String s = "";
			for (int c = minc1;c < maxc1;c++) {
				s += Math.pow(P, (double)c)+",";
			}
			s += Math.pow(P, (double)maxc1)+"\n";
			for (int c = minc2;c < maxc2;c++) {
				s += Math.pow(P, (double)c)+",";
			}
			s += Math.pow(P, (double)maxc2)+"\n";
			bw.write(s);
			for (int i = 0;i < Nc1;i++) {
				s = "";
				for (int j = 0;j < Nc2-1;j++) {
					s += cells[i][j]+",";
				}
				s += cells[i][Nc2-1]+"\n";
				bw.write(s);
			}
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Divide scatter plot [file_scatter] into [NxN] cells.
	 * See the position (cell) of each point in [file_xcell].
	 * See the cell plot in [file_cell]: first two lines are numbers of cells on axis
	 * and then we have [NxN] numbers - frequency of points in each cell.
	 * [mindeg] for lower bound of the 4th column: minimum out/in-degree
	 */
	static void scatter_to_cell_N(String file_scatter, String file_xcell, String file_cell, 
			String axis_log, int N, int mindeg) {
		String AXIS_LOG_LOG = "log";
		String line = null;
		double mind1 = 1000000.0, maxd1 = -1000000.0;
		double mind2 = 1000000.0, maxd2 = -1000000.0;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_scatter)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double d1 = Double.valueOf(arr[1]);
				double d2 = Double.valueOf(arr[2]);
				if (mindeg > 0 && Integer.valueOf(arr[3]) < mindeg) continue;
				if (axis_log.equals(AXIS_LOG_LOG)) {
					if (d1 == 0 || d2 == 0) continue;
					d1 = Math.log(d1);
					d2 = Math.log(d2);
				}
				mind1 = Math.min(mind1, d1);
				maxd1 = Math.max(maxd1, d1);
				mind2 = Math.min(mind2, d2);
				maxd2 = Math.max(maxd2, d2);
			}
			br.close();
			int[][] cells = new int[N][N];
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_xcell));
			bw.write("#"+N+"\n");
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_scatter)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double d1 = Double.valueOf(arr[1]);
				double d2 = Double.valueOf(arr[2]);
				if (mindeg > 0 && Integer.valueOf(arr[3]) < mindeg) continue;
				if (axis_log.equals(AXIS_LOG_LOG)) {
					if (d1 == 0 || d2 == 0) continue;
					d1 = Math.log(d1);
					d2 = Math.log(d2);
				}
				int c1 = Math.min(N-1, (int)((d1-mind1)/(maxd1-mind1)*N));
				int c2 = Math.min(N-1, (int)((d2-mind2)/(maxd2-mind2)*N));
				bw.write(arr[0]+","+c1+","+c2+"\n");
				cells[c1][c2] += 1;
			}
			br.close();
			bw.close();
			bw = new BufferedWriter(new FileWriter(file_cell));
			String s = "";
			for (int c = 0;c < N-1;c++) {
				double num = mind1+(maxd1-mind1)*c/N;
				if (axis_log.equals(AXIS_LOG_LOG)) num = Math.exp(num);
				s += num+",";
			}
			if (axis_log.equals(AXIS_LOG_LOG)) maxd1 = Math.exp(maxd1);
			s += maxd1+"\n";
			for (int c = 0;c < N-1;c++) {
				double num = mind2+(maxd2-mind2)*c/N;
				if (axis_log.equals(AXIS_LOG_LOG)) num = Math.exp(num);
				s += num+",";
			}
			if (axis_log.equals(AXIS_LOG_LOG)) maxd2 = Math.exp(maxd2);
			s += maxd2+"\n";
			bw.write(s);
			for (int i = 0;i < N;i++) {
				s = "";
				for (int j = 0;j < N-1;j++) {
					s += cells[i][j]+",";
				}
				s += cells[i][N-1]+"\n";
				bw.write(s);
			}
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * With [file_xcell]: node -> cellid (x-feature HITS, y-feature Degree),
	 * [file_cell]: cell -> frequency,
	 * [file_adjacency]: adjacency list, node's neighbors,
	 * calculate 'normalcy' and 'coherence' of neighbors.
	 */
	static void normalcy_coherence_old(String file_nodemap, String file_adjacency, String file_xcell, String file_cell, String file_norcohdeg) {
		String temphead = "_temp";
		int MAX_SIZE_SLICE = 100000000; // 100M
		File file = new File(file_adjacency);
		if (!file.exists()) return;
		int rmod = (int)(1.0+file.length()/MAX_SIZE_SLICE);
		// prepare temporal files - x->cid->foreground
		HashSet<String> files_temp = new HashSet<String>();
		HashMap<Integer, BufferedWriter> r2bw = new HashMap<Integer, BufferedWriter>();
		try {
			for (int r = 0;r < rmod;r++) {
				String file_temp = temphead+r;
				files_temp.add(file_temp);
				BufferedWriter bw = new BufferedWriter(new FileWriter(file_temp));
				r2bw.put(r, bw);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		String line = null;
		String[] arr = null;
		HashMap<String, Integer> cell2cid = null;
		int[] x2cid = null;
		int[] cid2bg = null;
		try {
			// cell of nodes (as the neighbors of the objective)
			cell2cid = new HashMap<String, Integer>();
			int N = number_of_nodes(file_nodemap);
			x2cid = new int[N];
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_xcell)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#' || line.charAt(0) == '%') continue;
				arr = line.split(",");
				int x = Integer.valueOf(arr[0]);
				String cell = arr[1]+","+arr[2];
				if (!cell2cid.containsKey(cell)) cell2cid.put(cell, cell2cid.size());
				x2cid[x] = cell2cid.get(cell);
			}
			br.close();
			System.out.println("Already load cell->cid and x->cid!");
			// frequency of background points of every cell
			int C = cell2cid.size();
			cid2bg = new int[C];
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_cell)));
			arr = br.readLine().split(",");
			int C1 = arr.length;
			arr = br.readLine().split(",");
			int C2 = arr.length;
			for (int c1 = 0;c1 < C1;c1++) {
				line = br.readLine();
				arr = line.split(",");
				for (int c2 = 0;c2 < C2;c2++) {
					int freq = Integer.valueOf(arr[c2]);
					if (freq > 0) {
						String cell = c1+","+c2;
						int cid = cell2cid.get(cell);
						cid2bg[cid] = freq;
					}
				}
			}
			br.close();
			System.out.println("Already load cid->background!");
			// cell to frequency of foreground points of every node
			HashMap<Integer, Integer> cid2fg = new HashMap<Integer, Integer>();
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_adjacency)));
			int linenum = 0;
			while ((line = br.readLine()) != null) {
				linenum += 1;
				if (linenum % 100000 == 0) {
					System.out.println("Loading x->cid->foreground: line "+linenum);
				}
				cid2fg.clear();
				arr = line.split(",");
				int elem0 = Integer.valueOf(arr[0]);
				arr = arr[1].split(";");
				for (String selem : arr) {
					int elem1 = Integer.valueOf(selem);
					int cid = x2cid[elem1];
					int fg = 0;
					if (cid2fg.containsKey(cid)) fg = cid2fg.get(cid);
					fg += 1;
					cid2fg.remove(cid);
					cid2fg.put(cid, fg);
				}
				int r = elem0 % rmod;
				String s = elem0+",";
				for (int cid : cid2fg.keySet()) {
					s += cid+"_"+cid2fg.get(cid)+";";
				}
				s = s.substring(0,s.length()-1);
				r2bw.get(r).write(s+"\n");
			}
			br.close();
			System.out.println("Already load x->cid->foreground!");
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			for (Integer r : r2bw.keySet()) r2bw.get(r).close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (cid2bg == null) return;
		try {
			// normalcy and coherence
			String[] arrcidfg = null;
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_norcohdeg));
			for (String file_temp : files_temp) {
				HashMap<Integer,HashMap<Integer, Integer>> x2cid2fg = new HashMap<Integer,HashMap<Integer, Integer>>();
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_temp)));
				while ((line = br.readLine()) != null) {
					arr = line.split(",");
					int x = Integer.valueOf(arr[0]);
					if (!x2cid2fg.containsKey(x)) x2cid2fg.put(x,new HashMap<Integer, Integer>());
					arr = arr[1].split(";");
					for (String cidfg : arr) {
						arrcidfg = cidfg.split("_");
						int cid = Integer.valueOf(arrcidfg[0]);
						int fg = Integer.valueOf(arrcidfg[1]);
						if (x2cid2fg.get(x).containsKey(cid)) {
							fg += x2cid2fg.get(x).get(cid);
							x2cid2fg.get(x).remove(cid);
						}
						x2cid2fg.get(x).put(cid, fg);
					}
				}
				br.close();
				for (int x : x2cid2fg.keySet()) {
					double bgfg = 0.0, fgfg = 0.0, bg, fg;
					int B = 0, F = 0;
					for (int cid : x2cid2fg.get(x).keySet()) {
						fg = x2cid2fg.get(x).get(cid);
						bg = cid2bg[cid];
						bgfg += bg*fg;
						fgfg += fg*fg;
						B += bg;
						F += fg;
					}
					if (F > 0) {
						double normalcy = bgfg/B/F, coherence = fgfg/F/F;
						bw.write(x+","+normalcy+","+coherence+","+F+"\n");
					}
				}
			}
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			for (String file_temp : files_temp) {
				file = new File(file_temp);
				if (file.exists()) file.delete();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	static void normalcy_coherence(String file_nodemap, String file_adjacency, String file_xcell, String file_cell, String file_norcohdeg) {
		String temphead = "_temp";
		int MAX_SIZE_SLICE = 100000000; // 100M
		File file = new File(file_adjacency);
		if (!file.exists()) return;
		int rmod = (int)(1.0+file.length()/MAX_SIZE_SLICE);
		// prepare temporal files - x->cid->foreground
		HashSet<String> files_temp = new HashSet<String>();
		HashMap<Integer, BufferedWriter> r2bw = new HashMap<Integer, BufferedWriter>();
		try {
			for (int r = 0;r < rmod;r++) {
				String file_temp = temphead+r;
				files_temp.add(file_temp);
				BufferedWriter bw = new BufferedWriter(new FileWriter(file_temp));
				r2bw.put(r, bw);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		String line = null;
		String[] arr = null;
		HashMap<String, Integer> cell2cid = null;
		int[] x2cid = null;
		int[] cid2bg = null;
		try {
			// cell of nodes (as the neighbors of the objective)
			cell2cid = new HashMap<String, Integer>();
			int N = number_of_nodes(file_nodemap);
			x2cid = new int[N];
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_xcell)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#' || line.charAt(0) == '%') continue;
				arr = line.split(",");
				int x = Integer.valueOf(arr[0]);
				String cell = arr[1]+","+arr[2];
				if (!cell2cid.containsKey(cell)) cell2cid.put(cell, cell2cid.size());
				x2cid[x] = cell2cid.get(cell);
			}
			br.close();
			System.out.println("Already load cell->cid and x->cid!");
			// frequency of background points of every cell
			int C = cell2cid.size();
			cid2bg = new int[C];
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_cell)));
			arr = br.readLine().split(",");
			int C1 = arr.length;
			arr = br.readLine().split(",");
			int C2 = arr.length;
			for (int c1 = 0;c1 < C1;c1++) {
				line = br.readLine();
				arr = line.split(",");
				for (int c2 = 0;c2 < C2;c2++) {
					int freq = Integer.valueOf(arr[c2]);
					if (freq > 0) {
						String cell = c1+","+c2;
						int cid = cell2cid.get(cell);
						cid2bg[cid] = freq;
					}
				}
			}
			br.close();
			System.out.println("Already load cid->background!");
			// cell to frequency of foreground points of every node
			HashMap<Integer, Integer> cid2fg = new HashMap<Integer, Integer>();
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_adjacency)));
			int linenum = 0;
			while ((line = br.readLine()) != null) {
				linenum += 1;
				if (linenum % 100000 == 0) {
					System.out.println("Loading x->cid->foreground: line "+linenum);
				}
				cid2fg.clear();
				arr = line.split(",");
				int elem0 = Integer.valueOf(arr[0]);
				arr = arr[1].split(";");
				for (String selem : arr) {
					int elem1 = Integer.valueOf(selem);
					int cid = x2cid[elem1];
					int fg = 0;
					if (cid2fg.containsKey(cid)) fg = cid2fg.get(cid);
					fg += 1;
					cid2fg.remove(cid);
					cid2fg.put(cid, fg);
				}
				int r = elem0 % rmod;
				String s = elem0+",";
				for (int cid : cid2fg.keySet()) {
					s += cid+"_"+cid2fg.get(cid)+";";
				}
				s = s.substring(0,s.length()-1);
				r2bw.get(r).write(s+"\n");
			}
			br.close();
			System.out.println("Already load x->cid->foreground!");
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			for (Integer r : r2bw.keySet()) r2bw.get(r).close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (cid2bg == null) return;
		try {
			// normalcy and coherence
			int B = 0;
			for (int bg : cid2bg) B += bg;	
			String[] arrcidfg = null;
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_norcohdeg));
			for (String file_temp : files_temp) {
				HashMap<Integer,HashMap<Integer, Integer>> x2cid2fg = new HashMap<Integer,HashMap<Integer, Integer>>();
				BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_temp)));
				while ((line = br.readLine()) != null) {
					arr = line.split(",");
					int x = Integer.valueOf(arr[0]);
					if (!x2cid2fg.containsKey(x)) x2cid2fg.put(x,new HashMap<Integer, Integer>());
					arr = arr[1].split(";");
					for (String cidfg : arr) {
						arrcidfg = cidfg.split("_");
						int cid = Integer.valueOf(arrcidfg[0]);
						int fg = Integer.valueOf(arrcidfg[1]);
						if (x2cid2fg.get(x).containsKey(cid)) {
							fg += x2cid2fg.get(x).get(cid);
							x2cid2fg.get(x).remove(cid);
						}
						x2cid2fg.get(x).put(cid, fg);
					}
				}
				br.close();
				for (int x : x2cid2fg.keySet()) {
					double bgfg = 0.0, fgfg = 0.0, bg, fg;
//					int B = 0, F = 0;
					int F = 0;
					for (int cid : x2cid2fg.get(x).keySet()) {
						fg = x2cid2fg.get(x).get(cid);
						bg = cid2bg[cid];
						bgfg += bg*fg;
						fgfg += fg*fg;
//						B += bg;
						F += fg;
					}
					if (F > 0) {
						double normalcy = bgfg/B/F, coherence = fgfg/F/F;
						bw.write(x+","+normalcy+","+coherence+","+F+"\n");
					}
				}
			}
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			for (String file_temp : files_temp) {
				file = new File(file_temp);
				if (file.exists()) file.delete();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * With [file_norcohdeg]: node -> normalcy, coherence and degree (out-degree, in-degree),
	 * fit a parabola of NORMALCY vs COHERENCE: {coh} = a*{nor}*{nor}.
	 * Output [file_suspdeg]: node -> suspiciousness, degree,
	 * while suspiciousness = coherence-a*normalcy*normalcy.
	 */
	static void fit_parabola(String file_norcohdeg, String file_suspdeg) {
		int INTERVAL = 100;
		double[] mincoh = new double[INTERVAL];
		for (int i = 0;i < INTERVAL;i++) mincoh[i] = 1.0;
		double suma = 0.0;
		double vara = 0.0;
		double avga = 0.0;
		String line = null;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_norcohdeg)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double nor = Double.valueOf(arr[1]);
				double coh = Double.valueOf(arr[2]);
				int pos = ((int)(nor*INTERVAL))%INTERVAL;
				mincoh[pos] = Math.min(mincoh[pos], coh);
			}
			br.close();
			for (int i = 0;i < INTERVAL;i++) {
				double nor = 1.0*(i+0.5)/INTERVAL;
				double coh = mincoh[i];
				double a = coh/nor/nor;
				suma += a;
			}
			avga = suma/INTERVAL;
			for (int i = 0;i < INTERVAL;i++) {
				double nor = 1.0*(i+0.5)/INTERVAL;
				double coh = mincoh[i];
				double a = coh/nor/nor;
				vara += (a-avga)*(a-avga);
			}
			vara = Math.sqrt(vara/INTERVAL);			
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_suspdeg));
			bw.write("#"+avga+","+vara+"\n");
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_norcohdeg)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double nor = Double.valueOf(arr[1]);
				double coh = Double.valueOf(arr[2]);
				double susp = coh-avga*nor*nor;
				bw.write(arr[0]+","+susp+","+arr[3]+"\n");
			}
			br.close();
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();			
		}
	}
	
	/**
	 * With [file_norcohdeg]: node -> normalcy, coherence and degree (out-degree, in-degree),
	 * fit a power curve of NORMALCY vs COHERENCE: {coh} = {nor}^a.
	 * Output [file_suspdeg]: node -> suspiciousness, degree,
	 * while suspiciousness = coherence-normalcy^a.
	 */
	static void fit_power(String file_norcohdeg, String file_suspdeg) {
		int INTERVAL = 100;
		double[] mincoh = new double[INTERVAL];
		for (int i = 0;i < INTERVAL;i++) mincoh[i] = 1.0;
		double suma = 0.0;
		double vara = 0.0;
		double avga = 0.0;
		String line = null;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_norcohdeg)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double nor = Double.valueOf(arr[1]);
				double coh = Double.valueOf(arr[2]);
				int pos = ((int)(nor*INTERVAL))%INTERVAL;
				mincoh[pos] = Math.min(mincoh[pos], coh);
			}
			br.close();
			for (int i = 0;i < INTERVAL;i++) {
				double nor = 1.0*(i+0.5)/INTERVAL;
				double coh = mincoh[i];
				double a = Math.log(coh)/Math.log(nor);
				suma += a;
			}
			avga = suma/INTERVAL;
			for (int i = 0;i < INTERVAL;i++) {
				double nor = 1.0*(i+0.5)/INTERVAL;
				double coh = mincoh[i];
				double a = Math.log(coh)/Math.log(nor);
				vara += (a-avga)*(a-avga);
			}
			vara = Math.sqrt(vara/INTERVAL);			
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_suspdeg));
			bw.write("#"+avga+","+vara+"\n");
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_norcohdeg)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double nor = Double.valueOf(arr[1]);
				double coh = Double.valueOf(arr[2]);
				double susp = coh-Math.pow(nor, avga);
				bw.write(arr[0]+","+susp+","+arr[3]+"\n");
			}
			br.close();
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();			
		}
	}
	
	/**
	 * With [file_norcohdeg]: node -> normalcy, coherence and degree (out-degree, in-degree),
	 * output [file_suspdeg]: node -> suspiciousness, degree,
	 * while suspiciousness = coherence-1*normalcy.
	 */
	static void fit_slope(String file_norcohdeg, String file_suspdeg) {
		String line = null;
		try {			
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_suspdeg));
			bw.write("#1,0\n");
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_norcohdeg)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				double nor = Double.valueOf(arr[1]);
				double coh = Double.valueOf(arr[2]);
				double susp = coh-nor;
				bw.write(arr[0]+","+susp+","+arr[3]+"\n");
			}
			br.close();
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();			
		}
	}
	
	/**
	 * With [file_suspdeg]: node -> suspiciousness, degree, and [min_deg],
	 * output [file_suspncdf]: suspiciousness -> ncdf.
	 */
	/*
	static void susp_ncdf(String file_suspdeg, String file_suspncdf, int min_deg) {
		int INTERVAL = 1000;
		int[] numsusp = new int[INTERVAL];
		int N = 0;
		for (int i = 0;i < INTERVAL;i++) numsusp[i] = 0;
		String line = null;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_suspdeg)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				double susp = Double.valueOf(arr[1]);
				if (susp < 0) susp = 0;
				int deg = Integer.valueOf(arr[2]);
				if (deg >= min_deg) {
					int pos = ((int)(susp*INTERVAL))%INTERVAL;
					numsusp[pos] += 1;
					N += 1;
				}
			}
			br.close();
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_suspncdf));
			int num = 0;
			bw.write("0,1\n");
			for (int i = 0;i < INTERVAL;i++) {
				num += numsusp[i];
				double susp = 1.0*(i+0.5)/INTERVAL;
				double ncdf = 1.0-1.0*num/N;
				bw.write(susp+","+ncdf+"\n");
			}
			bw.write("1,0\n");
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();			
		}
	}
	*/
	
	/**
	 * Blame idols whose suspiciousness > average+VARTIMES*variance in [file_vsuspind].
	 * Give suspiciousness of fans as average suspiciousness of their idols with [file_ususpoutd] and [file_v2us].
	 * Output out-degree distribution [file_outd2crm] and [file_outd2cnew].
	 * ps: minimum in-degree of idols = [minind] and maximum out-degree of fans = [maxoutd].
	 */
	static void rmsuspvu(String file_vsuspind, String file_ususpoutd, String file_v2us,
			int minind, int minoutd,
			double VARTIMES_u, double VARTIMES_v,
			int N_u, int N_v, int N_nc,
			String[] types, HashMap<String, String[]> type2files) {
		String line = null;
		HashSet<Integer> uset = new HashSet<Integer>();
		HashSet<Integer> vset = new HashSet<Integer>();
		HashMap<Integer, Double> u2susp = new HashMap<Integer, Double>();
		HashMap<Integer, Double> v2susp = new HashMap<Integer, Double>();
		HashMap<Integer, Integer> u2outd = new HashMap<Integer, Integer>();
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_vsuspind)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				int v = Integer.valueOf(arr[0]);
				double susp = Double.valueOf(arr[1]);
				int ind = Integer.valueOf(arr[2]);
				if (ind < minind) continue;
				v2susp.put(v, susp);
			}
			br.close();
			double avg_vsusp = 0.0, var_vsusp = 0.0;
			for (double susp : v2susp.values()) avg_vsusp += susp;
			avg_vsusp /= v2susp.size();
			for (double susp : v2susp.values()) var_vsusp += (susp-avg_vsusp)*(susp-avg_vsusp);
			var_vsusp = Math.sqrt(var_vsusp/v2susp.size());
			// System.out.println(avg_vsusp+"\t"+var_vsusp);
			for (Entry<Integer, Double> e : v2susp.entrySet()) {
				if (e.getValue() > avg_vsusp+VARTIMES_v*var_vsusp) vset.add(e.getKey());
			}
			// System.out.println(vset.size());
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_v2us)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				int v = Integer.valueOf(arr[0]);
				if (!vset.contains(v)) continue;
				arr = arr[1].split(";");
				for (String ustr : arr) {
					int u = Integer.valueOf(ustr);
					double susp = 0.0;
					if (u2susp.containsKey(u)) {
						susp = u2susp.get(u)+1.0;
						u2susp.remove(u);
					}
					u2susp.put(u, susp);
				}
			}
			br.close();
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_ususpoutd)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				int u = Integer.valueOf(arr[0]);
				int outd = Integer.valueOf(arr[2]);
				u2outd.put(u, outd);
				if (u2susp.containsKey(u)) {
					double susp = u2susp.get(u)/outd;
					u2susp.remove(u);
					u2susp.put(u, susp);
				}
			}
			br.close();
			HashSet<Integer> uset0 = new HashSet<Integer>();
			for (int u : u2susp.keySet()) if (u2outd.get(u) < minoutd) uset0.add(u);
			for (int u : uset0) u2susp.remove(u);
			double avg_ususp = 0.0, var_ususp = 0.0;
			for (double susp : u2susp.values()) avg_ususp += susp;
			avg_ususp /= u2susp.size();
			for (double susp : u2susp.values()) var_ususp += (susp-avg_ususp)*(susp-avg_ususp);
			var_ususp = Math.sqrt(var_ususp/u2susp.size());
			for (Entry<Integer, Double> e : u2susp.entrySet()) {
				if (e.getValue() > avg_ususp+VARTIMES_u*var_ususp) uset.add(e.getKey());
			}
			u2susp = null;
			v2susp = null;
			u2outd = null;
			HashMap<Integer, Integer> outd2crm = new HashMap<Integer, Integer>();
			HashMap<Integer, Integer> ind2crm = new HashMap<Integer, Integer>();
			int[][] cells_uhdrm = new int[N_u][N_u];
			int[][] cells_vhdrm = new int[N_v][N_v];
			int[][] cells_uncrm = new int[N_nc][N_nc];
			int[][] cells_vncrm = new int[N_nc][N_nc];
			HashSet<Integer> xset = null;
			for (String type : types) {
				if (type.charAt(0) == 'u' ||
						type.charAt(0) == 'v' ||
						type.charAt(0) == 'x') {
					if (type.contains("u")) xset = uset;
					if (type.contains("v")) xset = vset;
					if (xset == null) continue;
					String[] files = type2files.get(type);
					BufferedWriter bw1 = new BufferedWriter(new FileWriter(files[1]));
					BufferedWriter bw2 = new BufferedWriter(new FileWriter(files[2]));
					br = new BufferedReader(new InputStreamReader(new FileInputStream(files[0])));
					while ((line = br.readLine()) != null) {
						if (line.charAt(0) == '#') continue;
						String[] arr = line.split(",");
						int x = Integer.valueOf(arr[0]);
						if (xset.contains(x))
							bw1.write(line+"\n");
						else
							bw2.write(line+"\n");
						if (!xset.contains(x)) continue;
						if (type.equals("xu1outd")) {
							int d = Integer.valueOf(arr[2]);
							int c = 1;
							if (outd2crm.containsKey(d)) {
								c += outd2crm.get(d);
								outd2crm.remove(d);
							}
							outd2crm.put(d, c);
						}
						if (type.equals("xv1ind")) {
							int d = Integer.valueOf(arr[2]);
							int c = 1;
							if (ind2crm.containsKey(d)) {
								c += ind2crm.get(d);
								ind2crm.remove(d);
							}
							ind2crm.put(d, c);
						}
						if (type.substring(0,2).equals("xc")) {
							int i = Integer.valueOf(arr[1]);
							int j = Integer.valueOf(arr[2]);
							if (type.equals("xcu1coutd")) cells_uhdrm[i][j] += 1;
							if (type.equals("xcv1cind")) cells_vhdrm[i][j] += 1;
							if (type.equals("xcunorcucoh")) cells_uncrm[i][j] += 1;
							if (type.equals("xcvnorcvcoh")) cells_vncrm[i][j] += 1;
						}
					}
					br.close();
					bw2.close();
					bw1.close();
				}
				xset = null;
			}
			
			BufferedWriter bw1,bw2;
			String[] files = null;
			String s = null;
			int N_n;
			int[][] cells_uhd = new int[N_u][N_u];
			int[][] cells_vhd = new int[N_v][N_v];
			int[][] cells_unc = new int[N_nc][N_nc];
			int[][] cells_vnc = new int[N_nc][N_nc];
			
			files = type2files.get("cellu1outd");
			N_n = N_u;
			bw1 = new BufferedWriter(new FileWriter(files[1]));
			bw2 = new BufferedWriter(new FileWriter(files[2]));
			br = new BufferedReader(new InputStreamReader(new FileInputStream(files[0])));
			s = br.readLine()+"\n"+br.readLine()+"\n";
			bw1.write(s);
			bw2.write(s);
			for (int i = 0;i < N_n;i++) {
				line = br.readLine();
				String[] arr = line.split(",");
				for (int j = 0;j < N_n;j++) {
					cells_uhd[i][j] = Integer.valueOf(arr[j]);
				}
			}
			br.close();
			for (int i = 0;i < N_n;i++) {
				s = "";
				for (int j = 0;j < N_n-1;j++) {
					s += cells_uhdrm[i][j]+",";
				}
				s += cells_uhdrm[i][N_n-1]+"\n";
				bw1.write(s);
				s = "";
				for (int j = 0;j < N_n-1;j++) {
					s += (cells_uhd[i][j]-cells_uhdrm[i][j])+",";
				}
				s += (cells_uhd[i][N_n-1]-cells_uhdrm[i][N_n-1])+"\n";
				bw2.write(s);
			}
			bw2.close();
			bw1.close();
			files = type2files.get("cellv1ind");
			N_n = N_v;
			bw1 = new BufferedWriter(new FileWriter(files[1]));
			bw2 = new BufferedWriter(new FileWriter(files[2]));
			br = new BufferedReader(new InputStreamReader(new FileInputStream(files[0])));
			s = br.readLine()+"\n"+br.readLine()+"\n";
			bw1.write(s);
			bw2.write(s);
			for (int i = 0;i < N_n;i++) {
				line = br.readLine();
				String[] arr = line.split(",");
				for (int j = 0;j < N_n;j++) {
					cells_vhd[i][j] = Integer.valueOf(arr[j]);
				}
			}
			br.close();
			for (int i = 0;i < N_n;i++) {
				s = "";
				for (int j = 0;j < N_n-1;j++) {
					s += cells_vhdrm[i][j]+",";
				}
				s += cells_vhdrm[i][N_n-1]+"\n";
				bw1.write(s);
				s = "";
				for (int j = 0;j < N_n-1;j++) {
					s += (cells_vhd[i][j]-cells_vhdrm[i][j])+",";
				}
				s += (cells_vhd[i][N_n-1]-cells_vhdrm[i][N_n-1])+"\n";
				bw2.write(s);
			}
			bw2.close();
			bw1.close();
			files = type2files.get("cellunorcoh");
			N_n = N_nc;
			bw1 = new BufferedWriter(new FileWriter(files[1]));
			bw2 = new BufferedWriter(new FileWriter(files[2]));
			br = new BufferedReader(new InputStreamReader(new FileInputStream(files[0])));
			s = br.readLine()+"\n"+br.readLine()+"\n";
			bw1.write(s);
			bw2.write(s);
			for (int i = 0;i < N_n;i++) {
				line = br.readLine();
				String[] arr = line.split(",");
				for (int j = 0;j < N_n;j++) {
					cells_unc[i][j] = Integer.valueOf(arr[j]);
				}
			}
			br.close();
			for (int i = 0;i < N_n;i++) {
				s = "";
				for (int j = 0;j < N_n-1;j++) {
					s += cells_uncrm[i][j]+",";
				}
				s += cells_uncrm[i][N_n-1]+"\n";
				bw1.write(s);
				s = "";
				for (int j = 0;j < N_n-1;j++) {
					s += (cells_unc[i][j]-cells_uncrm[i][j])+",";
				}
				s += (cells_unc[i][N_n-1]-cells_uncrm[i][N_n-1])+"\n";
				bw2.write(s);
			}
			bw2.close();
			bw1.close();
			files = type2files.get("cellvnorcoh");
			N_n = N_nc;
			bw1 = new BufferedWriter(new FileWriter(files[1]));
			bw2 = new BufferedWriter(new FileWriter(files[2]));
			br = new BufferedReader(new InputStreamReader(new FileInputStream(files[0])));
			s = br.readLine()+"\n"+br.readLine()+"\n";
			bw1.write(s);
			bw2.write(s);
			for (int i = 0;i < N_n;i++) {
				line = br.readLine();
				String[] arr = line.split(",");
				for (int j = 0;j < N_n;j++) {
					cells_vnc[i][j] = Integer.valueOf(arr[j]);
				}
			}
			br.close();
			for (int i = 0;i < N_n;i++) {
				s = "";
				for (int j = 0;j < N_n-1;j++) {
					s += cells_vncrm[i][j]+",";
				}
				s += cells_vncrm[i][N_n-1]+"\n";
				bw1.write(s);
				s = "";
				for (int j = 0;j < N_n-1;j++) {
					s += (cells_vnc[i][j]-cells_vncrm[i][j])+",";
				}
				s += (cells_vnc[i][N_n-1]-cells_vncrm[i][N_n-1])+"\n";
				bw2.write(s);
			}
			bw2.close();
			bw1.close();
			files = type2files.get("outd2c");
			bw1 = new BufferedWriter(new FileWriter(files[1]));
			bw2 = new BufferedWriter(new FileWriter(files[2]));
			br = new BufferedReader(new InputStreamReader(new FileInputStream(files[0])));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				int d = Integer.valueOf(arr[0]);
				int c = Integer.valueOf(arr[1]);
				if (outd2crm.containsKey(d)) {
					int crm = outd2crm.get(d);
					c -= crm;
					bw1.write(d+","+crm+"\n");
				}
				if (c > 0) bw2.write(d+","+c+"\n");
			}
			br.close();
			bw2.close();
			bw1.close();
			files = type2files.get("ind2c");
			bw1 = new BufferedWriter(new FileWriter(files[1]));
			bw2 = new BufferedWriter(new FileWriter(files[2]));
			br = new BufferedReader(new InputStreamReader(new FileInputStream(files[0])));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				int d = Integer.valueOf(arr[0]);
				int c = Integer.valueOf(arr[1]);
				if (ind2crm.containsKey(d)) {
					int crm = ind2crm.get(d);
					c -= crm;
					bw1.write(d+","+crm+"\n");
				}
				if (c > 0) bw2.write(d+","+c+"\n");
			}
			br.close();
			bw2.close();
			bw1.close();
		} catch (Exception e) {
			e.printStackTrace();			
		}
	}
	
	/**
	 * Blame fans whose suspiciousness > average+VARTIMES*variance in [file_ususpoutd].
	 * Give suspiciousness of idols as average suspiciousness of their fans with [file_vsuspind] and [file_u2vs].
	 * Output in-degree distribution [file_ind2crm] and [file_ind2cnew].
	 * ps: minimum in-degree of idols = [minind] and maximum out-degree of fans = [maxoutd].
	 */
	/*
	static void rmsuspuv(String file_ususpoutd, String file_vsuspind, String file_u2vs,
			String file_ind2crm, String file_ind2cnew,
			int minoutd, int minind, double VARTIMES_U, double VARTIMES_V) {
		String line = null;
		HashMap<Integer, Double> u2susp = new HashMap<Integer, Double>();
		HashSet<Integer> uset = new HashSet<Integer>();
		HashMap<Integer, Double> v2susp = new HashMap<Integer, Double>();
		HashSet<Integer> vset = new HashSet<Integer>();
		HashMap<Integer, Integer> v2ind = new HashMap<Integer, Integer>();
		HashMap<Integer, Integer> ind2crm = new HashMap<Integer, Integer>();
		HashMap<Integer, Integer> ind2cnew = new HashMap<Integer, Integer>();
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_ususpoutd)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				int u = Integer.valueOf(arr[0]);
				double susp = Double.valueOf(arr[1]);
				int outd = Integer.valueOf(arr[2]);
				if (outd < minoutd) continue;
				u2susp.put(u, susp);
			}
			br.close();
			double avg_ususp = 0.0, var_ususp = 0.0;
			for (double susp : u2susp.values()) avg_ususp += susp;
			avg_ususp /= u2susp.size();
			for (double susp : u2susp.values()) var_ususp += (susp-avg_ususp)*(susp-avg_ususp);
			var_ususp = Math.sqrt(var_ususp/u2susp.size());
			// System.out.println(avg_ususp+"\t"+var_ususp);
			for (Entry<Integer, Double> e : u2susp.entrySet()) {
				if (e.getValue() > avg_ususp+VARTIMES_U*var_ususp) uset.add(e.getKey());
			}
			// System.out.println(uset.size());
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_u2vs)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				int u = Integer.valueOf(arr[0]);
				if (!uset.contains(u)) continue;
				arr = arr[1].split(";");
				for (String vstr : arr) {
					int v = Integer.valueOf(vstr);
					double susp = 0.0;
					if (v2susp.containsKey(v)) {
						susp = v2susp.get(v)+1.0;
						v2susp.remove(v);
					}
					v2susp.put(v, susp);
				}
			}
			br.close();
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_vsuspind)));
			while ((line = br.readLine()) != null) {
				if (line.charAt(0) == '#') continue;
				String[] arr = line.split(",");
				int v = Integer.valueOf(arr[0]);
				int ind = Integer.valueOf(arr[2]);
				v2ind.put(v, ind);
				if (v2susp.containsKey(v)) {
					double susp = v2susp.get(v)/ind;
					v2susp.remove(v);
					v2susp.put(v, susp);
				}
			}
			br.close();
			HashSet<Integer> vset0 = new HashSet<Integer>();
			for (int v : v2susp.keySet()) if (v2ind.get(v) < minind) vset0.add(v);
			for (int v : vset0) v2susp.remove(v);
			double avg_vsusp = 0.0, var_vsusp = 0.0;
			for (double susp : v2susp.values()) avg_vsusp += susp;
			avg_vsusp /= v2susp.size();
			for (double susp : v2susp.values()) var_vsusp += (susp-avg_vsusp)*(susp-avg_vsusp);
			var_vsusp = Math.sqrt(var_vsusp/v2susp.size());
			// System.out.println(avg_vsusp+"\t"+var_vsusp);
			for (Entry<Integer, Double> e : v2susp.entrySet()) {
				if (e.getValue() > avg_vsusp+VARTIMES_V*var_vsusp) vset.add(e.getKey());
			}
			// System.out.println(vset.size());
			for (Entry<Integer, Integer> e : v2ind.entrySet()) {
				int v = e.getKey(), ind = e.getValue();
				if (vset.contains(v)) {
					int c = 1;
					if (ind2crm.containsKey(ind)) {
						c += ind2crm.get(ind);
						ind2crm.remove(ind);
					}
					ind2crm.put(ind, c);
				} else {
					int c = 1;
					if (ind2cnew.containsKey(ind)) {
						c += ind2cnew.get(ind);
						ind2cnew.remove(ind);
					}
					ind2cnew.put(ind, c);
				}
			}
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_ind2crm));
			SortedSet<Integer> inds = new TreeSet<Integer>(ind2crm.keySet());
			for (int ind : inds) bw.write(ind+","+ind2crm.get(ind)+"\n");
			bw.close();
			bw = new BufferedWriter(new FileWriter(file_ind2cnew));
			inds = new TreeSet<Integer>(ind2cnew.keySet());
			for (int ind : inds) bw.write(ind+","+ind2cnew.get(ind)+"\n");
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();			
		}
	}
	*/
	
	/**
	 * Translate Y-axis of degree distribution from node frequency to
	 * complementary cumulative distribution function (CCDF).
	 */
	static void dc_to_dccdf(String file_dc, String file_dccdf) {
		String line = null;
		double ccdf = 1.0;
		try {
			int N = 0;
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_dc)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				int degree = Integer.valueOf(arr[0]);
				int freq = Integer.valueOf(arr[1]);
				if (degree == 0) continue;
				N += freq;
			}
			br.close();
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_dccdf));
			br = new BufferedReader(new InputStreamReader(new FileInputStream(file_dc)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				int degree = Integer.valueOf(arr[0]);
				int freq = Integer.valueOf(arr[1]);
				if (degree == 0) continue;
				bw.write(degree+","+ccdf+"\n");
				ccdf -= 1.0*freq/N;
			}
			br.close();
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Approximate degree distribution to a lower bound of power law
	 * y: frequency (count); x: degree
	 * log(y) = k*log(x)+b (two parameters).
	 * y = k*x+b -> kx-y+b = 0
	 */
	static void approx(String file_dc, String file_approx, int degstart, int degend) {
		ArrayList<Double[]> d2clogs = new ArrayList<Double[]>();
		String line = null;
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file_dc)));
			while ((line = br.readLine()) != null) {
				String[] arr = line.split(",");
				int degree = Integer.valueOf(arr[0]);
				if (degree < degstart || degree > degend) continue;
				double dlog = Math.log(Double.valueOf(arr[0]));
				double clog = Math.log(Double.valueOf(arr[1]));
				Double[] d2clog = {dlog,clog};
				d2clogs.add(d2clog);
			}
			br.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		int N = d2clogs.size();
		double bestdist = -1.0, bestk = 0.0, bestb = 0.0;
		for (int i = 0;i < N-1;i++) {
			Double[] d2clogi = d2clogs.get(i);
			double dlogi = d2clogi[0], clogi = d2clogi[1];
			for (int j = i+1;j < N;j++) {
				Double[] d2clogj = d2clogs.get(j);
				double dlogj = d2clogj[0], clogj = d2clogj[1];
				double k = (clogj-clogi)/(dlogj-dlogi);
				double b = clogi-k*dlogi;
				int valid = 1;
				double dist = 0.0;
				for (int x = 0;x < N-1;x++) {
					Double[] d2clogx = d2clogs.get(x);
					double dlogx = d2clogx[0], clogx = d2clogx[1];
					if (clogx-k*dlogx-b < 0.0) {
						valid = 0;
						break;
					}
					dist += Math.abs(k*dlogx-clogx+b)/Math.sqrt(k*k+1);
				}
				if (valid == 0) continue;
				dist /= N;
				if (bestdist < 0.0 || bestdist > dist) {
					bestdist = dist;
					bestk = k;
					bestb = b;
				}
			}
		}
		try {
			BufferedWriter bw = new BufferedWriter(new FileWriter(file_approx));
//			String s = degstart+","+degend+","+bestk+","+bestb+","+bestdist;
			for (int x = 1;x < 1000;x++) {
				double xlog = Math.log(1.0*x);
				bw.write(x+","+Math.exp(bestk*xlog+bestb)+"\n");
			}
			bw.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args) {
		long startTime = System.currentTimeMillis();
		int argc = args.length;
		if (argc < 2) {
			System.out.println("[step] {parameters in step}");
			return;
		}
		String step = args[0];
		if (step.equals("graph_to_edgelist")) {
			if (argc < 5) {
				System.out.println("[dir_data] [edgelist_graph] [edgelist_matrix] [nodemap]");
				return;
			}
			graph_to_edgelist(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4]);
		}
		if (step.equals("injected_on_edgelist")) {
			if (argc < 7) {
				System.out.println("[edgelist] [nodemap] [edgelist_injected] [nodemap_injected] " +
						"[injectedu] [injectedv] " +
						"{parameters in injection - spike(M,N,Dspike,C), gap(M,N,D0,Dgap,C)}");
				return;
			}
			ArrayList<String> argslist = new ArrayList<String>();
			for (int i = 7;i < argc;i++) {
				argslist.add(args[i]);
			}
			injected_on_edgelist(args[1], args[2], args[3], args[4], args[5], args[6], argslist);
		}
		if (step.equals("edgelist_to_dc")) {
			if (argc < 7) {
				System.out.println("[dir_data] [edgelist] [nodemap] [xindoutd] [outd2c] [ind2c]");
				return;
			}
			edgelist_to_dc(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4],
					args[1]+"/"+args[5], args[1]+"/"+args[6]);
		}
		if (step.equals("edgelist_to_adjacencylist")) {
			if (argc < 5) {
				System.out.println("[dir_data] [edgelist] [adjacencylist_u2vs] [adjacencylist_v2us]");
				return;
			}
			edgelist_to_adjacencylist(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4]);
		}
		if (step.equals("hits_degree_adjacency")) {
			if (argc < 9) {
				System.out.println("[dir_data] [adjacencylist_u2vs] [adjacencylist_v2us] [nodemap] " +
						"[xindoutd] [xu1outd] [xv1ind] [T]");
				return;
			}
			hits_degree_adjacency(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4],
					args[1]+"/"+args[5], args[1]+"/"+args[6], args[1]+"/"+args[7],
					Integer.valueOf(args[8]));
		}
		if (step.equals("hits_degree")) {
			if (argc < 8) {
				System.out.println("[dir_data] [edgelist] [nodemap] [xindoutd] [xu1outd] [xv1ind] [T]");
				return;
			}
			hits_degree(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4],
					args[1]+"/"+args[5], args[1]+"/"+args[6], Integer.valueOf(args[7]));
		}
		if (step.equals("scatter_to_cell")) {
			if (argc < 6) {
				System.out.println("[dir_data] [scatter:xu1outd,xv1ind] [xcell:xcu1coutd,xcv1cind] " +
						"[cell:cellu1outd,cellv1ind] [power:P]");
				return;
			}
			scatter_to_cell(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4], Double.valueOf(args[5]));
		}
		if (step.equals("scatter_to_cell_N")) {
			if (argc < 7) {
				System.out.println("[dir_data] [scatter:xu1outd,xv1ind] [xcell:xcu1coutd,xcv1cind] " +
						"[cell:cellu1outd,cellv1ind] [axis_log:logarithm-logarithm(log)/linear-linear(lin)] " +
						"[N:NxN] {MIN degree (4th column) - lower bound}");
				return;
			}
			int mindeg = 0;
			if (argc == 8) mindeg = Integer.valueOf(args[7]);
			scatter_to_cell_N(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4],
					args[5], Integer.valueOf(args[6]), mindeg);
		}
		if (step.equals("normalcy_coherence")) {
			if (argc < 7) {
				System.out.println("[dir_data] [nodemap] [adjacencylist:u2vs,v2us] [xcell:xcv1cind,xcu1coutd] " +
						"[cell:cellv1ind,cellu1outd] [norcohdeg:unorcohoutd,vnorcohind]");
				return;
			}
			normalcy_coherence(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4], 
					args[1]+"/"+args[5], args[1]+"/"+args[6]);
		}
		if (step.equals("fit_parabola")) {
			if (argc < 4) {
				System.out.println("[dir_data] [norcohdeg:unorcohoutd,vnorcohind] [suspdeg:ususpoutd,vsuspind]");
				return;
			}
			fit_parabola(args[1]+"/"+args[2], args[1]+"/"+args[3]);
		}
		if (step.equals("fit_power")) {
			if (argc < 4) {
				System.out.println("[dir_data] [norcohdeg:unorcohoutd,vnorcohind] [suspdeg:ususpoutd,vsuspind]");
				return;
			}
			fit_power(args[1]+"/"+args[2], args[1]+"/"+args[3]);
		}
		if (step.equals("fit_slope")) {
			if (argc < 4) {
				System.out.println("[dir_data] [norcohdeg:unorcohoutd,vnorcohind] [suspdeg:ususpoutd,vsuspind]");
				return;
			}
			fit_slope(args[1]+"/"+args[2], args[1]+"/"+args[3]);
		}
		/*
		if (step.equals("susp_ncdf")) {
			if (argc < 4) {
				System.out.println("[dir_data] [suspdeg:ususpoutd,vsuspind] [suspncdf:ususpncdf,vsuspncdf] " +
						"{MIN degree (3th column) - lower bound}");
				return;
			}
			int mindeg = 0;
			if (argc == 5) mindeg = Integer.valueOf(args[4]);
			susp_ncdf(args[1]+"/"+args[2], args[1]+"/"+args[3], mindeg);
		}
		*/
		if (step.equals("rmsuspvu")) {
			if (argc < 29) {
				System.out.println("[dir_data] [vsuspind] [ususpoutd] [adjacencylist_v2us] " +
						"[minind] [minoutd] " +
						"[VARTIMES_u] [VARTIMES_v] " +
						"[N_u] [N_v] [N_nc] " +
						"[rm] [new] [setting] " +
						"[xu1outd] [xv1ind]  " +
						"[xcu1coutd] [xcv1cind] " +
						"[cellu1outd] [cellv1ind] " +
						"[unorcohoutd] [vnorcohind] " +
						"[xcunorcucoh] [xcvnorcvcoh] " +
						"[cellunorcoh] [cellvnorcoh] "+
						"[outd2c] [ind2c]"
						);
				return;
			}
			String[] types = new String[] {
					"xu1outd","xv1ind","xcu1coutd","xcv1cind","cellu1outd","cellv1ind",
					"unorcohoutd","vnorcohind","xcunorcucoh","xcvnorcvcoh","cellunorcoh","cellvnorcoh",
					"outd2c","ind2c"};
			HashMap<String, String[]> type2files = new HashMap<String, String[]>();
			for (int i = 0;i < types.length;i++) {
				type2files.put(types[i],
						new String[] {args[1]+"/"+args[i+15],
						args[1]+"/"+args[i+15]+args[12]+args[14],
						args[1]+"/"+args[i+15]+args[13]+args[14]});
			}
			rmsuspvu(args[1]+"/"+args[2], args[1]+"/"+args[3], args[1]+"/"+args[4],
					Integer.valueOf(args[5]), Integer.valueOf(args[6]),
					Double.valueOf(args[7]), Double.valueOf(args[8]),
					Integer.valueOf(args[9]), Integer.valueOf(args[10]), Integer.valueOf(args[11]),
					types, type2files);
			
		}
		/*
		if (step.equals("rmsuspuv")) {
			if (argc < 11) {
				System.out.println("[dir_data] [ususpoutd] [vsuspind] [minoutd] " +
						"[adjacencylist_u2vs] [minind] [ind2crm] [ind2cnew] " +
						"[VARTIMES_U] [VARTIMES_V]");
				return;
			}
			rmsuspuv(args[1]+"/"+args[2], args[1]+"/"+args[3],
					args[1]+"/"+args[5], args[1]+"/"+args[7], args[1]+"/"+args[8],
					Integer.valueOf(args[4]), Integer.valueOf(args[6]),
					Double.valueOf(args[9]), Double.valueOf(args[10]));
		}
		*/
		if (step.equals("dc_to_dccdf")) {
			if (argc < 4) {
				System.out.println("[dir_data] [d2c] [d2ccdf]");
				return;
			}
			dc_to_dccdf(args[1]+"/"+args[2], args[1]+"/"+args[3]);
		}
		if (step.equals("approx")) {
			if (argc < 6) {
				System.out.println("[dir_data] [d2c] [approx] [degstart] [degend]");
				return;
			}
			approx(args[1]+"/"+args[2], args[1]+"/"+args[3],
				Integer.valueOf(args[4]), Integer.valueOf(args[5]));
		}
		if (step.equals("generate_rplg")) {
			if (argc < 5) {
				System.out.println("[edgelist] [nodemap] [N] [alpha]");
				return;
			}
			generate_rplg(args[1], args[2], Integer.valueOf(args[3]),
					Double.valueOf(args[4]));
		}
		long endTime = System.currentTimeMillis();
		System.out.println("Time cost: "+(endTime-startTime));
	}
} 
