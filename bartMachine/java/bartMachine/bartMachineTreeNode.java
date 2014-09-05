package bartMachine;

import gnu.trove.list.array.TDoubleArrayList;
import gnu.trove.list.array.TIntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import OpenSourceExtensions.TDoubleHashSetAndArray;
import OpenSourceExtensions.UnorderedPair;

/**
 * The class that stores all the information in one node of the BART trees
 * 
 * @author Adam Kapelner and Justin Bleich
 */
public class bartMachineTreeNode implements Cloneable {
	
	/** Setting this to true will print out debug information at the node level during Gibbs sampling */
	public static final boolean DEBUG_NODES = false;
	
	/** a flag that represents an invalid double value */
	protected static final double BAD_FLAG_double = -Double.MAX_VALUE;
	/** a flag that represents an invalid integer value */
	protected static final int BAD_FLAG_int = -Integer.MAX_VALUE;
	
	/** a link back to the overall bart model */
	private bartMachine_b_hyperparams bart;	
	/** the parent node of this node */
	public bartMachineTreeNode parent;
	/** the left daughter node */
	public bartMachineTreeNode left;
	/** the right daughter node */
	public bartMachineTreeNode right;
	/** the generation of this node from the top node (root note has generation = 0 by definition) */
	public int depth;
	/** is this node a terminal node? */
	public boolean isLeaf;
	/** the attribute this node makes a decision on */
	public int splitAttributeM;
	/** the value this node makes a decision on */
	public double splitValue;
	/** send missing data to the right? */ 
	public boolean sendMissingDataRight;
	/** if this is a leaf node, then the result of the prediction for regression, otherwise null */
	public double y_pred = BAD_FLAG_double;
	
	/** the number of data points in this node */
	public transient int n_eta;
	/** these are the yhats in the correct order */
	public transient double[] yhats;

	/** the indices in {0, 1, ..., n-1} of the data records in this node */
	protected transient int[] indicies;	
	/** the y's in this node */
	protected transient double[] responses;
	/** the square of the sum of the responses, y */
	private transient double sum_responses_qty_sqd;
	/** the sum of the responses, y */
	private transient double sum_responses_qty;	
	/** this caches the possible split variables populated only if the <code>mem_cache_for_speed</code> feature is set to on */
	private transient TIntArrayList possible_rule_variables;
	/** this caches the possible split values BY variable populated only if the <code>mem_cache_for_speed</code> feature is set to on */
	private transient HashMap<Integer, TDoubleHashSetAndArray> possible_split_vals_by_attr;
	/** this number of possible split variables at this node */
	protected transient Integer padj;	
	/** a shared pointer to an object that tabulates the counts of attributes being used in split points in this tree */
	protected int[] attribute_split_counts;

	//special caches for gibbs sampler speedup. These are never to be duplicated during clone
	//they are initialized to BAD_FLAG so we can save a pointer hop as well by using the Java primitive
	public double qqqq = BAD_FLAG_double;

	
	public bartMachineTreeNode(){}	
	
	/**
	 * Creates a new node
	 * 
	 * @param parent		The parent of this node
	 * @param bart		The BART model this node belongs to
	 */
	public bartMachineTreeNode(bartMachineTreeNode parent, bartMachine_b_hyperparams bart){
		this.parent = parent;
		this.yhats = parent.yhats;
		this.attribute_split_counts = parent.attribute_split_counts;
		this.bart = bart;
		
		if (parent != null){
			depth = parent.depth + 1;
		}
		isLeaf = true; //default is that it is a leaf
	}
	
	/**
	 * Creates a new node
	 * 
	 * @param parent		The parent of this node
	 */
	public bartMachineTreeNode(bartMachineTreeNode parent){
		this(parent, parent.bart);
	}
	
	/**
	 * Creates a new node
	 * 
	 * @param bart		The BART model this node belongs to
	 */
	public bartMachineTreeNode(bartMachine_b_hyperparams bart) {
		this.bart = bart;
		isLeaf = true;
		depth = 0;
	}

	/**
	 * Creates a cloned copy of the tree beginning at this node by recursing cloning its children.
	 * The clone is shallow to save memory.
	 * 
	 * @return	A cloned copy of this tree
	 */
	public bartMachineTreeNode clone(){
		bartMachineTreeNode copy = new bartMachineTreeNode();
		copy.bart = bart;
		copy.parent = parent;
		copy.isLeaf = isLeaf;
		copy.splitAttributeM = splitAttributeM;
		copy.splitValue = splitValue;
		copy.possible_rule_variables = possible_rule_variables;
		copy.sendMissingDataRight = sendMissingDataRight;
		copy.possible_split_vals_by_attr = possible_split_vals_by_attr;
		copy.depth = depth;
		copy.responses = responses;
		copy.indicies = indicies;
		copy.n_eta = n_eta;
		copy.yhats = yhats;
		copy.attribute_split_counts = attribute_split_counts.clone();
		
		if (left != null){ //we need to clone the child and mark parent correctly
			copy.left = left.clone();
			copy.left.parent = copy;
		}
		if (right != null){ //we need to clone the child and mark parent correctly
			copy.right = right.clone();
			copy.right.parent = copy;
		}		
		return copy;
	}
	
	/**
	 * The sample average response value at this node
	 * 
	 * @return	The sample average value
	 */
	public double avgResponse(){
		return StatToolbox.sample_average(responses);
	}
	
	/**
	 * Search the tree below this current node for terminal nodes that have more than <code>n_rule</code>
	 * data records
	 * 
	 * @param n_rule	The number of data records in in terminal nodes of interest we wish to select
	 * @return			A list of these nodes
	 */
	public ArrayList<bartMachineTreeNode> getTerminalNodesWithDataAboveOrEqualToN(int n_rule){
		ArrayList<bartMachineTreeNode> terminal_nodes_data_above_n = new ArrayList<bartMachineTreeNode>();
		findTerminalNodesDataAboveOrEqualToN(this, terminal_nodes_data_above_n, n_rule);
		return terminal_nodes_data_above_n;
	}
	
	/**
	 * Return all terminal nodes under this node
	 * 
	 * @return	A list of the terminal nodes
	 */
	public ArrayList<bartMachineTreeNode> getTerminalNodes(){
		return getTerminalNodesWithDataAboveOrEqualToN(0);
	}
	
	/**
	 * Search the tree recursively below a node for terminal nodes that have more than <code>n_rule</code>
	 * data records
	 * 
	 * @param node				The node under investigation
	 * @param terminal_nodes	The growing list of nodes that fit this criteria
	 * @param n_rule			The number of data records in in terminal nodes of interest we wish to select
	 */
	private static void findTerminalNodesDataAboveOrEqualToN(bartMachineTreeNode node, ArrayList<bartMachineTreeNode> terminal_nodes, int n_rule) {
		if (node.isLeaf && node.n_eta >= n_rule){
			terminal_nodes.add(node);
		}
		else if (!node.isLeaf){ //as long as we're not in a leaf we should recurse
			if (node.left == null || node.right == null){
				System.err.println("error node no children during findTerminalNodesDataAboveOrEqualToN id: " + node.stringID());
			}			
			findTerminalNodesDataAboveOrEqualToN(node.left, terminal_nodes, n_rule);
			findTerminalNodesDataAboveOrEqualToN(node.right, terminal_nodes, n_rule);
		}
	}
	
	/**
	 * Find a list of prunable and changeable nodes (ie singly internal nodes) 
	 * from this point in the tree and below
	 * 
	 * @return	The list of these nodes
	 */
	public ArrayList<bartMachineTreeNode> getPrunableAndChangeableNodes(){
		ArrayList<bartMachineTreeNode> prunable_and_changeable_nodes = new ArrayList<bartMachineTreeNode>();
		findPrunableAndChangeableNodes(this, prunable_and_changeable_nodes);
		return prunable_and_changeable_nodes;
	}

	/**
	 * Find a list of prunable and changeable nodes (ie singly internal nodes) 
	 * from a node and below using recursion
	 * 
	 * @param node				The node to find singly internal nodes below
	 * @param prunable_nodes	A running list of singly internal nodes
	 */
	private static void findPrunableAndChangeableNodes(bartMachineTreeNode node, ArrayList<bartMachineTreeNode> prunable_nodes) {
		if (node.isLeaf){
			return;
		}
		else if (node.left.isLeaf && node.right.isLeaf){
			prunable_nodes.add(node);
		}
		else {
			findPrunableAndChangeableNodes(node.left, prunable_nodes);
			findPrunableAndChangeableNodes(node.right, prunable_nodes);
		}
	}

	/**
	 * We prune the tree at this node. We cut off its children, mark it as a leaf / terminal node,
	 * and erase its split rule
	 * 
	 * @param node		The node at which to trim the tree at
	 */
	public static void pruneTreeAt(bartMachineTreeNode node) {
		node.left = null;
		node.right = null;
		node.isLeaf = true;
		node.splitAttributeM = BAD_FLAG_int;
		node.splitValue = BAD_FLAG_double;
	}
	
	/**
	 * Find the terminal node at the largest depth from this point in the tree and down
	 * 
	 * @return	The depth of the deepest terminal node
	 */
	public int deepestNode(){
		if (this.isLeaf){
			return 0;
		}
		else {
			int ldn = this.left.deepestNode();
			int rdn = this.right.deepestNode();
			if (ldn > rdn){
				return 1 + ldn;
			}
			else {
				return 1 + rdn;
			}
		}
	}

	/**
	 * Evaluate a record recursively accounting for split rules and the presence of missing data
	 * 
	 * @param record		The record which to evaluate in this tree
	 * @return				The returned prediction from the terminal node that this tree structure maps the record to
	 */
	public double Evaluate(double[] record) {
		bartMachineTreeNode evalNode = this;
		while (true){
			if (evalNode.isLeaf){
				return evalNode.y_pred;
			}
			//all split rules are less than or equals (this is merely a convention)
			//it's a convention that makes sense - if X_.j is binary, and the split values can only be 0/1
			//then it MUST be <= so both values can be considered
			//handle missing data first
			if (Classifier.isMissing(record[evalNode.splitAttributeM])){
				evalNode = evalNode.sendMissingDataRight ? evalNode.right : evalNode.left;
			}			
			else if (record[evalNode.splitAttributeM] <= evalNode.splitValue){
				evalNode = evalNode.left;
			}
			else {
				evalNode = evalNode.right;
			}
		}
	}

	/** Remove all the data in this node and its children recursively to save memory */
	public void flushNodeData() {
		yhats = null;
		indicies = null;	
		responses = null;
		possible_rule_variables = null;
		possible_split_vals_by_attr = null;
		
		if (this.left != null)
			this.left.flushNodeData();
		if (this.right != null)
			this.right.flushNodeData();
	}
	
	/** Propagates the records to the appropriate daughter nodes recursively after a change in split rule. */
	public void propagateDataByChangedRule() {		
		if (isLeaf){ //only propagate if we are in a split node and NOT a leaf
			if (DEBUG_NODES){
				printNodeDebugInfo("propagateDataByChangedRule LEAF");
			}
			return;
		}
		
		//split the data correctly
		TIntArrayList left_indices = new TIntArrayList(n_eta); 
		TIntArrayList right_indices = new TIntArrayList(n_eta);
		TDoubleArrayList left_responses = new TDoubleArrayList(n_eta);
		TDoubleArrayList right_responses = new TDoubleArrayList(n_eta);
		
		for (int i = 0; i < n_eta; i++){
			double[] datum = bart.X_y.get(indicies[i]);
			//handle missing data first
			if (Classifier.isMissing(datum[splitAttributeM])){
				if (sendMissingDataRight){
					right_indices.add(indicies[i]);
					right_responses.add(responses[i]);
				} 
				else {
					left_indices.add(indicies[i]);
					left_responses.add(responses[i]);					
				}
			}
			else if (datum[splitAttributeM] <= splitValue){
				left_indices.add(indicies[i]);
				left_responses.add(responses[i]);
			}
			else {
				right_indices.add(indicies[i]);
				right_responses.add(responses[i]);
			}
		}
		//populate the left daughter
		left.n_eta = left_responses.size();
		left.responses = left_responses.toArray();
		left.indicies = left_indices.toArray();
		//populate the right daughter
		right.n_eta = right_responses.size();
		right.responses = right_responses.toArray();
		right.indicies = right_indices.toArray();
		//recursively propagate to children
		left.propagateDataByChangedRule();
		right.propagateDataByChangedRule();
	}
	
	/**
	 * Update this node and its children with a set of new responses recursively
	 * 
	 * @param new_responses		The new responses
	 */
	public void updateWithNewResponsesRecursively(double[] new_responses) {
		//nuke previous responses and sums
		responses = new double[n_eta]; //ensure correct dimension
		sum_responses_qty_sqd = 0; //need to be primitives
		sum_responses_qty = 0; //need to be primitives
		//copy all the new data in appropriately
		for (int i = 0; i < n_eta; i++){
			double y_new = new_responses[indicies[i]];
			responses[i] = y_new;
		}
		if (DEBUG_NODES){
			System.out.println("new_responses: (size " + new_responses.length + ") [" + Tools.StringJoin(bart.un_transform_y_and_round(new_responses)) + "]");
			printNodeDebugInfo("updateWithNewResponsesRecursively");
		}
		
		if (this.isLeaf){
			return;
		}		
		this.left.updateWithNewResponsesRecursively(new_responses);
		this.right.updateWithNewResponsesRecursively(new_responses);
	}
	
	/**
	 * How many terminal nodes are below this node?
	 * 
	 * @return	The number of terminal nodes
	 */
	public int numLeaves(){
		if (this.isLeaf){
			return 1;
		}
		else {
			return this.left.numLeaves() + this.right.numLeaves();
		}
	}
	
	/**
	 * Find the total number of nodes (internal and terminal) recursively below this node
	 * 
	 * @return	The number of nodes
	 */
	public int numNodesAndLeaves() {
		if (this.isLeaf){
			return 1;
		}
		else {
			return 1 + this.left.numNodesAndLeaves() + this.right.numNodesAndLeaves();
		}
	}	

	/**
	 * Find the number of singly internal nodes (available for pruning) recursively
	 * from this point in the tree down
	 * 
	 * @return		The number of nodes available for pruning or changing
	 */
	public int numPruneNodesAvailable() {
		if (this.isLeaf){
			return 0;
		}
		if (this.left.isLeaf && this.right.isLeaf){
			return 1;
		}
		return this.left.numPruneNodesAvailable() + this.right.numPruneNodesAvailable();
	}	
	
	/**
	 * Get the prediction for this node transformed back to the original response scale
	 * 
	 * @return	The untransformed prediction
	 */
	public double prediction_untransformed(){
		return y_pred == BAD_FLAG_double ? BAD_FLAG_double : bart.un_transform_y(y_pred);
	}
	
	/**
	 * Find the average repsonse in this node and then transform it back to the original response scale
	 * 
	 * @return	The untransformed average
	 */
	public double avg_response_untransformed(){
		return bart.un_transform_y(avgResponse());
	}

	/**
	 * Find the square of the sum of responses and cache it
	 * 
	 * @return	The squared quantity of the sum of responses
	 */
	public double sumResponsesQuantitySqd() {
		if (sum_responses_qty_sqd == 0){
			sum_responses_qty_sqd = Math.pow(sumResponses(), 2);
		}
		return sum_responses_qty_sqd;
	}

	/**
	 * Simply the sum of the repsonses in this node
	 * 
	 * @return	The sum
	 */
	public double sumResponses() {
		if (sum_responses_qty == 0){
			sum_responses_qty = 0.0;
			for (int i = 0; i < n_eta; i++){
				sum_responses_qty += responses[i];
			}
		}
		return sum_responses_qty;
	}	
	
	/**
	 * A wrapper to find a list of predictors that are valid for splitting at this node. 
	 * If <code>mem_cache_for_speed</code> is turned on in the construction of the BART model, 
	 * this list gets cached.
	 * 
	 * @return		The list of predictors indexed by the columns in the design matrix
	 */
	protected TIntArrayList predictorsThatCouldBeUsedToSplitAtNode() {
		if (bart.mem_cache_for_speed){
			if (possible_rule_variables == null){
				possible_rule_variables = tabulatePredictorsThatCouldBeUsedToSplitAtNode();
			}
			return possible_rule_variables;			
		}
		else {
			return tabulatePredictorsThatCouldBeUsedToSplitAtNode();
		}
	}
	
	/**
	 * Finds a list of predictors that are valid for splitting at this node.
	 * 
	 * @return		The list of predictors indexed by the columns in the design matrix
	 */
	private TIntArrayList tabulatePredictorsThatCouldBeUsedToSplitAtNode() {
		TIntArrayList possible_rule_variables = new TIntArrayList();
		
		for (int j = 0; j < bart.p; j++){
			//if size of unique of x_i > 1
			double[] x_dot_j = bart.X_y_by_col.get(j);
			
			for (int i = 1; i < indicies.length; i++){
				if (x_dot_j[indicies[i - 1]] != x_dot_j[indicies[i]]){
					possible_rule_variables.add(j);
					break;
				}
			}
		}
		return possible_rule_variables;	
	}

	/**
	 * Gets the total number of valid split points that can be used for rules at this juncture
	 * for the selected split rule predictor (it will be different for each predictor)
	 * 
	 * @return		The number of valid split points  
	 */
	public int nAdj(){
		return possibleSplitValuesGivenAttribute().size();
	}	
	
	/**
	 * A wrapper to find a list of split values that are valid for splitting at this node. 
	 * If <code>mem_cache_for_speed</code> is turned on in the construction of the BART model, 
	 * this list gets cached.
	 * 
	 * @return		The list of split values
	 */
	protected TDoubleHashSetAndArray possibleSplitValuesGivenAttribute() {
		if (bart.mem_cache_for_speed){
			if (possible_split_vals_by_attr == null){
				possible_split_vals_by_attr = new HashMap<Integer, TDoubleHashSetAndArray>();
			}
			if (possible_split_vals_by_attr.get(splitAttributeM) == null){
				possible_split_vals_by_attr.put(splitAttributeM, tabulatePossibleSplitValuesGivenAttribute());
			}
			return possible_split_vals_by_attr.get(splitAttributeM);
		} 
		else {
			return tabulatePossibleSplitValuesGivenAttribute();
		}
	}
	
	/**
	 * Finds a list of split values that are valid for splitting at this node.
	 * 
	 * @return		The list of split values
	 */
	private TDoubleHashSetAndArray tabulatePossibleSplitValuesGivenAttribute() {
		double[] x_dot_j = bart.X_y_by_col.get(splitAttributeM);
		double[] x_dot_j_node = new double[n_eta];
		for (int i = 0; i < n_eta; i++){
			double val = x_dot_j[indicies[i]];
			if (Classifier.isMissing(val)){
				x_dot_j_node[i] = BAD_FLAG_double;
			}
			else {
				x_dot_j_node[i] = val;
			}
		}		
		
		TDoubleHashSetAndArray unique_x_dot_j_node = new TDoubleHashSetAndArray(x_dot_j_node);
		unique_x_dot_j_node.remove(BAD_FLAG_double); //kill all missings immediately
		double max = Tools.max(x_dot_j_node);
		unique_x_dot_j_node.remove(max); //kill the max
		return unique_x_dot_j_node;
	}

	/**
	 * Of the valid split values to split at for this split rule at this node, pick one at random
	 * 
	 * @return		The randomly selected split value
	 */
	public double pickRandomSplitValue() {	
		TDoubleHashSetAndArray split_values = possibleSplitValuesGivenAttribute();
		if (split_values.size() == 0){
			return bartMachineTreeNode.BAD_FLAG_double;
		}
		int rand_index = (int) Math.floor(StatToolbox.rand() * split_values.size());
		return split_values.getAtIndex(rand_index);
	}
	
	/**
	 * Is this node a stump? That is, does it have no parents and no children?
	 * 
	 * @return	True if it's a stump
	 */
	public boolean isStump() {
		return parent == null && left == null && right == null;
	}
	
	/**
	 * Find the string ID of this node
	 * 
	 * @return	The unique ID of this node
	 */
	public String stringID() {
		return toString().split("@")[1];
	}	

	/**
	 * When a new stump gets created, this function sets its data. This allows
	 * the stump to grow a tree within a BART model
	 * 
	 * @param X_y		the training data that will be filtered down the tree structure
	 * @param y_trans	the transformed responses that correspond to the <code>X_y</code> data records
	 * @param p			the number of attributes in the training data
	 */
	public void setStumpData(ArrayList<double[]> X_y, double[] y_trans, int p) {
		//pull out X data, set y's, and indices appropriately
		n_eta = X_y.size();
		
		responses = new double[n_eta];
		indicies = new int[n_eta];
		
		
		for (int i = 0; i < n_eta; i++){
			indicies[i] = i;
		}
		for (int i = 0; i < n_eta; i++){
			for (int j = 0; j < p + 2; j++){
				if (j == p){
					responses[i] = y_trans[i];
				}
			}
		}	

		//initialize the yhats
		yhats = new double[n_eta];
		//intialize the var counts
		attribute_split_counts = new int[p];
		//initialize sendMissing
		sendMissingDataRight = pickRandomDirectionForMissingData();
		
		if (DEBUG_NODES){
			printNodeDebugInfo("setStumpData");
		}
	}

	/** Given new yhat predictions, update them for this node */
	public void updateYHatsWithPrediction() {		
		for (int i = 0; i < indicies.length; i++){
			yhats[indicies[i]] = y_pred;
		}
		if (DEBUG_NODES){
			printNodeDebugInfo("updateYHatsWithPrediction");
		}
	}

	/**
	 * A wrapper to find all interactions recursively by checking all splits underneath this node
	 *  
	 * @param set_of_interaction_pairs	A running list of interaction pairs
	 */
	public void findInteractions(HashSet<UnorderedPair<Integer>> set_of_interaction_pairs) {		
		if (this.isLeaf){
			return;
		}
		//add all pairs for which this split at this node interacts
		findSplitAttributesUsedUnderneath(this.splitAttributeM, set_of_interaction_pairs);
		//recurse further to all the children
		this.left.findInteractions(set_of_interaction_pairs);
		this.right.findInteractions(set_of_interaction_pairs);
		
	}

	/**
	 * Finds interactions recursively for one node's attribute by checking all splits underneath this node
	 * 
	 * @param interacted_attribute			The attribute in the top node that is being interacted with split rules in the daughter nodes
	 * @param set_of_interaction_pairs		A running list of interaction pairs
	 */
	private void findSplitAttributesUsedUnderneath(int interacted_attribute, HashSet<UnorderedPair<Integer>> set_of_interaction_pairs) {
		if (this.isLeaf){
			return;
		}
		//add new pair
		if (!this.left.isLeaf){
			set_of_interaction_pairs.add(new UnorderedPair<Integer>(interacted_attribute, this.left.splitAttributeM));
		}
		if (!this.right.isLeaf){
			set_of_interaction_pairs.add(new UnorderedPair<Integer>(interacted_attribute, this.right.splitAttributeM));
		}
		//now recurse
		this.left.findSplitAttributesUsedUnderneath(interacted_attribute, set_of_interaction_pairs);
		this.right.findSplitAttributesUsedUnderneath(interacted_attribute, set_of_interaction_pairs);
	}

	/** When <code>mem_cache_for_speed</code> is set in the BART model, running this function will reset the caches for this node's valid predictors and valid split values */
	public void clearRulesAndSplitCache() {
		possible_rule_variables = null;
		possible_split_vals_by_attr = null;
	}
	
	/**
	 * Picks a random direction for missing data to flow down the tree from this node. As of
	 * now, this is a 50-50 coin flip left:right.
	 * 
	 * @return	True / false is returned
	 */
	public static boolean pickRandomDirectionForMissingData() {
		return StatToolbox.rand() < 0.5 ? false : true;
	}
	
	/**
	 * In debugging, print a string that codes this node's location in the entire tree
	 * 	
	 * @param show_parent	Show a character if this node is a stump
	 * @return				The coded string
	 */
	public String stringLocation(boolean show_parent) {
		if (this.parent == null){
			return show_parent ? "P" : "";
		}
		else if (this.parent.left == this){
			return this.parent.stringLocation(false) + "L";
		}
		else if (this.parent.right == this){
			return this.parent.stringLocation(false) + "R";
		}
		else {
			return this.parent.stringLocation(false) + "?";
		}
	}	

	/**
	 * Prints debug information about this node, its parent and its immediate children
	 * 
	 * @param title		A string to print within this message
	 */
	public void printNodeDebugInfo(String title) {		
		System.out.println("\n" + title + " node debug info for " + this.stringLocation(true) + (isLeaf ? " (LEAF) " : " (INTERNAL NODE) ") + " d = " + depth);
		System.out.println("-----------------------------------------");
		System.out.println("n_eta = " + n_eta + " y_pred = " + (y_pred == BAD_FLAG_double ? "BLANK" : bart.un_transform_y_and_round(y_pred)));
		System.out.println("parent = " + parent + " left = " + left + " right = " + right);
		
		if (this.parent != null){
			System.out.println("----- PARENT RULE:   X_" + parent.splitAttributeM + " <= " + parent.splitValue + " & M -> " + (parent.sendMissingDataRight ? "R" : "L") + " ------");
			//get vals of this x currently here
			double[] x_dot_j = bart.X_y_by_col.get(parent.splitAttributeM);
			double[] x_dot_j_node = new double[n_eta];
			for (int i = 0; i < n_eta; i++){
				x_dot_j_node[i] = x_dot_j[indicies[i]];
			}
			Arrays.sort(x_dot_j_node);
			System.out.println("   all X_" + parent.splitAttributeM + " values here: [" + Tools.StringJoin(x_dot_j_node) + "]");
		}
		
		if (!isLeaf){
			System.out.println("----- RULE:   X_" + splitAttributeM + " <= " + splitValue + " & M -> " + (sendMissingDataRight ? "R" : "L") + " ------");
			//get vals of this x currently here
			double[] x_dot_j = bart.X_y_by_col.get(splitAttributeM);
			double[] x_dot_j_node = new double[n_eta];
			for (int i = 0; i < n_eta; i++){
				x_dot_j_node[i] = x_dot_j[indicies[i]];
			}
			Arrays.sort(x_dot_j_node);
			System.out.println("   all X_" + splitAttributeM + " values here: [" + Tools.StringJoin(x_dot_j_node) + "]");
		}	

		
		System.out.println("sum_responses_qty = " + sum_responses_qty + " sum_responses_qty_sqd = " + sum_responses_qty_sqd);
		
		if (bart.mem_cache_for_speed){
			System.out.println("possible_rule_variables: [" + Tools.StringJoin(possible_rule_variables, ", ") + "]");
			System.out.println("possible_split_vals_by_attr: {");
			if (possible_split_vals_by_attr != null){
				for (int key : possible_split_vals_by_attr.keySet()){
					double[] array = possible_split_vals_by_attr.get(key).toArray();
					Arrays.sort(array);
					System.out.println("  " + key + " -> [" + Tools.StringJoin(array) + "],");
				}
				System.out.print(" }\n");
			}
			else {
				System.out.println(" NULL MAP\n}");
			}
		}
		
		System.out.println("responses: (size " + responses.length + ") [" + Tools.StringJoin(bart.un_transform_y_and_round(responses)) + "]");
		System.out.println("indicies: (size " + indicies.length + ") [" + Tools.StringJoin(indicies) + "]");
		if (Arrays.equals(yhats, new double[yhats.length])){
			System.out.println("y_hat_vec: (size " + yhats.length + ") [ BLANK ]");
		}
		else {
			System.out.println("y_hat_vec: (size " + yhats.length + ") [" + Tools.StringJoin(bart.un_transform_y_and_round(yhats)) + "]");
		}
		System.out.println("-----------------------------------------\n\n\n");
	}	
	
	/**
	 * When counting the attributes used in this tree's split rules, decrement the following attribute
	 * 
	 * @param j		The attribute number (in the original traning data design matrix) to decrement in count
	 */
	public void decrement_variable_count(int j) {
		attribute_split_counts[j]--;
	}

	/**
	 * When counting the attributes used in this tree's split rules, increment the following attribute
	 * 
	 * @param j		The attribute number (in the original traning data design matrix) to increment in count
	 */
	public void increment_variable_count(int j) {
		attribute_split_counts[j]++;		
	}
	
	public bartMachineTreeNode getLeft() {
		return left;
	}
	public bartMachineTreeNode getRight() {
		return right;
	}
	public int getGeneration() {
		return depth;
	}
	public void setGeneration(int generation) {
		this.depth = generation;
	}	
	public boolean isLeaf() {
		return isLeaf;
	}
	public void setLeaf(boolean isLeaf) {
		this.isLeaf = isLeaf;
	}
	public void setLeft(bartMachineTreeNode left) {
		this.left = left;
	}
	public void setRight(bartMachineTreeNode right) {
		this.right = right;
	}
	public int getSplitAttributeM() {
		return splitAttributeM;
	}
	public void setSplitAttributeM(int splitAttributeM) {
		this.splitAttributeM = splitAttributeM;
	}
	public double getSplitValue() {
		return splitValue;
	}
	public void setSplitValue(double splitValue) {
		this.splitValue = splitValue;
	}
}