package bartMachine;

public class bartMachinePaneledTreeNode extends bartMachineTreeNode implements Cloneable {

	/** the parent node of this node */
	public bartMachinePaneledTreeNode parent;
	/** the left daughter node */
	public bartMachinePaneledTreeNode left;
	/** the right daughter node */
	public bartMachinePaneledTreeNode right;
	
	
	/**
	 * Creates a new node
	 * 
	 * @param parent		The parent of this node
	 */
	public bartMachinePaneledTreeNode(bartMachinePaneledTreeNode parent){
		super(parent, parent.bart);
	}
	
	/**
	 * Creates a new node
	 * 
	 * @param bart		The BART model this node belongs to
	 */	
	public bartMachinePaneledTreeNode(bartMachine_b_hyperparams bart){
		super(bart);		
	}
}
