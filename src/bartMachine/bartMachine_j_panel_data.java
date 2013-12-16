package bartMachine;

public class bartMachine_j_panel_data extends bartMachine_i_prior_cov_spec {

	public int[] worker_ids;
	
	protected void createChildrenOnGrowNodePanelData(bartMachineTreeNode grow_node) {
		grow_node.left = new bartMachineTreeNode(grow_node);
		grow_node.right = new bartMachineTreeNode(grow_node);
	}
	
	/**
	 * Calculates the log of the likelihood ratio for a grow step
	 * 
	 * @param grow_node		The node that was grown in the proposal tree
	 * @return				The log of the likelihood ratio
	 * @see 				Section A.1.2 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	protected double calcLnLikRatioGrowPanelData(bartMachinePaneledTreeNode grow_node) {
		// TODO Auto-generated method stub
		return 0;
	}
	

	/**
	 * Calculates the log likelihood ratio for a change step
	 * 
	 * @param eta		The node in the original tree that was targeted for a change in the splitting rule
	 * @param eta_star	The same node but now with a different splitting rule
	 * @return			The log likelihood ratio
	 * @see 			Section A.3.2 of Kapelner, A and Bleich, J. bartMachine: A Powerful Tool for Machine Learning in R. ArXiv e-prints, 2013
	 */
	private double calcLnLikRatioChangePanelData(bartMachinePaneledTreeNode eta, bartMachinePaneledTreeNode eta_star) {
		// TODO Auto-generated method stub
		return 0;
	}
	
	
	
	/////////////nothing but scaffold code below, do not alter!
	
	protected bartMachineTreeNode InitializeStump(){
		if (worker_ids == null){
			return super.InitializeStump();
		}
		return new bartMachinePaneledTreeNode(this);
	}	

	protected void createChildrenOnGrowNode(bartMachinePaneledTreeNode grow_node) {
		if (worker_ids == null){
			super.createChildrenOnGrowNode(grow_node);
		}
		createChildrenOnGrowNodePanelData(grow_node);
	}
	
	protected double calcLnLikRatioGrow(bartMachinePaneledTreeNode grow_node) {
		if (worker_ids == null){
			return super.calcLnLikRatioGrow(grow_node);
		}
		return calcLnLikRatioGrowPanelData(grow_node);
	}
	
	protected double calcLnLikRatioChange(bartMachinePaneledTreeNode eta, bartMachinePaneledTreeNode eta_star) {
		if (worker_ids == null){
			return super.calcLnLikRatioChange(eta, eta_star);
		}
		return calcLnLikRatioChangePanelData(eta, eta_star);
	}

	
}
