package bartMachine;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;

import javax.imageio.ImageIO;

/**
 * This class builds illustrations of many trees. Since it is only used
 * as a debugging feature, it is undocumented
 * 
 * @author Adam Kapelner
 */
public class TreeArrayIllustration {

	private int sample_num;
	private ArrayList<bartMachineTreeNode> trees;
	private ArrayList<Double> likelihoods;
	private String unique_name;
	
	public static NumberFormat one_digit_format = NumberFormat.getInstance();
	static {
		one_digit_format.setMaximumFractionDigits(1);
	}	

	public TreeArrayIllustration(int sample_num, String unique_name) {
		this.sample_num = sample_num;
		this.unique_name = unique_name;
		trees = new ArrayList<bartMachineTreeNode>();
		likelihoods = new ArrayList<Double>();
	}

	public void AddTree(bartMachineTreeNode tree) {
		trees.add(tree);
	}

	public void addLikelihood(double lik) {
		likelihoods.add(lik);
	}	
	
	public synchronized void CreateIllustrationAndSaveImage() {
		//first pull out all the tree images
		int m = trees.size();
		int w = 0;
		int h = Integer.MIN_VALUE;
		ArrayList<BufferedImage> canvases = new ArrayList<BufferedImage>(m);
		for (int t = 0; t < m; t++){
			bartMachineTreeNode tree = trees.get(t);
			HashMap<String, String> info = new HashMap<String, String>();
			info.put("tree_num", "" + (t + 1));
			info.put("num_iteration", "" + sample_num);
//			info.put("likelihood", "" + one_digit_format.format(likelihoods.get(t)));
			BufferedImage canvas = new TreeIllustration(tree, info).getCanvas();
			w += canvas.getWidth(); //aggregate the widths
			if (canvas.getHeight() > h){ //get the maximum height
				h = canvas.getHeight();
			}
			canvases.add(canvas);
		}
		
		BufferedImage master_canvas = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_BINARY);
		int sliding_width = 0;
		for (int t = 0; t < m; t++){
			BufferedImage canvas = canvases.get(t);
			master_canvas.getGraphics().drawImage(canvas, sliding_width, 0, null);
			sliding_width += canvas.getWidth();
		}
		saveImageFile(master_canvas);
		
	}
	
	private void saveImageFile(BufferedImage image) {
//		System.out.println("w = " + image.getWidth() + " h = " + image.getHeight() + "sample_num: " + sample_num);
		String title = "BART_" + unique_name + "_iter_" + LeadingZeroes(sample_num, 5);
		try {
			ImageIO.write(image, "PNG", new File(title + ".png"));
		} catch (IOException e) {
			System.err.println("can't save " + title);
		}
	}
	
	private static final String ZEROES = "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
	public static String LeadingZeroes(double num, int num_digits) {
		if (num < 10 && num_digits >= 2){
			return ZEROES.substring(0, num_digits - 1) + num;
		}
		else if (num < 100 && num_digits >= 3){
			return ZEROES.substring(0, num_digits - 2) + num;
		}
		else if (num < 1000 && num_digits >= 4){
			return ZEROES.substring(0, num_digits - 3) + num;
		}
		else if (num < 10000 && num_digits >= 5){
			return ZEROES.substring(0, num_digits - 4) + num;
		}
		else if (num < 100000 && num_digits >= 6){
			return ZEROES.substring(0, num_digits - 5) + num;
		}
		else if (num < 1000000 && num_digits >= 7){
			return ZEROES.substring(0, num_digits - 6) + num;
		}
		else if (num < 10000000 && num_digits >= 8){
			return ZEROES.substring(0, num_digits - 7) + num;
		}
		return String.valueOf(num);
	}
	public static String LeadingZeroes(int num, int num_digits) {
		if (num < 10 && num_digits >= 2){
			return ZEROES.substring(0, num_digits - 1) + num;
		}
		else if (num < 100 && num_digits >= 3){
			return ZEROES.substring(0, num_digits - 2) + num;
		}
		else if (num < 1000 && num_digits >= 4){
			return ZEROES.substring(0, num_digits - 3) + num;
		}
		else if (num < 10000 && num_digits >= 5){
			return ZEROES.substring(0, num_digits - 4) + num;
		}
		else if (num < 100000 && num_digits >= 6){
			return ZEROES.substring(0, num_digits - 5) + num;
		}
		else if (num < 1000000 && num_digits >= 7){
			return ZEROES.substring(0, num_digits - 6) + num;
		}
		else if (num < 10000000 && num_digits >= 8){
			return ZEROES.substring(0, num_digits - 7) + num;
		}
		return String.valueOf(num);
	}		


	
}
