package bartMachine;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.text.NumberFormat;
import java.util.HashMap;

import javax.imageio.ImageIO;

/**
 * This class builds illustrations of a tree. Since it is only used
 * as a debugging feature, it is undocumented
 * 
 * @author Adam Kapelner
 */
public class TreeIllustration {

	private static final Color line_color = Color.WHITE;
	private static final int font_size = 12;
	private static final String font_family = "Arial";
	private static final int margin_in_px = 70;
	private static final double character_width_in_px = 4.2;
	private static final int length_in_px_per_half_split = 20;
	private static final int depth_in_px_per_split = 100;
	public static NumberFormat one_digit_format = NumberFormat.getInstance();
	public static NumberFormat two_digit_format = NumberFormat.getInstance();
	static {
		one_digit_format.setMaximumFractionDigits(1);
		one_digit_format.setGroupingUsed(false);
		two_digit_format.setMaximumFractionDigits(2);
		two_digit_format.setGroupingUsed(false);
	}
	
	/** the root of the tree we're illustrating */
	private bartMachineTreeNode root;
	/** the illustration image */
	private transient BufferedImage canvas;

	//	private int breadth_in_num_splits;
	private int depth_in_num_splits;
	private transient HashMap<String, String> info;

	public TreeIllustration(bartMachineTreeNode root, HashMap<String, String> info) {
		this.root = root;
		this.info = info;
		calculateBreadthAndDepthAndThickness();
		initializeCanvas();
		//recursively draw all splits, start drawing on top and horizontally in the middle
		drawSplit(root, canvas.getWidth() / 2, margin_in_px);
		drawTitle(info);
	}
	
	private BufferedImage cropCanvas() {
		int x_min = Integer.MAX_VALUE;
		int x_max = Integer.MIN_VALUE;
		
		//first get min and max
		for (int i = 0; i < canvas.getWidth(); i++){
			for (int j = 0; j < canvas.getHeight(); j++){
				if (canvas.getRGB(i, j) == line_color.getRGB()){
					if (i > x_max){
						x_max = i;
					}
					if (i < x_min){
						x_min = i;
					}
				}
			}
		}
		//create the new image
		int new_width = x_max - x_min + 2 * margin_in_px;
		BufferedImage new_canvas = new BufferedImage(new_width, canvas.getHeight(), BufferedImage.TYPE_BYTE_BINARY);
		//now copy the old into the new
		for (int i = x_min; i <= x_max; i++){
			for (int j = 0; j < canvas.getHeight(); j++){
				new_canvas.setRGB(i - x_min + margin_in_px, j, canvas.getRGB(i, j));
			}
		}
		//and send it to be saved
		return new_canvas;
	}

	private void drawTitle(HashMap<String, String> info) {
//		System.out.println("drawTitle" + info.get("num_iteration"));
		//draw title and try to center it
		if (info.get("num_iteration") == null){
//			System.out.println("drawTitle num_iter");
			int draw_x = (int)Math.round(canvas.getWidth() / 2.0 - "prior".length() / 2  * character_width_in_px);
			canvas.getGraphics().drawString("prior", draw_x, 15);				
		}
		else if (info.get("num_acceptances") != null && info.get("num_iteration") != null){
			int num_acc = Integer.parseInt(info.get("num_acceptances"));
			int num_iter = Integer.parseInt(info.get("num_iteration"));
			String first_line = "acc: " + num_acc + " iter: " + num_iter + "  (" + Math.round(num_acc / (double)num_iter * 100) + "%)";
			int draw_x = (int)Math.round(canvas.getWidth() / 2.0 - first_line.length() / 2  * character_width_in_px);
			canvas.getGraphics().drawString(first_line, draw_x, 15);	
			
			String second_line = info.get("change_step") + " (" + info.get("changed_node") + ")";			
//			if (info.get("split_attribute") != null){
//				second_line += "   X_" + info.get("split_attribute") + " < " + info.get("split_value");				
//			}
			
			draw_x = (int)Math.round(canvas.getWidth() / 2.0 - second_line.length() / 2  * character_width_in_px);
			canvas.getGraphics().drawString(second_line, draw_x, 15 + 15);			
		}
		if (info.get("tree_num") != null && info.get("num_iteration") != null){
//			System.out.println("drawTitle num_tree");
			int draw_x = (int)Math.round(canvas.getWidth() / 2.0 - "prior".length() / 2  * character_width_in_px);
			String iter_info = info.get("num_iteration") == "0" ? "prior" : "i: " + info.get("num_iteration");
			String tree_info = " t: " + info.get("tree_num");
			String lik_info = " lik: " + info.get("likelihood");
			canvas.getGraphics().drawString(iter_info + tree_info + lik_info, draw_x, 15);
		}
	}
	
	/**
	 * This {@link java.io.FilenameFilter file filter} returns
	 * only image files of type "jpg", "tif", "tiff, and "bmp"
	 *
	 */
	public static class ImageFileFilter implements FilenameFilter{
		/**
		 * Given a file, returns true if it is an image
		 * 
		 * @param dir		the directory the file is located in
		 * @param name		the file itself
		 * @return			whether or not the file is an image
		 */
		public boolean accept(File dir, String name) {
			String[] fileparts=name.split("\\.");
			if (fileparts.length >= 2){
				String ext=fileparts[fileparts.length - 1].toLowerCase();
				if (ext.equals("jpg") || ext.equals("tif") || ext.equals("tiff") || ext.equals("TIFF") || ext.equals("bmp") || ext.equals("png"))
					return true;
				else 
					return false;
			}
			else return false;
		}		
	}	

	public static void DeletePreviousTreeIllustrations(){
		 String[] image_filenames = new File("/").list(new ImageFileFilter());
		 for (String filename : image_filenames){
			 new File(filename).delete();
		 }
	}

	private void saveImageFile(BufferedImage image, HashMap<String, String> info) {
		String title = "r" + info.get("num_restart") + "_a" + (info.get("num_acceptances") == null ? 0 : Integer.parseInt(info.get("num_acceptances")));
		try {
			ImageIO.write(image, "PNG", new File(title + ".png"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void initializeCanvas() {
		int w = 2 * margin_in_px + length_in_px_per_half_split * 2 * (int)Math.pow(2, depth_in_num_splits) * 2;
		int h = 2 * margin_in_px + depth_in_num_splits * depth_in_px_per_split;
		initializeCanvas(w, h);
	}
	
	private static final int border_width = 1;
	private void initializeCanvas(int w, int h) {
		canvas = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_BINARY);
		canvas.getGraphics().setFont(new Font(font_family, Font.PLAIN, font_size));
		canvas.getGraphics().setColor(line_color);
		for (int i = 0; i < w; i++){
			for (int j = 0; j < h; j++){
				if (i < border_width || j < border_width || i >= w - border_width || j >= h - border_width){ //w - i <= border_width || || j - h < border_width
					canvas.setRGB(i, j, Color.WHITE.getRGB());
				}
			}		
		}
	}	

	private void drawSplit(bartMachineTreeNode node, int x, int y) {
//		System.out.println("node:" + node.stringID() + " leaf:" + node.isLeaf + " left: " + node.left + " right:" + node.right);
		Graphics g = canvas.getGraphics();
		if (node.isLeaf && node.log_lambda_comp_pred != bartMachineTreeNode.BAD_FLAG_double){
			String pred = two_digit_format.format(node.prediction_log_lambda());//;
			int draw_x = (int)Math.round(x - pred.length() / 2.0 * character_width_in_px);
			g.drawString(pred + " (" + node.n_eta + ") ", draw_x, y + 16);
		}
		else if (node.splitAttributeM != bartMachineTreeNode.BAD_FLAG_int && node.splitValue != bartMachineTreeNode.BAD_FLAG_double) {
			int attr = node.splitAttributeM;
			double val = node.splitValue;
			String rule_and_n = "X_" + (attr + 1) + " < " + two_digit_format.format(val) + " (" + node.n_eta + ") " + two_digit_format.format(node.avg_response_untransformed());
			int draw_x = (int)Math.round(x - rule_and_n.length() / 2.0 * character_width_in_px);
			g.drawString(rule_and_n, draw_x, y - 5);
			draw_x = (int)Math.round(x - node.stringID().length() / 2.0 * character_width_in_px);
			g.drawString(node.stringID(), draw_x, y + 15);
			//now we have to draw the left and right
			int x_offset = length_in_px_per_half_split * (int)Math.pow(2, depth_in_num_splits - node.depth);
			g.drawLine(x, y, x - x_offset, y);
			g.drawLine(x - x_offset, y, x - x_offset, y + depth_in_px_per_split);
			drawSplit(node.left, x - x_offset, y + depth_in_px_per_split);
			g.drawLine(x, y, x + x_offset, y);
			g.drawLine(x + x_offset, y, x + x_offset, y + depth_in_px_per_split);
			drawSplit(node.right, x + x_offset, y + depth_in_px_per_split);
		}
	}

	private void calculateBreadthAndDepthAndThickness() {
//		breadth_in_num_splits = root.widestGeneration();
		depth_in_num_splits = root.deepestNode();
//		System.out.println("tree: " + root.stringID() + " breadth: " + breadth_in_num_splits + " depth: " + depth_in_num_splits);
	}

	public void WriteTitleAndSaveImage() {
		saveImageFile(cropCanvas(), info);		
	}
	
	public BufferedImage getCanvas() {
		return canvas;
	}
}
