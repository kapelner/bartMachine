package AlgorithmTesting;

import java.beans.XMLEncoder;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import bartMachine.*;
import bartMachine.Classifier.ErrorTypes;


public class DataAnalysis {
	
	/** this is a file that is in CSV format (csv extension) with/out a header named c_<name> or r_<name> for classification or regression respectively */
	private static final String DataSetFilename = "r_simple";
//	private static final String DataSetFilename = "r_just_noise";
//	private static final String DataSetFilename = "r_treemodel";
//	private static final String DataSetFilename = "r_treemodel_high_p";
//	private static final String DataSetFilename = "r_treemodel_high_p_low_n";
//	private static final String DataSetFilename = "r_treemodel_high_n";
//	private static final String DataSetFilename = "r_treemodel_low_n";	
//	private static final String DataSetFilename = "r_friedman";
//	private static final String DataSetFilename = "r_friedman_hd";	
//	private static final String DataSetFilename = "r_univariatelinear";
//	private static final String DataSetFilename = "r_bivariatelinear";
//	private static final String DataSetFilename = "r_boston";
//	private static final String DataSetFilename = "r_boston_half";	
//	private static final String DataSetFilename = "r_boston_tiny_with_missing";	
//	private static final String DataSetFilename = "r_zach";
//	private static final String DataSetFilename = "r_forestfires";
//	private static final String DataSetFilename = "r_wine_white";
//	private static final String DataSetFilename = "r_concretedata";
//	private static final String DataSetFilename = "c_breastcancer";
//	private static final String DataSetFilename = "c_crime";
//	private static final String DataSetFilename = "c_crime_big";	

	public static void main(String[] args) throws IOException{
		System.out.println("java ver: " + System.getProperty("java.version"));
		//make sure y is last column of data matrix
		DataSetupForCSVFile data = new DataSetupForCSVFile(new File("datasets", DataSetFilename + ".csv"), true);
		Classifier machine = null; //we're going to use some machine to do it... 
		
		//if the filename begins with a "c" => classification task, if it begins with an "r" => regression task
		if (DataSetFilename.charAt(0) == 'c'){ //classification problem
			machine = new bartMachineClassificationMultThread();
			long start_time = System.currentTimeMillis();
			machine.setData(data.getX_y());
			
			machine.Build(); 
			System.out.println("errors: " + 
					(int)machine.calculateInSampleLoss(Classifier.ErrorTypes.MISCLASSIFICATION, 4) + 
					"/" + 
					machine.getN() +
					"  (" + machine.calculateInSampleLoss(ErrorTypes.MISCLASSIFICATION, 4) / (double) machine.getN() * 100 + "%)");
			long end_time = System.currentTimeMillis();
			System.out.println("Current Time:"+ (end_time-start_time)/1000);
		}
		else {
			//regression problem
			machine = new bartMachineRegressionMultThread();
			machine.setData(data.getX_y());
			machine.Build();
			long L2 = Math.round(machine.calculateInSampleLoss(Classifier.ErrorTypes.L2, 4));
			System.out.println("(in sample) L1 error: " + 
				Math.round(machine.calculateInSampleLoss(Classifier.ErrorTypes.L1, 4)) +
				" L2 error: " + L2 + " rmse: " + Math.sqrt(L2 / (double)machine.getN()));
		}
		
		
		ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream("bartMachine.bin"));
		out.writeObject(machine);
		out.close();
//		XMLEncoder encoder = new XMLEncoder(new BufferedOutputStream(new FileOutputStream("bartMachine.xml")));
//		encoder.writeObject(machine);
//		encoder.close();
		
		
		machine = null;
		System.out.println("serialized and now reopening bartmachine");
		
		
		try {
			ObjectInputStream reader = new ObjectInputStream(new FileInputStream("bartMachine.bin")); 
			machine = new bartMachineRegressionMultThread(); 
			try {
				machine = (bartMachineRegressionMultThread) reader.readObject();
			} catch (ClassNotFoundException e) {e.printStackTrace();} 
			reader.close();
		} catch (IOException e){e.printStackTrace();}
		
		double[] record = {1.0};
		System.out.println(machine.Evaluate(record));
		

	}
}
