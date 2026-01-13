package bartMachine;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.logging.FileHandler;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

import jdk.incubator.vector.DoubleVector;

import CustomLogging.*;

/**
 * The base class for all machine learning / statistical-learning
 * algorithms. Extend this class to add your own implementation.
 * 
 * @author Adam Kapelner and Justin Bleich
 */
public abstract class Classifier implements Serializable{
	private static final long serialVersionUID = -7470305759402002995L;

	/** Are we on a Windows machine (sometimes this matters) */
	public static final boolean ON_WINDOWS = System.getProperty("os.name").toLowerCase().indexOf("win") >= 0;
	
	/** The way we represent missing values from within our implementation */
	public static final double MISSING_VALUE = Double.NaN;
	/**
	 * Is this value missing?
	 * 
	 * @param x	The value we wish to check if it is missing
	 * @return	True if the value is missing
	 */
	public static boolean isMissing(double x){
		return Double.isNaN(x);
	}
	
	/** an array of the raw training data by ROW i.e. consisting of xi = [xi1, ..., xiM, yi] */
	protected transient ArrayList<double[]> X_y;
	/** an array of the raw training data by COLUMN i.e. consisting of xj = [x1j, ..., xnj] with the last entry being [y1, ..., yn] */ 
	protected transient ArrayList<double[]> X_y_by_col;
	/** the raw responses */
	protected transient double[] y_orig;
	/** the responses transformed (only if necessary) */
	protected transient double[] y_trans;
	/** the number of records in the training set */
	protected int n;
	/** the number of features / predictors in the training set */
	protected int p;
	
	/** the name of this classifier (useful for debugging) */
	protected String unique_name = "unnamed";

	/** the in sample residuals [e1, ..., en] of this classifier after it has been built and evaluated */
	private transient double[] in_sample_residuals;		

	
	/** A dummy constructor which keeps <code>Serializable</code> happy */
	public Classifier(){}
		
	/**
	 * Adds an observation / record to the training data array. The
	 * observation is converted to doubles and the entries that are 
	 * unrecognized are converted to {@link #MISSING_VALUE}'s.
	 * 
	 * @param x_i	The observation / record to be added as a String array.
	 */
	public void addTrainingDataRow(String[] x_i){
		//initialize data matrix if it hasn't been initialized already
		if (X_y == null){
			X_y = new ArrayList<double[]>();
		}
		
		//now add the new record
		final double[] record = new double[x_i.length];
		for (int i = 0; i < x_i.length; i++){
			try {
				record[i] = Double.parseDouble(x_i[i]);
			}
			catch (NumberFormatException e){
				record[i] = MISSING_VALUE;
//				System.out.println("missing value at record #" + X_y.size() + " attribute #" + i);
			}
		}				
		X_y.add(record);		
	}
	
	/**
	 * This method finalizes the training data after all
	 * records have been added via the method {@link #addTrainingDataRow}.	
	 */
	public void finalizeTrainingData(){
		setData(X_y);
	}
	
	/**
	 * This method sets the training data of this machine learning classifier.
	 * It also populates many essential fields such as <code>n</code>, <code>p</code>,
	 * <code>y_orig</code>, <code>X_y</code> and <code>X_y_by_col</code>.
	 * 
	 * @param X_y	The list of double vectors to be set as the training data.
	 */
	public void setData(ArrayList<double[]> X_y){
		n = X_y.size();
		p = X_y.get(0).length - 1;
//		System.out.println("setData n:" + n + " p:" + p);
		y_orig = extractResponseFromRawData(X_y);
//		for (int i = 0; i < n; i++){
//			System.out.println("i:" + i + " yi:" + y[i]);
//		}
		transformResponseVariable();
//		X = extractDesignMatrixFromRawData(X_y);
		this.X_y = addIndicesToDataMatrix(X_y);
		this.X_y_by_col = getDataMatrixByCol(X_y);
	}
	
	/**
	 * Given a training data set indexed by row, this produces a training
	 * data set indexed by column
	 * 
	 * @param X_y	The training data set indexed by row
	 * @return		The training data set indexed by column
	 */
	private ArrayList<double[]> getDataMatrixByCol(ArrayList<double[]> X_y) {
		 double[][] colMatrix = new double[p][n];
		 java.util.stream.IntStream.range(0, p).parallel().forEach(j -> {
			 for (int i = 0; i < n; i++){
				 colMatrix[j][i] = X_y.get(i)[j];
			 }
		 });
		 ArrayList<double[]> X_y_by_col = new ArrayList<double[]>(p);
		 for (int j = 0; j < p; j++){
			 X_y_by_col.add(colMatrix[j]);
		 }
		 return X_y_by_col;
	 }

	/** 
	 * This function tacks on the original index of each observation to a training data set
	 * tacked on at the end of the observations' vectors.
	 * 
	 * @param X_y_old	The original training data set
	 * @return			The training data set with indices tacked on.
	 */
	private ArrayList<double[]> addIndicesToDataMatrix(ArrayList<double[]> X_y_old) {
		ArrayList<double[]> X_y_new = new ArrayList<double[]>(n);
		for (int i = 0; i < n; i++){
			double[] x = new double[p + 2];
			for (int j = 0; j < p + 1; j++){
				x[j] = X_y_old.get(i)[j];
			}
			x[p + 1] = i;
			X_y_new.add(x);
//			System.out.println("row " + i + ": " + Tools.StringJoin(x));
		}
		return X_y_new;
	}

	/**
	 * This provides a vector of responses from the training data set
	 * 
	 * @param X_y	The training data set
	 * @return		The vector of responses
	 */
	private double[] extractResponseFromRawData(ArrayList<double[]> X_y) {
		double[] y = new double[X_y.size()];
		for (int i = 0; i < X_y.size(); i++){
			double[] record = X_y.get(i);
			y[i] = record[record.length - 1];
		}
		return y;
	}
	
	/** build the machine learning classifier (implemented by a daughter class), you must {@link #setData(ArrayList) set the data} first */
	public abstract void Build();
	
	/**
	 * Useful for debugging only. Undocumented.
	 * 
	 * For details see <a href="https://blogs.oracle.com/nickstephen/entry/java_redirecting_system_out_and_err">Redirecting System.out and System.err</a>.
	 */
	public void suppressOrWriteToDebugLog(){
		//also handle the logging
        LogManager logManager = LogManager.getLogManager();
        logManager.reset();

        // create log file, no limit on size
        FileHandler fileHandler = null;
		try {
			fileHandler = new FileHandler(unique_name + ".log", Integer.MAX_VALUE, 1, false);
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
        fileHandler.setFormatter(new SuperSimpleFormatter());
        Logger.getLogger("").addHandler(fileHandler);
        
        
        // now rebind stdout/stderr to logger
        Logger logger = Logger.getLogger("stdout");         
        LoggingOutputStream  los = new LoggingOutputStream(logger, StdOutErrLevel.STDOUT);
        System.setOut(new PrintStream(los, true));
        logger = Logger.getLogger("stderr");                                    
        los = new LoggingOutputStream(logger, StdOutErrLevel.STDERR);            
        System.setErr(new PrintStream(los, true)); 		
	}
	
	/** deletes all data that's unneeded at this point in runtime in order to save memory */
	protected abstract void FlushData();
	
	/**
	 * After the classifier has been built, new records can be evaluated / predicted
	 * (implemented by a daughter class)
	 * 
	 * @param record		The observation to be evaluated / predicted
	 * @param num_cores		The number of processor cores to be used during the evaluation / prediction
	 * @return				The prediction
	 */
	public abstract double Evaluate(double[] record, int num_cores);

	/**
	 * Evaluate multiple records at once. Subclasses should override this for performance.
	 * 
	 * @param records 		The observations to be evaluated
	 * @param num_cores 	The number of processor cores to use
	 * @return 				The predictions
	 */
	public double[] Evaluate(double[][] records, int num_cores) {
		double[] results = new double[records.length];
		for (int i = 0; i < records.length; i++) {
			results[i] = Evaluate(records[i], num_cores);
		}
		return results;
	}
	
	/**
	 * A wrapper for {@link #Evaluate(double[], int)} where one processor core is used
	 * 
	 * @param record		The observation to be evaluated / predicted
	 * @return				The prediction
	 */
	public double Evaluate(double[] record){
		return Evaluate(record, 1);
	}
	
	/**
	 * Given a data record, return the Y (response) value i.e. take the last index
	 * 
	 * @param record		the data record
	 * @return				the data record's response value (or class)
	 */
	public double getResponseFromRecord(double[] record){
		return record[p];
	}

	/** Stop the classifier during its building phase */
	public abstract void StopBuilding();

	/**
	 * How many features are in the training data set?
	 * 
	 * @return	The number of features in the training data set
	 */
	public int getP() {
		return p;
	}
	
	/**
	 * How many observations are in the training data set?
	 * 
	 * @return	The number of observations in the training data set
	 */
	public int getN() {
		return n;
	}	
	
	/** Useful for debugging. Undocumented */
	public void dumpDataToFile(String optional_title){
		PrintWriter out=null;
		try {
			out = new PrintWriter(new BufferedWriter(new FileWriter("data_out" + (optional_title == null ? "" : optional_title) + ".csv")));
		} catch (IOException e) {
			System.out.println("cannot be edited in CSV appending");
		}
		
		//print fileheader
		for (int j = 0; j < p; j++){
			out.print("," + j);
		}
		out.print(",y");
		out.print("\n");
		//now print the data
		for (int i = 0; i < n; i++){
			double[] record = X_y.get(i);
			for (int j = 0; j <= p; j++){
				out.print("," + record[j]);
			}
			out.print("\n");
		}
		out.close();		
	}
	
	/** a variable that represents the different error calculation types */
	public static enum ErrorTypes {L1, L2, MISCLASSIFICATION};
	
	/**
	 * Calculates the in-sample error using the specified loss function
	 * 
	 * @param num_cores_evaluate 	The number of processor cores to use
	 */		
	private void calculateInSampleResiduals(int num_cores_evaluate){
		long t0 = System.currentTimeMillis();
		System.out.print("calculating in-sample residuals...");
		in_sample_residuals = new double[n];
		for (int i = 0; i < n; i++){
			double[] record = X_y.get(i);
			double y = getResponseFromRecord(record);
			double yhat = Evaluate(record, num_cores_evaluate);
//			System.out.println("y: " + y + " yhat: " + yhat);
			in_sample_residuals[i] = y - yhat;
		}
		long t1 = System.currentTimeMillis();
		System.out.print("done in " + ((t1 - t0) / 1000.0) + " sec \n");
	}
	
	/**
	 * Calculates the in-sample error based on a specified error metric
	 * 
	 * @param type_of_error_rate	The error metric to use to compute loss
	 * @param num_cores_evaluate	The number of processor cores to use
	 * @return						The in-sample loss as a sum total across all training observations
	 */
	public double calculateInSampleLoss(ErrorTypes type_of_error_rate, int num_cores_evaluate){	
		if (in_sample_residuals == null){
			calculateInSampleResiduals(num_cores_evaluate);
		}
		
		double loss = 0;
		System.out.print("calculateInSampleLoss for " + type_of_error_rate + "...");
		for (int i = 0; i < n; i++){
			switch (type_of_error_rate){
				case L1:
					loss += Math.abs(in_sample_residuals[i]);
					break;
				case L2:
					loss += in_sample_residuals[i] * in_sample_residuals[i];
					break;
				case MISCLASSIFICATION:
					loss += (in_sample_residuals[i] == 0 ? 0 : 1);
					break;
			}
		}
		System.out.print("done\n");
//		System.out.println("in_sample_residuals: " + Tools.StringJoin(in_sample_residuals));
		return loss;
	}
	
	/**
	 * Transforms the response variable (implemented by a daughter class).
	 * The default here is to just save the original response.
	 */
	protected void transformResponseVariable() {
		y_trans = new double[y_orig.length];
		//default is to do nothing... ie just copy the y's into y_trans's
		for (int i = 0; i < n; i++){
			y_trans[i] = y_orig[i];
		}		
	}	

	/**
	 * Untransforms a response value (implemented by a daughter class).
	 * The default here is to just return the original response.
	 * 
	 * @param y_i	The value to untransform
	 * @return		The untransformed value
	 */
	protected double un_transform_y(double y_i) {
		return y_i;
	}		
	
	public Classifier clone(){
		return null;
	}
	
	public void setUniqueName(String unique_name) {
		this.unique_name = unique_name;
	}
	
	public void writeStdOutToLogFile(){
		try {
		  Logger.getLogger("").addHandler(new StreamHandler()); //turn off std out
		  suppressOrWriteToDebugLog();
		}
		catch (Error e){
			System.out.println("Logger and or suppressOrWriteToDebugLog FAILING\n");
		}    
 	}
}
