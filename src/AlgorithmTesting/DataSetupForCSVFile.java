/*
    BART - Bayesian Additive Regressive Trees
    Software for Supervised Statistical Learning
    
    Copyright (C) 2012 Professor Ed George & Adam Kapelner, 
    Dept of Statistics, The Wharton School of the University of Pennsylvania

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details:
    
    http://www.gnu.org/licenses/gpl-2.0.txt

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

package AlgorithmTesting;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;

import bartMachine.Classifier;


public class DataSetupForCSVFile {

	private final ArrayList<double[]> X_y;
	private int K;
	private boolean file_has_header;
//	private int p;
//	private ArrayList<FeatureType> feature_types;
//	private ArrayList<String> feature_names;
	
	public DataSetupForCSVFile(File file, boolean header) {
		X_y = new ArrayList<double[]>();
		this.file_has_header = header;
		try {
			LoadDataIntoXyFormatAndFindFeatureNamesAndP(file);
		} catch (IOException e) {
			e.printStackTrace();
		}
		extractNumClassesFromDataMatrix();
	}
	
	private void extractNumClassesFromDataMatrix() {
		HashSet<Double> set_of_classes = new HashSet<Double>();
		for (double[] record : X_y){
			double k = record[record.length - 1];
//			System.out.println("y = " + k);
			set_of_classes.add(k); //the last position is the response
		}
		K = set_of_classes.size();
//		System.out.println("num classes: " + K);
	}

	private void LoadDataIntoXyFormatAndFindFeatureNamesAndP(File file) throws IOException{		
		//begin by iterating over the file
		BufferedReader in = new BufferedReader(new FileReader(file));
		int line_num = 0;
		while (true){
			String datum = in.readLine();
			if (datum == null) {
				break;
			}
			String[] datums = datum.split(",");
//			p = datums.length - 1;
			

			if (line_num == 0 && file_has_header){
//				feature_types = new ArrayList<FeatureType>(p);
//				for (int i = 0; i < p; i++){
//					feature_types.add(FeatureType.NUMBER); //default for now
//				}
//				feature_names = new ArrayList<String>(p);
//				for (int i = 0; i < p; i++){
//					feature_names.add(datums[i]); //default for now
//					System.out.println("feature " + (i + 1) + " " + datums[i]);
//				}
			}
			else {
				final double[] record = new double[datums.length];
				for (int i = 0; i < datums.length; i++){
					try {
						record[i] = Double.parseDouble(datums[i]);
					} catch(NumberFormatException e){
						record[i] = Classifier.MISSING_VALUE;
					}
				}				
				X_y.add(record);
//				System.out.println("record: " + Tools.StringJoin(record, ", "));
			}
			line_num++;
		}
		in.close();
	}


	public int getK() {
		return K;
	}


	public ArrayList<double[]> getX_y() {
		return X_y;
	}	

}
