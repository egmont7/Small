package unl.cse.cse230.assembler;

import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.Scanner;

public class Assembler {

	private List<Label> labels;
	private List<Instruction> instructions;

	public Assembler(String inputFile) {
		labels = new ArrayList<Label>();
		instructions = new ArrayList<Instruction>();
		InstructionSet is = new InstructionSet();

		try {
			System.out.println("Attempting to open file:" + inputFile);
			Scanner s = new Scanner(new FileReader(inputFile));
			int instructionNumber = 0;
			while (s.hasNextLine()) {
				String line = s.nextLine();
				// System.out.println("line: " + line);
				List<String> tokens = tokenize(line);
				if (tokens.size() == 0) {
					continue;
				}

				/*
				 * Check to see if the first token is a label, it would be
				 * defined by *:
				 */
				if (tokens.get(0).endsWith(":")) {
					String labelName = tokens.get(0).substring(0,
							tokens.get(0).length() - 1);
					tokens.remove(0);
					labels.add(new Label(labelName, instructionNumber));
				}
				if (tokens.size() == 0) {
					continue;
				}

				/*
				 * Extract first token (instruction name/condition)
				 */
				String iName = tokens.remove(0);

				/*
				 * Extract condition if one exists.
				 */
				String condition = "";
				for (int i = 0; i < iName.length() - 1; i++) {
					if (iName.charAt(i) == '.') {
						condition = iName.substring(i + 1);
						iName = iName.substring(0, i);
					}
				}
				if (condition.equals("")) { // Default condition is
					// "always execute"
					condition = "al";
				}
				Condition cond = is.getConditionWithName(condition);
				char type = is.getInstructionType(iName);
				switch (type) {

				case 'r':
					instructions.add(new RInstruction(iName, cond, tokens,
							instructionNumber, line));
					break;
				case 'd':
					instructions.add(new DInstruction(iName, cond, tokens,
							instructionNumber, line));
					break;
				case 'b':
					instructions.add(new BInstruction(iName, cond, tokens,
							instructionNumber, line));
					break;
				case 'j':
					instructions.add(new JInstruction(iName, tokens,
							instructionNumber, line));
					break;
				case '!':
					instructions.add(new Data(tokens, instructionNumber, line));
					break;
				default:
					System.out.println("Syntax error on instruction "
							+ instructionNumber);
					System.out.println("Unrecognized instruction: " + iName);
					System.exit(0);
				}
				instructionNumber++;

			}
		} catch (IOException e) {
			System.out.println("File not found.");
			System.exit(0);
		}
		try {
			// System.out.println("LABELS:");
			// System.out.println(labels);
			//
			// for(Instruction i : instructions){
			// System.out.println(i.toLongString());
			// }

			for (Instruction i : instructions) {
				// System.out.println(i.toLongString());
				i.setImmediate(labels);
			}
		} catch (InputException e) {
			System.out.println(e.getLocalizedMessage());
			System.exit(0);
		}
	}

	public String getOutputPlain() {
		String output = "";
		for (int i = 0; i < instructions.size(); i++) {
			output += instructions.get(i).toString() + "\n";
		}
		return output;
	}

	public String getOutputmif(String title) {
		String output = "";

		output += "-- AUTOGENERATED MIF FROM \"" + title + "\"\n";
		DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
		Calendar cal = Calendar.getInstance();
		output += "-- DATE: " + dateFormat.format(cal.getTime()) + "\n\n";
		output += "WIDTH=24; \n";
		output += "DEPTH=1024; \n\n";
		output += "ADDRESS_RADIX=UNS;\n";
		output += "DATA_RADIX=BIN; \n\n";
		output += "CONTENT_BEGIN \n";

		for (int i = 0; i < instructions.size(); i++) {
			output += "\t" + i + "   :   " + instructions.get(i).toString()
					+ "; -- " + instructions.get(i).getRawData() + "\n";
		}
		output += "\t[" + instructions.size() + ".."
				+ "1023]   :   000000000000000000000000;\n";
		output += "END;\n";
		return output;
	}

	public static List<String> tokenize(String rawData) {
		List<String> tokens = new ArrayList<String>();
		String s[] = rawData.split("\\s+");
		tokens.addAll(Arrays.asList(s));

		if (tokens.size() > 0 && tokens.get(0).equals("")) {
			tokens.remove(0);
		}

		/*
		 * Remove any comments
		 */
		for (int i = 0; i < tokens.size(); i++) {
			for (int j = 0; j < tokens.get(i).length(); j++) {
				if (tokens.get(i).charAt(j) == '#') {
					tokens.set(i, tokens.get(i).substring(0, j));
					int numberToRemove = tokens.size() - i;
					for (int k = 0; k < numberToRemove; k++) {
						tokens.remove(tokens.size() - 1);
					}
					break;
				}
			}
		}
		return tokens;

	}

	public static void main(String[] args) {
		String inputFilename = "";
		String outputFilename = "";
		boolean mifFormat = false;

		if (args.length == 0 || args[0].equals("-h")) {
			System.out
					.println("Thank you for using this assembler! You're the coolest...");
			System.out
					.println("Please send questions and bug reports to cfangmeier74@gmail.com");
			System.out.println("Usage:");
			System.out
					.println("java -jar Assembler [-mif] -in sourcefile.s [-out outputFile.mif]");
			System.out
					.println("\t -mif : Use Quartus Memory Initialization File format for output");
			System.out
					.println("\t -in : Specify name of assembly source input file.");
			System.out
					.println("\t -out : Specify file to write output to. Otherwise output goes to terminal.");
			System.exit(0);
		}

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-in")) {
				inputFilename = args[i + 1];
				i++;
			} else if (args[i].equals("-out")) {
				outputFilename = args[i + 1];
				i++;
			} else if (args[i].equals("-mif")) {
				mifFormat = true;
			} else {
				System.out.println("Unrecognized option:" + args[i]);
				System.exit(0);
			}
		}

		Assembler assembler = new Assembler(inputFilename);

		String output;
		if (mifFormat) {
			output = assembler.getOutputmif(inputFilename);
		} else {
			output = assembler.getOutputPlain();
		}

		if (outputFilename.equals("")) {
			System.out.print(output);
		} else {
			try {
				// Create file
				FileWriter fstream = new FileWriter(outputFilename);
				BufferedWriter out = new BufferedWriter(fstream);
				out.write(output);
				// Close the output stream
				out.close();
			} catch (Exception e) {// Catch exception if any
				System.out.println("Error: " + e.getMessage());
			}
		}

		System.out.println("finished! Goodbye.");
	}
}