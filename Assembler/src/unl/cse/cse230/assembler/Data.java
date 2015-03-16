package unl.cse.cse230.assembler;

import java.util.List;

public class Data extends Instruction {
	private int immediate;

	public Data( List<String> args, int instructionNumber,
			String rawData) {
		this.arguments = args;
		this.instructionNumber = instructionNumber;
		this.rawData = rawData;
		encoding = new boolean[24];

		try {
			processArguments();
		} catch (InputException e) {
			System.out.println(e.getLocalizedMessage());
		}
	}



	private void processArguments() throws InputException {


			if (arguments.size() != 1) {
				throw new InputException("syntax error",
						"wrong number of arguments", instructionNumber);
			}
			immediate = parseImmediate(arguments.get(0));
			
			if(usesLabel){
				throw new InputException("syntax error",
						"!data directives must have numerical arguments", instructionNumber);
			}
			
			boolean immB[] = int2binary(immediate);
			
			for(int i = 0; i < 24 && i < immB.length; i++){
				encoding[i] = immB[i];
			}
			
	}
}
