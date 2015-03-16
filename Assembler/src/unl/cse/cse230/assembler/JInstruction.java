package unl.cse.cse230.assembler;

import java.util.List;

public class JInstruction extends Instruction {
	private int immediate;
	private int regd;

	@SuppressWarnings("unused")
	public JInstruction(String name, List<String> args, int instructionNumber,
			String rawData) {
		this.name = name;
		this.arguments = args;
		this.instructionNumber = instructionNumber;
		this.rawData = rawData;
		encoding = new boolean[24];

		for (boolean b : encoding) {
			b = false;
		}
		try {
			processArguments();
		} catch (InputException e) {
			System.out.println(e.getLocalizedMessage());
		}
	}

	public void setImmediate(List<Label> labels) throws InputException {
		if (usesLabel) {
			boolean foundLabel = false;
			for (Label l : labels) {
				if (l.name.equals(label)) {
					foundLabel = true;
					immediate = l.location;
					boolean b[] = int2binary(l.location);

					encoding[19] = b[19];
					encoding[18] = b[18];
					encoding[17] = b[17];
					encoding[16] = b[16];
					encoding[15] = b[15];
					encoding[14] = b[14];
					encoding[13] = b[13];
					encoding[12] = b[12];
					encoding[11] = b[11];
					encoding[10] = b[10];
					encoding[9] = b[9];
					encoding[8] = b[8];
					encoding[7] = b[7];
					encoding[6] = b[6];
					encoding[5] = b[5];
					encoding[4] = b[4];
					encoding[3] = b[3];
					encoding[2] = b[2];
					encoding[1] = b[1];
					encoding[0] = b[0];
					break;
				}
			}
			if (!foundLabel) {
				throw new InputException("Undefined Label ", label, instructionNumber);
			}

		}
	}

	private void encodeImmediate() {
		boolean b[] = int2binary(immediate);

		encoding[19] = b[19];
		encoding[18] = b[18];
		encoding[17] = b[17];
		encoding[16] = b[16];
		encoding[15] = b[15];
		encoding[14] = b[14];
		encoding[13] = b[13];
		encoding[12] = b[12];
		encoding[11] = b[11];
		encoding[10] = b[10];
		encoding[9] = b[9];
		encoding[8] = b[8];
		encoding[7] = b[7];
		encoding[6] = b[6];
		encoding[5] = b[5];
		encoding[4] = b[4];
		encoding[3] = b[3];
		encoding[2] = b[2];
		encoding[1] = b[1];
		encoding[0] = b[0];
	}

	private void processArguments() throws InputException {

		if (arguments.get(arguments.size() - 1).equals("s")) {
			throw new InputException("syntax error",
					"J-Type instructions cannot use set bit", instructionNumber);
		}

		if (name.equals("li")) {
			if (arguments.size() != 2) {
				throw new InputException("syntax error",
						"wrong number of arguments", instructionNumber);
			}
			regd = parseRegister(arguments.get(0));
			immediate = parseImmediate(arguments.get(1));
			if(!usesLabel){
				encodeImmediate();
			}
			boolean regdB[] = int2binary(regd);
			encoding[23] = false;
			encoding[22] = true;
			encoding[21] = false;
			encoding[20] = false;
			encoding[19] = regdB[3];
			encoding[18] = regdB[2];
			encoding[17] = regdB[1];
			encoding[16] = regdB[0];
		} else if (name.equals("j")) {
			if (arguments.size() != 1) {
				throw new InputException("syntax error",
						"wrong number of arguments", instructionNumber);
			}
			immediate = parseImmediate(arguments.get(0));
			if(!usesLabel){
				encodeImmediate();
			}
			encoding[23] = false;
			encoding[22] = false;
			encoding[21] = false;
			encoding[20] = false;
		} else if (name.equals("jal")) {
			if (arguments.size() != 1) {
				throw new InputException("syntax error",
						"wrong number of arguments", instructionNumber);
			}
			immediate = parseImmediate(arguments.get(0));
			if(!usesLabel){
				encodeImmediate();
			}
			encoding[23] = true;
			encoding[22] = false;
			encoding[21] = false;
			encoding[20] = false;
		}
	}
}
