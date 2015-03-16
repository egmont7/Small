package unl.cse.cse230.assembler;

import java.util.List;

public class DInstruction extends Instruction {
	private Condition condition;
	private int regs, regt, immediate;

	@SuppressWarnings("unused")
	public DInstruction(String name, Condition con, List<String> args,
			int instructionNumber, String rawData) {
		this.name = name;
		this.condition = con;
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

					boolean b[] = int2binary(l.location);

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
				throw new InputException("Undefined Label ", label,
						instructionNumber);
			}

		}
	}

	private void processArguments() throws InputException {

		if (arguments.get(arguments.size() - 1).equals("s")) {
			encoding[15] = true;
			arguments.remove(arguments.size() - 1);
		}

		if (arguments.size() != 3) {
			throw new InputException("syntax error",
					"wrong number of arguments", instructionNumber);
		}

		regs = parseRegister(arguments.get(0));
		regt = parseRegister(arguments.get(1));
		immediate = parseImmediate(arguments.get(2));

		if (name.equals("lw")) {
			encoding[23] = false;
			encoding[22] = false;
			encoding[21] = true;
			encoding[20] = false;
		} else if (name.equals("sw")) {
			encoding[23] = false;
			encoding[22] = true;
			encoding[21] = true;
			encoding[20] = false;
		} else if (name.equals("addi")) {
			encoding[23] = true;
			encoding[22] = false;
			encoding[21] = true;
			encoding[20] = false;
		} else if (name.equals("swio")){
			encoding[23] = false;
			encoding[22] = true;
			encoding[21] = true;
			encoding[20] = false;
		} else if (name.equals("lwio")){
			encoding[23] = true;
			encoding[22] = true;
			encoding[21] = true;
			encoding[20] = false;
		}

		encoding[19] = condition.conditionCode[0];
		encoding[18] = condition.conditionCode[1];
		encoding[17] = condition.conditionCode[2];
		encoding[16] = condition.conditionCode[3];

		boolean regCode[] = int2binary(regs);
		encoding[14] = regCode[3];
		encoding[13] = regCode[2];
		encoding[12] = regCode[1];
		encoding[11] = regCode[0];

		regCode = int2binary(regt);
		encoding[10] = regCode[3];
		encoding[9] = regCode[2];
		encoding[8] = regCode[1];
		encoding[7] = regCode[0];

		if (!usesLabel) {
			regCode = int2binary(immediate);
			encoding[6] = regCode[6];
			encoding[5] = regCode[5];
			encoding[4] = regCode[4];
			encoding[3] = regCode[3];
			encoding[2] = regCode[2];
			encoding[1] = regCode[1];
			encoding[0] = regCode[0];
		}

	}

}
