package unl.cse.cse230.assembler;

import java.math.BigInteger;
import java.util.List;

abstract public class Instruction {
	protected String rawData;
	protected String name;
	protected List<String> arguments;
	protected boolean encoding[];
	protected int instructionNumber;
	protected boolean usesLabel;
	protected String label;

	public String toString() {
		String s = new String("");
		for (int i = 23; i >= 0; i--) {
			if (encoding[i]) {
				s = s + "1";
			} else {
				s = s + "0";
			}
		}
		return s;
	}

	public String getRawData() {
		return rawData;
	}

	public static boolean[] int2binary(int number) {
		String binaryString = Integer.toBinaryString(number);
		boolean code[] = new boolean[20];
		for (@SuppressWarnings("unused")
		boolean b : code) {
			b = false;
		}

		for (int i = 0; i < 20 && i < binaryString.length(); i++) {
			if (binaryString.charAt(binaryString.length() - i - 1) == '0') {
				code[i] = false;
			} else {
				code[i] = true;
			}
		}

		return code;
	}

	public void setImmediate(List<Label> labels) throws InputException {
	}
	public String toLongString(){
		return instructionNumber + ": " + rawData + " " + toString() + " " + label;
	}

	protected int parseRegister(String reg) throws InputException {
		try {
			if (reg.charAt(0) == '$') {
				reg = reg.substring(2);
				return Integer.parseInt(reg);
			}
		} catch (Exception e) {
			throw new InputException("Input Exception", reg, instructionNumber);
		}
		return 0;
	}

	protected int parseImmediate(String immediate) throws InputException {
		try {
			if (immediate.length() > 2
					&& immediate.substring(0, 2).equals("0x")) {
				usesLabel = false;
				BigInteger bImm = new BigInteger(immediate.substring(2), 16);
				return bImm.intValue();
			} else if (immediate.length() > 2
					&& immediate.substring(0, 2).equals("0b")) {
				usesLabel = false;
				BigInteger bImm = new BigInteger(immediate.substring(2), 2);
				return bImm.intValue();
			} else if (immediate.length() > 0 && Character.isDigit(immediate.charAt(0)) ||
					   (immediate.length() > 1 && immediate.charAt(0) == '-' && Character.isDigit(immediate.charAt(1)))) {
				usesLabel = false;
				return Integer.parseInt(immediate);
			} else {
				usesLabel = true;
				label = immediate;
				return 0;
			}

		} catch (NumberFormatException e) {
			System.out.println(e.getLocalizedMessage());
			throw new InputException("Syntax Error ", immediate,
					instructionNumber);
		}

	}

}
