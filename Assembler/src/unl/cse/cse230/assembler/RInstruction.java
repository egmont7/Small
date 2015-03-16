package unl.cse.cse230.assembler;

import java.util.List;

public class RInstruction extends Instruction{
	private Condition condition;
	private int regs, regd, regt;
	
	
	@SuppressWarnings("unused")
	public RInstruction(String name, Condition con, List<String> args, int instructionNumber, String rawData){
		this.name = name;
		this.condition = con;
		this.arguments = args;
		this.instructionNumber = instructionNumber;
		this.rawData = rawData;
		encoding = new boolean[24];
		
		for(boolean b : encoding){
			b = false;
		}
		try{
		processArguments();
		} catch(InputException e){
			System.out.println(e.getLocalizedMessage());
		}
	}
	
	
	
	private void processArguments() throws InputException{

		if(arguments.get(arguments.size() -1).equals("s")){
			encoding[15] = true;
			arguments.remove(arguments.size() - 1);
		}

		
		if(name.equals("jr")){
			if(arguments.size() != 1){
				System.out.println("syntax error on line " + instructionNumber);
				System.out.println("\tWrong number of arguments:" + arguments.size());
				System.exit(1);
			}
			regd = 0;
			regs = parseRegister(arguments.get(0));
			regt = 0;
			
			encoding[23] = true;
			encoding[22] = false;
			encoding[21] = true;
			encoding[20] = true;
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

			regCode = int2binary(regd);
			encoding[6] = regCode[3];
			encoding[5] = regCode[2];
			encoding[4] = regCode[1];
			encoding[3] = regCode[0];

			encoding[2] = false;
			encoding[1] = false;
			encoding[0] = false;
			
		}
		else if(name.equals("sll")){
			if(arguments.size() != 3){
				System.out.println("syntax error on line " + instructionNumber);
				System.out.println("\tWrong number of arguments:" + arguments.size());
				System.exit(1);
			}
			regd = parseRegister(arguments.get(0));
			regs = parseRegister(arguments.get(1));
			regt = parseImmediate(arguments.get(2));
			
			encoding[23] = false;
			encoding[22] = true;
			encoding[21] = true;
			encoding[20] = true;
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

			regCode = int2binary(regd);
			encoding[6] = regCode[3];
			encoding[5] = regCode[2];
			encoding[4] = regCode[1];
			encoding[3] = regCode[0];

			encoding[2] = false;
			encoding[1] = false;
			encoding[0] = false;
		}
		else{
			if(arguments.size() != 3){
				System.out.println("syntax error on line " + instructionNumber);
				System.out.println("\tWrong number of arguments:" + arguments.size());
				for(String s : arguments){
					System.out.print("\"" + s + "\" " );
				}
				System.out.println();
				System.exit(1);
			}
			regd = parseRegister(arguments.get(0));
			regs = parseRegister(arguments.get(1));
			regt = parseRegister(arguments.get(2));

			encoding[23] = false;
			encoding[22] = false;
			encoding[21] = true;
			encoding[20] = true;
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

			regCode = int2binary(regd);
			encoding[6] = regCode[3];
			encoding[5] = regCode[2];
			encoding[4] = regCode[1];
			encoding[3] = regCode[0];

			if(name.equals("add"))
			{
				encoding[2] = false;
				encoding[1] = true;
				encoding[0] = false;
			}
			else if(name.equals("sub")){
				encoding[2] = false;
				encoding[1] = false;
				encoding[0] = false;
			}
			else if(name.equals("and")){
				encoding[2] = true;
				encoding[1] = true;
				encoding[0] = false;
			}
			else if(name.equals("or")){
				encoding[2] = true;
				encoding[1] = false;
				encoding[0] = true;
			}
			else if(name.equals("xor")){
				encoding[2] = true;
				encoding[1] = false;
				encoding[0] = false;
			}
		}
	}
	
	
}
