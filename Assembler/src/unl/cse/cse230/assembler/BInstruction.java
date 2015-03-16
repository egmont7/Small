package unl.cse.cse230.assembler;

import java.util.List;

public class BInstruction extends Instruction {
	private Condition condition;
	private int immediate;
	
	
	@SuppressWarnings("unused")
	public BInstruction(String name, Condition con, List<String> args, int instructionNumber, String rawData){
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
	
	
	public void setImmediate(List<Label> labels) throws InputException{
		if(usesLabel){
			boolean foundLabel = false;
			for(Label l : labels){
				if(l.name.equals(label)){
					foundLabel = true;
					
					boolean b[] = int2binary(l.location - (instructionNumber + 1));
					
					encoding[15] = b[15];
					encoding[14] = b[14];
					encoding[13] = b[13];
					encoding[12] = b[12];
					encoding[11] = b[11];
					encoding[10] = b[10];
					encoding[9]  = b[9];
					encoding[8]  = b[8];
					encoding[7]  = b[7];
					encoding[6]  = b[6];
					encoding[5]  = b[5];
					encoding[4]  = b[4];
					encoding[3]  = b[3];
					encoding[2]  = b[2];
					encoding[1]  = b[1];
					encoding[0]  = b[0];
					break;
				}
			}
			if(!foundLabel){
				throw new InputException("Undefined Label ", label, instructionNumber);
			}
			
		}
	}
	
	private void processArguments() throws InputException{

		if(arguments.get(arguments.size() -1).equals("s")){
			throw new InputException("syntax error", "B-Type instructions cannot use set bit", instructionNumber);
		}
		
		if(arguments.size() != 1){
			throw new InputException("syntax error", "wrong number of arguments", instructionNumber);
		}
		
		immediate = parseImmediate(arguments.get(0));
		
		if(name.equals("b")){
			encoding[23] = false;
			encoding[22] = false;
			encoding[21] = false;
			encoding[20] = true;
		}
		else if(name.equals("bal")){
			encoding[23] = true;
			encoding[22] = false;
			encoding[21] = false;
			encoding[20] = true;
		}		
		
		encoding[19] = condition.conditionCode[0];
		encoding[18] = condition.conditionCode[1];
		encoding[17] = condition.conditionCode[2];
		encoding[16] = condition.conditionCode[3];


		if(!usesLabel){
			boolean b[] = int2binary(immediate);
			encoding[15] = b[15];
			encoding[14] = b[14];
			encoding[13] = b[13];
			encoding[12] = b[12];
			encoding[11] = b[11];
			encoding[10] = b[10];
			encoding[9]  = b[9];
			encoding[8]  = b[8];
			encoding[7]  = b[7];
			encoding[6]  = b[6];
			encoding[5]  = b[5];
			encoding[4]  = b[4];
			encoding[3]  = b[3];
			encoding[2]  = b[2];
			encoding[1]  = b[1];
			encoding[0]  = b[0];
		}
		
	}
}
