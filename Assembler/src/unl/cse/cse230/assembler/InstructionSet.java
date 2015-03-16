package unl.cse.cse230.assembler;

import java.util.ArrayList;
import java.util.List;

public class InstructionSet {

	private List<InstructionType> instructions;
	private List<Condition> conditions;
	
	public InstructionSet(){
		instructions = new ArrayList<InstructionType>();
		conditions = new ArrayList<Condition>();
		defineConditions();
		defineInstructions();
	}
	
	
	
	public Condition getConditionWithName(String name){
		for(int i = 0; i < conditions.size(); i++){
			if(conditions.get(i).conditionLabel.equals(name)){
				return conditions.get(i);
			}
		}
		return null;
	}

	public char getInstructionType(String name) {

		for(int i = 0; i < instructions.size(); i++){
			if(instructions.get(i).name.equals(name)){
				return instructions.get(i).type;
			}
		}
		return 0;
	}
	
	
	
	private void defineConditions(){
		//CODE: "eq" 0
		boolean code[] = new boolean[4];
		code[0]=false; code[1]=false; code[2]=false; code[3] = false;
		conditions.add(new Condition("eq", code));
		
		//CODE: "ne" 1
		code[0]=false; code[1]=false; code[2]=false; code[3] = true;
		conditions.add(new Condition("ne", code));

		//CODE: "hs/cs" 2
		code[0]=false; code[1]=false; code[2]=true; code[3] = false;
		conditions.add(new Condition("hs", code));

		//CODE: "lo" 3
		code[0]=false; code[1]=false; code[2]=true; code[3] = true;
		conditions.add(new Condition("lo", code));

		//CODE: "mi" 4
		code[0]=false; code[1]=true; code[2]=false; code[3] = false;
		conditions.add(new Condition("mi", code));

		//CODE: "pl" 5
		code[0]=false; code[1]=true; code[2]=false; code[3] = true;
		conditions.add(new Condition("pl", code));

		//CODE: "vs" 6
		code[0]=false; code[1]=true; code[2]=true; code[3] = false;
		conditions.add(new Condition("vs", code));

		//CODE: "vc" 7
		code[0]=false; code[1]=true; code[2]=true; code[3] = true;
		conditions.add(new Condition("vc", code));

		//CODE: "hi" 8
		code[0]=true; code[1]=false; code[2]=false; code[3] = false;
		conditions.add(new Condition("hi", code));

		//CODE: "ls" 9
		code[0]=true; code[1]=false; code[2]=false; code[3] = true;
		conditions.add(new Condition("ls", code));

		//CODE: "ge" 10
		code[0]=true; code[1]=false; code[2]=true; code[3] = false;
		conditions.add(new Condition("ge", code));

		//CODE: "lt" 11
		code[0]=true; code[1]=false; code[2]=true; code[3] = true;
		conditions.add(new Condition("lt", code));

		//CODE: "gt" 12
		code[0]=true; code[1]=true; code[2]=false; code[3] = false;
		conditions.add(new Condition("gt", code));

		//CODE: "le" 13
		code[0]=true; code[1]=true; code[2]=false; code[3] = true;
		conditions.add(new Condition("le", code));

		//CODE: "al" 14
		code[0]=true; code[1]=true; code[2]=true; code[3] = false;
		conditions.add(new Condition("al", code));

		//CODE: "nv" 15
		code[0]=true; code[1]=true; code[2]=true; code[3] = true;
		conditions.add(new Condition("nv", code));
	}
	
	private void defineInstructions(){
		//R-Type Instructions
		instructions.add(new InstructionType('r', "add"));

		instructions.add(new InstructionType('r', "sub"));

		instructions.add(new InstructionType('r', "and"));

		instructions.add(new InstructionType('r', "or"));

		instructions.add(new InstructionType('r', "xor"));

		instructions.add(new InstructionType('r', "sll"));

		instructions.add(new InstructionType('r', "jr"));

		//D-type Instructions
		instructions.add(new InstructionType('d', "lw"));

		instructions.add(new InstructionType('d', "sw"));

		instructions.add(new InstructionType('d', "addi"));
		
		instructions.add(new InstructionType('d', "lwio"));

		instructions.add(new InstructionType('d', "swio"));
		

		//B-type Instructions
		instructions.add(new InstructionType('b', "b"));

		instructions.add(new InstructionType('b', "bal"));

		//J-type Instructions
		instructions.add(new InstructionType('j', "j"));

		instructions.add(new InstructionType('j', "jal"));

		instructions.add(new InstructionType('j', "li"));
		
		//Data
		instructions.add(new InstructionType('!', "!data"));
		
		
	}
	
}
