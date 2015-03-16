package unl.cse.cse230.assembler;


public class Condition {
	public String conditionLabel;
	public boolean[] conditionCode;
	
	public Condition(String conditionLabel, boolean[] conditionCode){
		this.conditionLabel = conditionLabel;
		this.conditionCode = new boolean[4];
		this.conditionCode[0] = conditionCode[0];
		this.conditionCode[1] = conditionCode[1];
		this.conditionCode[2] = conditionCode[2];
		this.conditionCode[3] = conditionCode[3];
	}
	
}
