package unl.cse.cse230.assembler;

public class InputException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String errorType, description;
	private int lineNumber;
	
	
	public InputException(String errorType,String description,  int lineNumber){
		super();
		this.errorType = errorType;
		this.description = description;
		this.lineNumber = lineNumber;
	}


	@Override
	public String getLocalizedMessage() {
		return errorType + "at Instruction " + lineNumber + ": " + description ;
	}
	
	
	
	
	
}
