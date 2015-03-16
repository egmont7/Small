package unl.cse.cse230.assembler;

public class Label {
	public String name;
	public int location;
	
	public Label(String name, int location){
		this.name = name;
		this.location = location;
	}
	
	public String toString(){
		return name + ": " + location;
	}
	
}
