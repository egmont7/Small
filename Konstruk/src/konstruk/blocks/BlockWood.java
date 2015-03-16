/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.blocks;

/**
 *
 * @author Caleb
 */
public class BlockWood extends Block{
    
    
    public BlockWood(){
        name = "Wood";
        resilience = 0.5f;
        atlasLookups = new int[]{
            0x0005,  //-y
            0x0005,  //+y
            0x0004,  //-x
            0x0004,  //+x
            0x0004,  //-z
            0x0004}; //+z
    }
}
