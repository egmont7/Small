/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.blocks;

/**
 *
 * @author Caleb
 */
public class BlockGrass extends Block{
    public BlockGrass(){
        name = "Grass";
        resilience = 0.1f;
        atlasLookups = new int[]{
            0x0001,  //-y
            0x0002,  //+y
            0x0003,  //-x
            0x0003,  //+x
            0x0003,  //-z
            0x0003}; //+z
    }
    
    
}
