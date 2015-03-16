/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.blocks;

/**
 *
 * @author Caleb
 */
public class BlockWater extends Block{
    public BlockWater(){
        name = "Water";
        resilience = 1.0f;
        isTranslucent = true;
        atlasLookups = new int[]{
            0x0006,  //-y
            0x0006,  //+y
            0x0006,  //-x
            0x0006,  //+x
            0x0006,  //-z
            0x0006}; //+z
    }
}
