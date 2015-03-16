/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.blocks;

/**
 *
 * @author Caleb
 */
public class BlockStone extends Block{
    public BlockStone(){
        name = "Stone";
        resilience = 0.8f;
        atlasLookups = new int[]{
            0x0000,  //-y
            0x0000,  //+y
            0x0000,  //-x
            0x0000,  //+x
            0x0000,  //-z
            0x0000}; //+z
    }
}
