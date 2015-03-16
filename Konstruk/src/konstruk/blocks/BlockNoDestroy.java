/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.blocks;

/**
 *
 * @author Caleb
 */
public class BlockNoDestroy extends Block{
    public BlockNoDestroy(){
        name = "No Destroy";
        resilience = 1f;
        atlasLookups = new int[]{
            0x0005,  //-y
            0x0005,  //+y
            0x0005,  //-x
            0x0005,  //+x
            0x0005,  //-z
            0x0005}; //+z
    }
}
