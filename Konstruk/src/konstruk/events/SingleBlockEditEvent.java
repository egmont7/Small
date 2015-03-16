/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.events;

import konstruk.utils.Vector3I;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class SingleBlockEditEvent extends KonstrukEvent {
    
    {
        priority = 2;
    }
    
    private Vector3I blockLocator;
    private int blockData;
    
    public SingleBlockEditEvent(Vector3I blockLocator, int blockData){
        this.blockLocator = blockLocator;
        this.blockData = blockData;
    }
    public Object run(VoxelWorld vw){
        vw.setBlockData(blockLocator, blockData);
        return null;
    }
}
