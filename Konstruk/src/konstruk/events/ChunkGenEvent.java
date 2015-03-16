/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.events;

import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class ChunkGenEvent extends KonstrukEvent{

    byte x, z;
    public ChunkGenEvent(byte x, byte z){
        this.x = x;
        this.z = z;
    }
    
    @Override
    public Object run(VoxelWorld vw) {
        vw.setChunkRenderable(x, z, true);
        return null;
    }
    
}
