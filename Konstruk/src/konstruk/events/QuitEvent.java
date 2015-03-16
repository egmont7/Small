/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.events;

import konstruk.utils.ChunkLocator;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class QuitEvent extends KonstrukEvent{

    @Override
    public Object run(VoxelWorld vw) {
        for(ChunkLocator cl : vw.getChunks()){
            vw.setChunkRenderable(cl.x, cl.z, false);
        }
//        LiveState.theLiveState.konstrukEventHandler.stop();
        return this;
    }
    
}
