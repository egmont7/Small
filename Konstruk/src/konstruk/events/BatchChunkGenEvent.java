/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.events;

import java.util.List;
import konstruk.utils.ChunkLocator;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class BatchChunkGenEvent extends KonstrukEvent{

    List<ChunkLocator> chunks;
    public BatchChunkGenEvent(List<ChunkLocator> chunks){
        this.chunks = chunks;
    }
    
    @Override
    public Object run(VoxelWorld vw) {
        for(ChunkLocator chunk : chunks){
            vw.setChunkRenderable(chunk.x, chunk.z, true);
        }
        return null;
    }
    
}