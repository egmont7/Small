/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.events;

import konstruk.KonstrukSettings;
import konstruk.utils.ChunkLocator;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class ReloadWorldEvent extends KonstrukEvent {

    private int x, z;
    
    public ReloadWorldEvent(int x, int z) {
        this.x = x;
        this.z = z;
    }
    
    @Override
    public Object run(VoxelWorld vw){
        
        for(ChunkLocator cl : vw.getChunks()){
            vw.setChunkRenderable(cl.x, cl.z, false);
        }
        for(int i = 0; i <= KonstrukSettings.VIEW_DISTANCE; i++){
            loadPerimeter(vw,i);
        }
        return null;
    }
    
    private void loadPerimeter(VoxelWorld vw, int dist){
        if(dist == 0){
            vw.setChunkRenderable((byte)x, (byte)z, true);
            return;
        }
        byte xStart, zStart;
        
        xStart = (byte)x;
        zStart = (byte)(z - dist);
        for(int i = 0; i < dist; i++){
            vw.setChunkRenderable(xStart,zStart, true);
            xStart++;
            zStart++;
        }
        for(int i = 0; i < dist; i++){
            vw.setChunkRenderable(xStart,zStart, true);
            xStart--;
            zStart++;
        }
        for(int i = 0; i < dist; i++){
            vw.setChunkRenderable(xStart,zStart, true);
            xStart--;
            zStart--;
        }
        for(int i = 0; i < dist; i++){
            vw.setChunkRenderable(xStart,zStart, true);
            xStart++;
            zStart--;
        }
    }
    
}
