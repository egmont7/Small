/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.voxel;

import java.io.Serializable;
import konstruk.KonstrukSettings;
import konstruk.procedural.Landscape;
import konstruk.utils.ChunkLocator;

/**
 *
 * @author Caleb
 */
public class Chunk implements Serializable {

//    private boolean dirty = true;
//    private boolean render = false;
    
    private int chunkX = KonstrukSettings.CHUNK_X;
    private int chunkY = KonstrukSettings.CHUNK_Y;
    private int chunkZ = KonstrukSettings.CHUNK_Z;
    public final ChunkLocator cl;
    public final String name;
    
    public final int[][][] chunkData = new int[chunkX][chunkZ][chunkY];
    
    public Chunk(byte chunkPosX, byte chunkPosZ, boolean buildTerrain) {
        cl = new ChunkLocator(chunkPosX, chunkPosZ);
        name = getChunkName(chunkPosX, chunkPosZ);
        if(buildTerrain){
            Landscape.buildTerrain(this);
        }
    }
    
    
    @Override
    public String toString(){
        return name;
    }
    
    public static String getChunkName(int x, int z){
        return "Chunk_"+x+"_"+z;
    }
}
