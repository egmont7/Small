/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.utils;

import konstruk.KonstrukSettings;

/**
 *
 * @author Caleb
 */
public class BlockLocator {
    
    public int x, y, z; 
    public byte i, j, k;
    public ChunkLocator chunkPos;
    
    public BlockLocator(byte chunkPosX, byte chunkPosZ, byte i, byte j, byte k){
        this.chunkPos = new ChunkLocator(chunkPosX, chunkPosZ);
        this.i = i;
        this.j = j;
        this.k = k;
        
        x = (chunkPosX*KonstrukSettings.CHUNK_X) + i;
        y = j;
        z = (chunkPosZ*KonstrukSettings.CHUNK_Z) + k;
    }
    
    public BlockLocator(Vector3I xyz){
        this(xyz.x,xyz.y,xyz.z);
    }
    
    public BlockLocator(int x, int y, int z){
        this.x = x;
        this.y = y;
        this.z = z;
        
        this.chunkPos = new ChunkLocator((byte)(x / KonstrukSettings.CHUNK_X), 
                                         (byte)(z / KonstrukSettings.CHUNK_Z));
        this.i = (byte)(x % KonstrukSettings.CHUNK_X);
        this.j = (byte)y;
        this.k = (byte)(z % KonstrukSettings.CHUNK_Z);
    }
    

}
