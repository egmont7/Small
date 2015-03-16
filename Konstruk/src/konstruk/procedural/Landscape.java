/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.procedural;

import konstruk.KonstrukSettings;
import konstruk.blocks.BlockManager;
import konstruk.voxel.Chunk;

/**
 *
 * @author Caleb
 */
public class Landscape {
    
    public static void buildTerrain(Chunk c){

        
        int air = BlockManager.manager.getBlock("Air").id;
        int stone = BlockManager.manager.getBlock("Stone").id;
        int grass = BlockManager.manager.getBlock("Grass").id;
        int water = BlockManager.manager.getBlock("Water").id;
        int noDestroy = BlockManager.manager.getBlock("No Destroy").id;
        
        
        int chunkX = KonstrukSettings.CHUNK_X;
        int chunkY = KonstrukSettings.CHUNK_Y;
        int chunkZ = KonstrukSettings.CHUNK_Z;
        int[][][] chunkData = c.chunkData;
        
        int x = c.cl.x * chunkX;
        int z = c.cl.z * chunkZ;
        final double scale1 = 0.01;
        final double scale2 = 0.05;
        final double scale3 = 0.005;
        final double severity1 = 30;
        final double severity2 = 3;
        for (int i = 0; i < chunkX; i++) {
            for (int k = 0; k < chunkZ; k++) {
                int height = (int) ((SimplexNoise.noise((x + i) * scale1, (z + k) * scale1) + 1) * severity1);
                height += (int) ((SimplexNoise.noise((x + i) * scale2, (z + k) * scale2) + 1) * severity2);
                height = (int) (height * (SimplexNoise.noise((x + i) * scale3, (z + k) * scale3) + 1) * 0.5);
                for (int j = 0; j < chunkY; j++) {

                    if (j == height) {
                        chunkData[i][k][j] = grass;
                    } else if (j < height && j != 0) {
                        chunkData[i][k][j] = stone;
                    } else if (j == 0) {
                        chunkData[i][k][j] = noDestroy;
                    } else {
                        chunkData[i][k][j] = air;
                    }
//                    if(j < 7 && chunkData[i][k][j] == 0){
//                        chunkData[i][k][j] = water;
//                    }
                    chunkData[i][k][j] = chunkData[i][k][j] | 0xFE000;
                }
            }
        }
    }
}
