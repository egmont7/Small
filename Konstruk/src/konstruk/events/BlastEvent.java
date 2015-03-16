/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.events;

import konstruk.blocks.Block;
import konstruk.blocks.BlockManager;
import konstruk.utils.Vector3I;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class BlastEvent extends KonstrukEvent {
    
    {
        priority = 3;
    }
    private Vector3I center;
    private float radius, magnitude;
    
    public BlastEvent(Vector3I center, float radius, float magnitude){
        this.center = center;
        this.radius = radius;
        this.magnitude = magnitude;
    }
    
    public Object run(VoxelWorld vw){
        float radius2 = radius*radius;
        int xMin = (int)Math.max(center.x - radius,0);
        int xMax = (int)(center.x+radius);
        int yMin = (int)Math.max(center.y - radius,0);
        int yMax = (int)(center.y+radius);
        int zMin = (int)Math.max(center.z - radius,0);
        int zMax = (int)(center.z+radius);
        for(int i = xMin; i < xMax; i++){
            float i2 = (float)Math.pow(i-center.x, 2);
            for(int j = yMin; j < yMax; j++){
                float j2 = (float) Math.pow(j-center.y, 2);
                for(int k = zMin; k < zMax; k++){
                    Vector3I pos = new Vector3I(i,j,k);
                    int type = vw.getBlockType(pos);
                    float dist = (float)(i2+j2+Math.pow(k-center.z, 2));
                    if(dist < radius2 && type != 0){
                        int strength = vw.getBlockStrength(pos);
                        float resilience = BlockManager.manager.getBlock(type).resilience;
                        strength -= (int)(magnitude * Math.pow(dist, -1.5) * (1f-resilience));
                        vw.setBlockStrength(pos, strength);
                        if(strength <= 0){
                            vw.setBlockData(pos, 0x0);
                        }
                    }
                }
            }
        }
        return null;
    }
}
