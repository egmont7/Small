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
public abstract class KonstrukEvent {
    public static int priority; // 1-9 , 1 highest, 9 lowest
    public abstract Object run(VoxelWorld vw);
    
    
}
