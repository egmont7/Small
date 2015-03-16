/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.events;

import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;
import konstruk.entities.Player;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class PlayerCreationEvent extends KonstrukEvent{
    {
        priority = 1;
    }
    
    private Camera cam;
    private Vector3f location;
    private String name;
    
    public PlayerCreationEvent(Camera cam, Vector3f location, String name){
        this.cam = cam;
        this.location = location;
        this.name = name;
    }
    
    @Override
    public Object run(VoxelWorld vw) {
        return new Player(cam, location, name);
    }
    
}
