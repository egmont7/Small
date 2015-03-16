/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.utils;

import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.scene.Geometry;

/**
 *
 * @author Caleb
 */
public class ChunkUpdateInfo {
    public Geometry geo;
    public RigidBodyControl control;
    
    public ChunkUpdateInfo(Geometry geo, RigidBodyControl control){
        this.geo = geo;
        this.control = control;
    }
}
