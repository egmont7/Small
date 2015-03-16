package konstruk.entities;

import com.jme3.bullet.collision.shapes.CapsuleCollisionShape;
import com.jme3.bullet.control.CharacterControl;
import com.jme3.math.Quaternion;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;

/**
 *
 * @author Caleb
 */
public class Player {
    
    private CharacterControl playerControl;
    private boolean left = false, right = false, up = false, down = false;
    private Vector3f walkDirection = new Vector3f();
    private float[] prevCamRotation = new float[3];
    private Camera cam;
    private String name;
    
    public Player(Camera cam, Vector3f location, String name){
        this.cam = cam;
        this.name = name;
        cam.getRotation().toAngles(prevCamRotation);
        cam.setFrustumPerspective(45f, (float) cam.getWidth() / cam.getHeight(), 0.1f, 1000f);
        CapsuleCollisionShape capsuleShape = new CapsuleCollisionShape(0.4f, 1.7f, 1);
        playerControl = new CharacterControl(capsuleShape, 0.05f);
        playerControl.setJumpSpeed(10);
        playerControl.setFallSpeed(40);
        playerControl.setGravity(30);
        playerControl.setPhysicsLocation(new Vector3f(5, 80, 5));
    }
    
    
    public CharacterControl getPlayerControl(){
        return this.playerControl;
    }
    
    public void setLeft(boolean left){this.left = left;}
    public void setRight(boolean right){this.right = right;}
    public void setUp(boolean up){this.up = up;}
    public void setDown(boolean down){this.down = down;}
    
    public void jump(){playerControl.jump();}
    
    
    public Vector3f getPosition(){
        return playerControl.getPhysicsLocation();
    }
    
    public void setPosition(Vector3f pos){
        playerControl.setPhysicsLocation(pos);
    }
    
    public void update(){
        float[] rot = new float[3];
        cam.getRotation().toAngles(rot);
        if(rot[0] > 1.45 || rot[0] <-1.45){
            rot[0] = prevCamRotation[0];
            cam.setRotation(new Quaternion(rot));
        }
        prevCamRotation = rot;
        
        float runSpeed = 0.3f;
        Vector3f camDir = cam.getDirection().clone().multLocal(1, 0, 1).normalizeLocal();
        Vector3f camLeft = cam.getLeft().clone().normalizeLocal();
        camDir.multLocal(runSpeed);
        camLeft.multLocal(runSpeed);
        walkDirection.set(0, 0, 0);
        if (left) {
            walkDirection.addLocal(camLeft);
        }
        if (right) {
            walkDirection.addLocal(camLeft.negate());
        }
        if (up) {
            walkDirection.addLocal(camDir);
        }
        if (down) {
            walkDirection.addLocal(camDir.negate());
        }
        playerControl.setWalkDirection(walkDirection.mult(0.3f));
        cam.setLocation(playerControl.getPhysicsLocation());
    }
}
