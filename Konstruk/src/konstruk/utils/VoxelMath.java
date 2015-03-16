/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.utils;

import com.jme3.math.Vector3f;
import konstruk.KonstrukSettings;

/**
 *
 * @author Caleb
 */
public abstract class VoxelMath {
      
//    public static int[] xyz2Chunkijk(Vector3I xyz){
//        int[] chunkijk = new int[4];
//        chunkijk[0] = ((xyz.x / KonstrukSettings.chunkX) << 8) | (xyz.z / KonstrukSettings.chunkZ);
//        chunkijk[1] = xyz.x % KonstrukSettings.chunkX;
//        chunkijk[2] = xyz.y;
//        chunkijk[3] = xyz.z % KonstrukSettings.chunkZ;
//        return chunkijk;
//    }
    
    
    
    public static Vector3I getAdjacentBlockFromSurfacePoint(Vector3f contactPoint, Vector3f normal){
        
        int x = -1;
        int y = -1;
        int z = -1;
        
        normal.x = (float)Math.round(normal.x);
        normal.y = (float)Math.round(normal.y);
        normal.z = (float)Math.round(normal.z);
        
        if(normal.subtract(Vector3f.UNIT_X).length() < 0.5){ //+x face
            x = Math.round(contactPoint.x);
            y = (int)contactPoint.y;
            z = (int)contactPoint.z;
        } else if(normal.subtract(Vector3f.UNIT_X.negate()).length() < 0.5){ //-x face
            x = Math.round(contactPoint.x) - 1;
            y = (int)contactPoint.y;
            z = (int)contactPoint.z;
        } else if(normal.subtract(Vector3f.UNIT_Y).length() < 0.5){ //+y face
            x = (int)contactPoint.x;
            y = Math.round(contactPoint.y);
            z = (int)contactPoint.z;
        } else if(normal.subtract(Vector3f.UNIT_Y.negate()).length() < 0.5){ //-y face
            x = (int)contactPoint.x;
            y = Math.round(contactPoint.y) - 1;
            z = (int)contactPoint.z;
        } else if(normal.subtract(Vector3f.UNIT_Z).length() < 0.5){ //+z face
            x = (int)contactPoint.x;
            y = (int)contactPoint.y;
            z = Math.round(contactPoint.z);
        } else if(normal.subtract(Vector3f.UNIT_Z.negate()).length() < 0.5){ //-z face
            x = (int)contactPoint.x;
            y = (int)contactPoint.y;
            z = Math.round(contactPoint.z) - 1;
        }
        return new Vector3I(x,y,z);
    }
  
     // This method is a *lot* faster than using (int)Math.floor(x)
    public static int fastfloor(double x) {
        return x>0 ? (int)x : (int)x-1;
    }
    
    public static Vector3I getBlockFromSurfacePoint(Vector3f contactPoint, Vector3f normal){
        
        int x = -1;
        int y = -1;
        int z = -1;
        
        normal.x = (float)Math.round(normal.x);
        normal.y = (float)Math.round(normal.y);
        normal.z = (float)Math.round(normal.z);
        
        if(normal.subtract(Vector3f.UNIT_X).length() < 0.5){ //+x face
            x = Math.round(contactPoint.x) - 1;
            y = (int)contactPoint.y;
            z = (int)contactPoint.z;
        } else if(normal.subtract(Vector3f.UNIT_X.negate()).length() < 0.5){ //-x face
            x = Math.round(contactPoint.x);
            y = (int)contactPoint.y;
            z = (int)contactPoint.z;
        } else if(normal.subtract(Vector3f.UNIT_Y).length() < 0.5){ //+y face
            x = (int)contactPoint.x;
            y = Math.round(contactPoint.y) - 1;
            z = (int)contactPoint.z;
        } else if(normal.subtract(Vector3f.UNIT_Y.negate()).length() < 0.5){ //-y face
            x = (int)contactPoint.x;
            y = Math.round(contactPoint.y);
            z = (int)contactPoint.z;
        } else if(normal.subtract(Vector3f.UNIT_Z).length() < 0.5){ //+z face
            x = (int)contactPoint.x;
            y = (int)contactPoint.y;
            z = Math.round(contactPoint.z) - 1;
        } else if(normal.subtract(Vector3f.UNIT_Z.negate()).length() < 0.5){ //-z face
            x = (int)contactPoint.x;
            y = (int)contactPoint.y;
            z = Math.round(contactPoint.z);
        }
        return new Vector3I(x,y,z);
    }
}
