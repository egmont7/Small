package konstruk.events;

import com.jme3.math.Vector3f;
import konstruk.KonstrukSettings;
import konstruk.gameStates.LiveState;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class RenderCenterChangeEvent extends KonstrukEvent{

    private int deltaX, deltaZ;
    private int centerX, centerZ;
    private VoxelWorld vw;
    public RenderCenterChangeEvent(int deltaX, int deltaZ, int centerX, int centerZ){
        this.deltaX = deltaX;
        this.deltaZ = deltaZ;
        this.centerX = centerX;
        this.centerZ = centerZ;
        System.out.println("RenderCenterChangeEvent: " + centerX+", "+centerZ);
    }
    
    @Override
    public Object run(VoxelWorld vw) {
        this.vw = vw;
        byte viewDistance = KonstrukSettings.VIEW_DISTANCE;
        if(deltaX == -1){
            //moved in -x
            removeSector1(viewDistance);
            addSector2(viewDistance);
            addSector3(viewDistance);
            removeSector4(viewDistance);
            
            setPlusX(viewDistance,  false);
            setMinusX(viewDistance, true);
            setPlusZ(viewDistance,  false);
            setMinusZ(viewDistance, false);
        } else if(deltaX == 1){
            //moved in +x
            addSector1(viewDistance);
            removeSector2(viewDistance);
            removeSector3(viewDistance);
            addSector4(viewDistance);
            
            setPlusX(viewDistance,  true);
            setMinusX(viewDistance, false);
            setPlusZ(viewDistance,  false);
            setMinusZ(viewDistance, false);
        }
        if(deltaZ == -1){
            //moved in -z
            addSector1(viewDistance);
            addSector2(viewDistance);
            removeSector3(viewDistance);
            removeSector4(viewDistance);
            
            setPlusX(viewDistance,  false);
            setMinusX(viewDistance, false);
            setPlusZ(viewDistance,  false);
            setMinusZ(viewDistance, true);
        } else if(deltaZ == 1){
            //moved in +z
            removeSector1(viewDistance);
            removeSector2(viewDistance);
            addSector3(viewDistance);
            addSector4(viewDistance);
            
            setPlusX(viewDistance,  false);
            setMinusX(viewDistance, false);
            setPlusZ(viewDistance,  true);
            setMinusZ(viewDistance, false);
        }
        return null;
    }
    
    private void enableChunk(int x, int z){
        if(x >= 0 && z >= 0){
            vw.setChunkRenderable((byte)x, (byte)z, true);
        }
        
    }
    private void disableChunk(int x, int z){
        if(x >= 0 && z >= 0){
            vw.setChunkRenderable((byte)x, (byte)z, false);
        }
    }
    
    private void addSector1(byte viewDistance){
        byte x = (byte)(centerX + 1);
        byte z = (byte)(centerZ - viewDistance);
        for(byte i = 0; i < viewDistance; i++){
            enableChunk((byte)(x + i), (byte)(z + i));
        }
    }
    private void removeSector1(byte viewDistance){
        byte x = (byte)(centerX + 1);
        byte z = (byte)(centerZ - viewDistance+1);
        for(byte i = 0; i < viewDistance-1; i++){
            disableChunk((byte)(x + i), (byte)(z + i));
        }
    }
    private void addSector2(byte viewDistance){
        byte x = (byte)(centerX - 1);
        byte z = (byte)(centerZ - viewDistance);
        for(byte i = 0; i < viewDistance; i++){
            enableChunk((byte)(x - i), (byte)(z + i));
        }
    }
    private void removeSector2(byte viewDistance){
        byte x = (byte)(centerX - 1);
        byte z = (byte)(centerZ - viewDistance+1);
        for(byte i = 0; i < viewDistance-1; i++){
            disableChunk((byte)(x - i), (byte)(z + i));
        }
    }
    private void addSector3(byte viewDistance){
        byte x = (byte)(centerX - 1);
        byte z = (byte)(centerZ + viewDistance);
        for(byte i = 0; i < viewDistance; i++){
            enableChunk((byte)(x - i), (byte)(z - i));
        }
    }
    private void removeSector3(byte viewDistance){
        byte x = (byte)(centerX - 1);
        byte z = (byte)(centerZ + viewDistance - 1);
        for(byte i = 0; i < viewDistance-1; i++){
            disableChunk((byte)(x - i), (byte)(z - i));
        }
    }
    private void addSector4(byte viewDistance){
        byte x = (byte)(centerX + 1);
        byte z = (byte)(centerZ + viewDistance);
        for(byte i = 0; i < viewDistance; i++){
            enableChunk((byte)(x + i), (byte)(z - i));
        }
    }
    private void removeSector4(byte viewDistance){
        byte x = (byte)(centerX + 1);
        byte z = (byte)(centerZ + viewDistance - 1);
        for(byte i = 0; i < viewDistance-1; i++){
            disableChunk((byte)(x + i), (byte)(z - i));
        }
    }
    private void setPlusX(byte viewDistance, boolean enabled){
        if(enabled){
            enableChunk(centerX+viewDistance+1, centerZ);
        } else{
            disableChunk(centerX+viewDistance, centerZ);
        }
    }
    private void setMinusX(byte viewDistance, boolean enabled){
        if(enabled){
            enableChunk(centerX-viewDistance-1, centerZ);
        } else{
            disableChunk(centerX-viewDistance, centerZ);
        }
    }
    private void setPlusZ(byte viewDistance, boolean enabled){
        if(enabled){
            enableChunk(centerX, centerZ+viewDistance+1);
        } else{
            disableChunk(centerX, centerZ+viewDistance);
        }
    }
    private void setMinusZ(byte viewDistance, boolean enabled){
        if(enabled){
            enableChunk(centerX, centerZ-viewDistance-1);
        } else{
            disableChunk(centerX, centerZ-viewDistance);
        }
    }
}
