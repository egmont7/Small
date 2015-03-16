package konstruk.voxel;

import com.jme3.math.Vector3f;
import java.io.Serializable;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import konstruk.KonstrukSettings;
import konstruk.events.ReloadWorldEvent;
import konstruk.events.RenderCenterChangeEvent;
import konstruk.gameStates.LiveState;
import konstruk.io.KonstrukWorldSave;
import konstruk.utils.BlockLocator;
import konstruk.utils.ChunkLocator;
import konstruk.utils.Vector3I;

/**
 *
 * @author Caleb
 */
public class VoxelWorld implements Serializable{
    
    public static int chunkX = KonstrukSettings.CHUNK_X;
    public static int chunkY = KonstrukSettings.CHUNK_Y;
    public static int chunkZ = KonstrukSettings.CHUNK_Z;
    
    private Map<ChunkLocator,Chunk> chunks;
    private Set<ChunkLocator>       dirtyChunks;
    private Set<ChunkLocator>       renderChunks;
    private KonstrukWorldSave konstrukWorldSave;
    
    public int renderCenterX, renderCenterZ;
    
    public VoxelWorld(String worldName) {
        konstrukWorldSave = new KonstrukWorldSave(worldName);
        chunks = new ConcurrentHashMap<>();
        dirtyChunks = new HashSet<>();
        renderChunks = new HashSet<>();
        renderCenterX = -100;
        renderCenterZ = -100;
    }
    
    public void updateCenter(Vector3f pos){
        int x = ((int)pos.x) / chunkX;
        int z = ((int)pos.z) / chunkZ;
        
        int dx = x - renderCenterX;
        int dz = z - renderCenterZ;
        
        if( dx*dx > 1 || dz*dz > 1){
            //reload world(from either a teleport command or game start
            LiveState.theLiveState.konstrukEventHandler.addEvent(new ReloadWorldEvent(x,z));
        } else if( dx*dx + dz*dz >= 1){
            //move render position by change
            LiveState.theLiveState.konstrukEventHandler.addEvent(
                    new RenderCenterChangeEvent(x - renderCenterX,z - renderCenterZ,renderCenterX,renderCenterZ));
        }
        renderCenterX = x;
        renderCenterZ = z;
    }
    
    public void setBlockData(Vector3I xyz, int blockData){
        setBlock(new BlockLocator(xyz), blockData);
    }

    public int getBlockType(Vector3I xyz){
        int data = getBlock(new BlockLocator(xyz));
        return data & 0x1FFF;
    }
    
    public void setBlockType(Vector3I xyz, int blockType){
        BlockLocator blockLocator = new BlockLocator(xyz);
        setBlock(blockLocator, (getBlock(blockLocator) & 0x1FFF) | blockType);
    }
    
    public int getBlockStrength(Vector3I xyz){
        int data = getBlock(new BlockLocator(xyz));
        return (data >> 13) & 0x7F;
    }
    
    public void setBlockStrength(Vector3I xyz, int strength){
        BlockLocator bl = new BlockLocator(xyz);
        int data = getBlock(bl);
        strength = (strength > 0) ? strength : 0;
        data = (data & 0xFFF01FFF) | ((strength & 0x7F) <<13);
        setBlock(bl,data);
    }
    
    public Chunk getChunk(ChunkLocator cl){
        return chunks.get(cl);
    }
    
    public Set<ChunkLocator> getChunks(){
        return chunks.keySet();
    }
    
    public Set<ChunkLocator> getDirtyChunks(){
        Set<ChunkLocator> res = new HashSet<>(dirtyChunks);
        dirtyChunks.clear();
        return res;
    }
    
    public void setChunkRenderable(byte x, byte z, boolean renderable){
        if(x < 0 || z < 0) return;
        ChunkLocator cl = new ChunkLocator(x,z);
        Chunk c = chunks.get(cl);
        
        if(renderable){
            if(c == null){//not in memory
                c = konstrukWorldSave.readChunk(cl.x, cl.z);
                if(c == null){ //not yet generated
                    c = addChunk(cl);
                }
            }
            chunks.put(cl, c);
            if(!renderChunks.contains(cl)){
                dirtyChunks.add(cl);
                renderChunks.add(cl);
            }
            
            setChunkDirty(new ChunkLocator((byte)(cl.x+1),(byte)cl.z));
            setChunkDirty(new ChunkLocator((byte)(cl.x-1),(byte)cl.z));
            setChunkDirty(new ChunkLocator((byte)cl.x,(byte)(cl.z+1)));
            setChunkDirty(new ChunkLocator((byte)cl.x,(byte)(cl.z-1)));
        } else{
            if(c != null){
                if(renderChunks.contains(cl)){
                    dirtyChunks.add(cl);
                }
                renderChunks.remove(cl);
                konstrukWorldSave.writeChunk(c);
                chunks.remove(cl);
            }
        }
    }
    
    public boolean isChunkRenderable(byte x, byte z){
        ChunkLocator cl = new ChunkLocator(x,z);
        return renderChunks.contains(cl);
    }
    
    private void setBlock(BlockLocator bl, int blockData){
        Chunk c = chunks.get(bl.chunkPos);
        if(c == null) return;
        c.chunkData[bl.i][bl.k][bl.j] = blockData;
        setChunkDirty(c.cl);
        if(bl.i == 0){
            setChunkDirty(bl.chunkPos.add((byte)-1,(byte)0));
        } else if(bl.i == chunkX-1){
            setChunkDirty(bl.chunkPos.add((byte)1,(byte) 0));
        }
        if(bl.k == 0){
            setChunkDirty(bl.chunkPos.add((byte)0,(byte) -1));
        } else if(bl.k == chunkZ-1){
            setChunkDirty(bl.chunkPos.add((byte)0,(byte) 1));
        }
    }
    
    private int getBlock(BlockLocator bl){
        return chunks.get(bl.chunkPos).chunkData[bl.i][bl.k][bl.j];
    }

    private Chunk addChunk(ChunkLocator cl){
        Chunk c = new Chunk(cl.x, cl.z, true);
        
        return c;
    }   
    
    private void setChunkDirty(ChunkLocator cl){
        if(chunks.containsKey(cl)){
            dirtyChunks.add(cl);
        }
    }
}
