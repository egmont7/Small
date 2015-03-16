/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.blocks;

import com.jme3.math.Vector2f;
import java.util.HashMap;
import java.util.Map;
/**
 *
 * @author Caleb
 */
public final class BlockManager {
    
    public static BlockManager manager = new BlockManager();
    
    private Map<Integer,Block> blocksId;
    private Map<String,Block> blocksName;
    private int numBlocks = 0;
    
    public BlockManager(){
        blocksId = new HashMap<>();
        blocksName = new HashMap<>();
        addBlock(new BlockAir());
        addBlock(new BlockStone());
        addBlock(new BlockGrass());
        addBlock(new BlockWater());
        addBlock(new BlockNoDestroy());
        addBlock(new BlockWood());
    }
    
    public void addBlock(Block block){
        blocksId.put(numBlocks, block);
        blocksName.put(block.name, block);
        block.id = numBlocks;
        numBlocks++;
    }
    
    public Block getBlock(int id){
        return blocksId.get(id);
    }
    
    public Block getBlock(String name){
        return blocksName.get(name);
    }

    public boolean isTranslucent(int id){
        return blocksId.get(id & 0x3FF).isTranslucent;
    }
    
    public Vector2f[] getTextureCoords(int blockId, int faceId){
        Block block = blocksId.get(blockId);
        
        int atlasLookup = block.atlasLookups[faceId];
        
        Vector2f[] textureCoords = new Vector2f[4];
        float size = 1.0f / 16.0f;
        textureCoords[0] = new Vector2f((atlasLookup & 0xFF)*size,1f-(atlasLookup >> 8)*size);
        textureCoords[1] = textureCoords[0].clone().addLocal(0f, -size);
        textureCoords[2] = textureCoords[0].clone().addLocal(size, -size);
        textureCoords[3] = textureCoords[0].clone().addLocal(size, 0f);
        return textureCoords;
    }
    
    public Vector2f[] getStrengthTextureCoords(int strength){
        int x_coord = 10 - ((int) (strength * 10f / 127f));
        
        Vector2f[] strengthTextureCoords = new Vector2f[4];
        float size = 1f / 11f;
        strengthTextureCoords[0] = new Vector2f(x_coord*size,1);
        strengthTextureCoords[1] = strengthTextureCoords[0].clone().addLocal(0f, -1);
        strengthTextureCoords[2] = strengthTextureCoords[0].clone().addLocal(size, -1);
        strengthTextureCoords[3] = strengthTextureCoords[0].clone().addLocal(size, 0f);
        return strengthTextureCoords;
    }
    
}
