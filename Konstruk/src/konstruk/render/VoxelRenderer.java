package konstruk.render;

import com.jme3.bullet.control.RigidBodyControl;
import com.jme3.bullet.util.CollisionShapeFactory;
import com.jme3.material.Material;
import com.jme3.math.Vector2f;
import com.jme3.math.Vector3f;
import com.jme3.renderer.queue.RenderQueue;
import com.jme3.scene.Geometry;
import com.jme3.scene.Mesh;
import com.jme3.scene.VertexBuffer;
import com.jme3.util.BufferUtils;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import konstruk.KonstrukSettings;
import konstruk.blocks.BlockManager;
import konstruk.utils.ChunkLocator;
import konstruk.utils.ChunkUpdateInfo;
import konstruk.utils.RenderInfo;
import konstruk.voxel.Chunk;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class VoxelRenderer {
    public static Material blockMaterial;
    
    public static void updateLightPos(float time){
//        float r = 40;
//        float w = 1f;
//        float x = (float)(r*Math.cos(time * w)) + r;
//        float z = (float)(r*Math.sin(time * w)) + r;
//        float y = (float)(r*0.3*Math.cos(time * w*0.5)) + 30;
//        blockMaterial.setVector3("LightPosition", new Vector3f(x, 30, z));
        float x = (float)Math.sin(time * 0.01);
        float y = (float)Math.sin(time * 0.01);
    }
    
    private static final 
            int[][] cubeIndices = {
                {1,2,3,0},  //-y
                {4,7,6,5},  //+y
                {4,0,3,7},  //-x
                {6,2,1,5},  //+x
                {5,1,0,4},  //-z
                {7,3,2,6}}; //+z
    
    private static final
            int[] quadIndices = {
                0,1,2,3,0,2
            };
    
    private static final 
            Vector3f [] vertexOffsets = {
                new Vector3f(0,0,0),
                new Vector3f(1,0,0),
                new Vector3f(1,0,1),
                new Vector3f(0,0,1),
                new Vector3f(0,1,0),
                new Vector3f(1,1,0),
                new Vector3f(1,1,1),
                new Vector3f(0,1,1),
            };
        
    private static final
            Vector3f [] cubeNormals = {
                new Vector3f(0,-1,0),
                new Vector3f(0,1,0),
                new Vector3f(-1,0,0),
                new Vector3f(1,0,0),
                new Vector3f(0,0,-1),
                new Vector3f(0,0,1),
            };
        
    public static void setBlockMaterial(Material blockMat){
        blockMaterial = blockMat;
    }

    public static List<ChunkUpdateInfo> prepareChunks(VoxelWorld vw, Set<ChunkLocator> chunks){
        List<ChunkUpdateInfo> list = new LinkedList<>();
        for(ChunkLocator cl: chunks){
            Geometry g = renderChunk(vw, cl.x,cl.z);
            RigidBodyControl rbc = null;
            if(vw.isChunkRenderable(cl.x, cl.z)){
                rbc = new RigidBodyControl(CollisionShapeFactory.createMeshShape(g),0);
            }
            list.add(new ChunkUpdateInfo(g, rbc));
        }
        return list;
    }
    private static final int maxCubes2Render = KonstrukSettings.CHUNK_X*
                                               KonstrukSettings.CHUNK_Y*
                                               KonstrukSettings.CHUNK_Z;
    private static Vector3f[] vertices = new Vector3f[24*maxCubes2Render];
    private static Vector3f[] normals  = new Vector3f[24*maxCubes2Render];
    private static Vector2f[] texCoords = new Vector2f[24*maxCubes2Render];
    private static Vector2f[] strengthTexCoords = new Vector2f[24*maxCubes2Render];
    private static int [] indexes = new int[36*maxCubes2Render];
    public static Geometry renderChunk(VoxelWorld vw, byte x, byte z){
        //System.out.println(vertices.length);
        if(!vw.isChunkRenderable(x, z)){
            return new Geometry(Chunk.getChunkName(x, z));
        }
        
        List<RenderInfo> renderBlocks = getRenderInfo(vw, x, z);
        int numCubes2Render = renderBlocks.size();

        
        Mesh mesh = new Mesh();
        int vertexOffset = 0;
        int indexOffset = 0;
        
        for(RenderInfo block : renderBlocks){

            int negY = block.blockLocator & 0xFF;
            int negX = ((block.blockLocator >> 24) & 0xFF) * KonstrukSettings.CHUNK_X 
                    + ((block.blockLocator >> 12) & 0xF);
            int negZ = ((block.blockLocator >> 16) & 0xFF) * KonstrukSettings.CHUNK_Z 
                    + ((block.blockLocator >> 8) & 0xF);
            Vector3f base = new Vector3f(negX,negY,negZ);

            
            
            Vector3f[] cubeVertices = new Vector3f[8];
            for(int i = 0; i < 8; i++) {
                cubeVertices[i] = base.add(vertexOffsets[i]);
            }
            
            int mask = 0x1;
            Vector2f[] strengthTexCoord = BlockManager.manager.getStrengthTextureCoords((block.blockData >> 13) & 0x7F);
            Vector2f[] texCoord;
            for (int i = 0; i < 6; i++){
                if((block.faceInfo & mask) != 0){
                     texCoord = BlockManager.manager.getTextureCoords(block.blockData & 0x1FFF, i);
                    
                    for(int j = 0; j < 4; j++){
                        vertices[vertexOffset+j] = cubeVertices[cubeIndices[i][j]];
                        texCoords[vertexOffset+j] = texCoord[j];
                        strengthTexCoords[vertexOffset+j] = strengthTexCoord[j];
                        normals[vertexOffset+j] = cubeNormals[i];
                    }
                    for(int j = 0; j < 6; j++){
                        indexes[indexOffset+j] = vertexOffset + quadIndices[j];
                    }
                    vertexOffset += 4;
                    indexOffset += 6;
                }
                mask = mask << 1;
            }
        }
        
        Vector3f[] vertices2 = Arrays.copyOfRange(vertices, 0, vertexOffset);
        Vector3f[] normals2 = Arrays.copyOfRange(normals, 0, vertexOffset);
        Vector2f[] texCoords2 = Arrays.copyOfRange(texCoords, 0, vertexOffset);
        Vector2f[] strengthTexCoords2 = Arrays.copyOfRange(strengthTexCoords, 0, indexOffset);
        int[] indexes2 = Arrays.copyOfRange(indexes, 0, indexOffset);
        
        
        mesh.setBuffer(VertexBuffer.Type.Position,  3, BufferUtils.createFloatBuffer(vertices2));
        mesh.setBuffer(VertexBuffer.Type.Normal,    3, BufferUtils.createFloatBuffer(normals2));
        mesh.setBuffer(VertexBuffer.Type.TexCoord,  2, BufferUtils.createFloatBuffer(texCoords2));
        mesh.setBuffer(VertexBuffer.Type.TexCoord2, 2, BufferUtils.createFloatBuffer(strengthTexCoords2));
        mesh.setBuffer(VertexBuffer.Type.Index,     3, BufferUtils.createIntBuffer(indexes2));
        mesh.updateBound();

        Geometry geo = new Geometry("Chunk_"+x+"_"+z, mesh);
        geo.setMaterial(blockMaterial);
//        rootNode.setShadowMode(RenderQueue.ShadowMode.CastAndReceive);
        geo.setShadowMode(RenderQueue.ShadowMode.CastAndReceive);
//        geo.setQueueBucket(Bucket.Translucent);
        return geo;
    }
    
    private static int currentBlock;
    public static boolean isBlocking(int neighbor){
        return BlockManager.manager.isTranslucent(neighbor) && 
                ((neighbor & 0x3FF) != (currentBlock & 0x3FF));
    }
    
    public static List<RenderInfo> getRenderInfo(VoxelWorld vw, byte x, byte z) {
        List<RenderInfo> meshInfo = new LinkedList();
        ChunkLocator cl = new ChunkLocator(x,z);
        
        Chunk xplus = vw.getChunk(cl.add((byte) 1, (byte)0));
        Chunk xminus = vw.getChunk(cl.add((byte) -1, (byte) 0));
        Chunk zplus = vw.getChunk(cl.add((byte) 0, (byte) 1));
        Chunk zminus = vw.getChunk(cl.add((byte) 0, (byte) -1));
        int[][][] chunkData = vw.getChunk(cl).chunkData;
        int chunkX = KonstrukSettings.CHUNK_X;
        int chunkY = KonstrukSettings.CHUNK_Y;
        int chunkZ = KonstrukSettings.CHUNK_Z;
        
        BlockManager m = BlockManager.manager;
        int air = m.getBlock("Air").id;
        //Traverse chunk looking for non-air blocks
        for (int i = 0; i < chunkX; i++) {
            for (int j = 0; j < chunkY; j++) {
                for (int k = 0; k < chunkZ; k++) {

                    if ((chunkData[i][k][j] & 0x3FF) != air) {
                        currentBlock = chunkData[i][k][j];
                        byte faceData = 0x0;
                        
                        //+x face
                        if (i == chunkX - 1) {
                            if (((xplus != null)
                                    && isBlocking(xplus.chunkData[0][k][j]))
                                    || xplus == null) {
                                faceData = (byte) (faceData | 0x8);
                            }
                        } else if (isBlocking(chunkData[i + 1][k][j])) {
                            faceData = (byte) (faceData | 0x8);
                        }
                        //-x face
                        if (i == 0) {
                            if (((xminus != null)
                                    && isBlocking(xminus.chunkData[chunkX - 1][k][j]))
                                    || xminus == null) {
                                faceData = (byte) (faceData | 0x4);
                            }
                        } else if (isBlocking(chunkData[i - 1][k][j])) {
                            faceData = (byte) (faceData | 0x4);
                        }
                        //+y face
                        if ((j == chunkY - 1)
                                || isBlocking(chunkData[i][k][j + 1])) {
                            faceData = (byte) (faceData | 0x2);
                        }
                        //-y face
                        if ((j == 0)
                                || isBlocking(chunkData[i][k][j - 1])) {
                            faceData = (byte) (faceData | 0x1);
                        }
                        //+z face
                        if (k == chunkZ - 1) {
                            if (((zplus != null)
                                    && (isBlocking(zplus.chunkData[i][0][j])))
                                    || zplus == null) {
                                faceData = (byte) (faceData | 0x20);
                            }
                        } else if (isBlocking(chunkData[i][k + 1][j])) {
                            faceData = (byte) (faceData | 0x20);
                        }
                        //-z face
                        if (k == 0) {
                            if (((zminus != null)
                                    && isBlocking(zminus.chunkData[i][chunkZ - 1][j]))
                                    || zminus == null) {
                                faceData = (byte) (faceData | 0x10);
                            }
                        } else if (isBlocking(chunkData[i][k - 1][j])) {
                            faceData = (byte) (faceData | 0x10);
                        }

                        if (faceData != 0) {
                            int locator = (cl.x << 24) | (cl.z << 16) | (i << 12) | (k << 8) | j;
                            meshInfo.add(new RenderInfo(chunkData[i][k][j], locator, faceData));
                        }
                    }
                }
            }
        }
        return meshInfo;
    }
}
