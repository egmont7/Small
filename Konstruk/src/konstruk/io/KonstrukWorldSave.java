/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import konstruk.KonstrukSettings;
import konstruk.voxel.Chunk;

/**
 *
 * @author Caleb
 */
public class KonstrukWorldSave {

    String directory;
    int fileLen = KonstrukSettings.CHUNK_X*
                  KonstrukSettings.CHUNK_Y*
                  KonstrukSettings.CHUNK_Z;
    public KonstrukWorldSave(String worldName) {
        directory = KonstrukSettings.SAVE_LOCATION + File.separator
                + worldName + File.separator;
        File dir = new File(directory);
        if (!dir.exists()) {
            System.out.println("Creating Directory: " + directory);
            dir.mkdir();
        }
    }

    public Chunk readChunk(int x, int z) {
        String path = directory + Chunk.getChunkName(x, z) + ".kchunk";
        File inFile = new File(path);
        if(!inFile.exists()){
            return null;
        }
        
        Chunk result = null;
        try{
            result = new Chunk((byte)x, (byte)z, false);
            RandomAccessFile in = new RandomAccessFile(inFile, "rw");
            FileChannel file = in.getChannel();
            ByteBuffer buf = file.map(FileChannel.MapMode.READ_WRITE, 0, 4 * fileLen);
            for(int i = 0; i < KonstrukSettings.CHUNK_X; i++){
                for(int j = 0; j < KonstrukSettings.CHUNK_Y; j++){
                    for(int k = 0; k < KonstrukSettings.CHUNK_Z; k++){
//                        buf.putInt(chunk.chunkData[i][j][k]);
                        result.chunkData[i][k][j] = buf.getInt();
                    }
                }
            }
            file.close();
            in.close();
        } catch(IOException ex){
            throw new RuntimeException(ex);
        }
        return result;
    }

    public void writeChunk(Chunk chunk) {

        String path = directory + chunk.name + ".kchunk";

        File outFile = new File(path);
        if (outFile.exists()) {
            outFile.delete();
        }
        
        
        try {
            RandomAccessFile out = new RandomAccessFile(outFile, "rw");
            FileChannel file = out.getChannel();
            ByteBuffer buf = file.map(FileChannel.MapMode.READ_WRITE, 0, 4 * fileLen);
            for(int i = 0; i < KonstrukSettings.CHUNK_X; i++){
                for(int j = 0; j < KonstrukSettings.CHUNK_Y; j++){
                    for(int k = 0; k < KonstrukSettings.CHUNK_Z; k++){
                        buf.putInt(chunk.chunkData[i][k][j]);
                    }
                }
            }
            file.close();
            out.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}