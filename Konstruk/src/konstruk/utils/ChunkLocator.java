/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.utils;

import java.io.Serializable;

/**
 *
 * @author Caleb
 */
public class ChunkLocator implements Serializable{
    public byte x, z;
    public ChunkLocator(byte i1, byte i2){
        this.x = i1;
        this.z = i2;
    }
    
    public ChunkLocator add(byte a, byte b){
        return new ChunkLocator((byte)(x + a), (byte)(z + b));
    }
    public ChunkLocator add(ChunkLocator a){
        return new ChunkLocator((byte)(x + a.x), (byte)(z + a.z));
    }
    public ChunkLocator addLocal(byte a, byte b){
        x += a;
        z += b;
        return this;
    }
    public ChunkLocator addLocal(ChunkLocator a){
        x += a.x;
        z += a.z;
        return this;
    }
    
    @Override
    public String toString(){
        return "" + x + ":" + z;
    }
    
    @Override
    public int hashCode() {
        int hash = x;
        hash = (hash << 8) | z;
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ChunkLocator other = (ChunkLocator) obj;
        if (this.x != other.x) {
            return false;
        }
        if (this.z != other.z) {
            return false;
        }
        return true;
    }
}
