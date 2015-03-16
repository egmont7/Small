/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.utils;

/**
 *
 * @author Caleb
 */
public class Vector2I {
    public int i1, i2;
    public Vector2I(int i1, int i2){
        this.i1 = i1;
        this.i2 = i2;
    }
    
    public Vector2I add(int a, int b){
        return new Vector2I(i1 + a, i2 + b);
    }
    public Vector2I add(Vector2I a){
        return new Vector2I(i1 + a.i1, i2 + a.i2);
    }
    public Vector2I addLocal(int a, int b){
        i1 += a;
        i2 += b;
        return this;
    }
    public Vector2I addLocal(Vector2I a){
        i1 += a.i1;
        i2 += a.i2;
        return this;
    }
    
    @Override
    public int hashCode() {
        return (i1 << 16) | i2;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Vector2I other = (Vector2I) obj;
        if (this.i1 != other.i1) {
            return false;
        }
        if (this.i2 != other.i2) {
            return false;
        }
        return true;
    }
}
