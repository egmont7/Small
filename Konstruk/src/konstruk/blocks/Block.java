/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.blocks;

/**
 *
 * @author Caleb
 */
    public abstract class Block {
        public String name;
        public int id;
        public float resilience; //Range: (0,1) exclusive
        public boolean isTranslucent = false;
        public int atlasLookups[];
    }
