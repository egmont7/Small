/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package konstruk.utils;

/**
 *
 * @author Caleb
 */
public class RenderInfo {
    public int blockLocator;
    public int blockData;
    public byte faceInfo;
    public RenderInfo(int blockData,int blockLocator, byte faceInfo){
        this.blockData = blockData;
        this.blockLocator = blockLocator;
        this.faceInfo = faceInfo;
    }
}
