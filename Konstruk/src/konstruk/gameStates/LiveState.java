package konstruk.gameStates;

import com.jme3.app.Application;
import com.jme3.app.SimpleApplication;
import com.jme3.app.state.AbstractAppState;
import com.jme3.app.state.AppStateManager;
import com.jme3.asset.AssetManager;
import com.jme3.bullet.BulletAppState;
import com.jme3.collision.CollisionResult;
import com.jme3.collision.CollisionResults;
import com.jme3.input.InputManager;
import com.jme3.input.KeyInput;
import com.jme3.input.MouseInput;
import com.jme3.input.controls.ActionListener;
import com.jme3.input.controls.KeyTrigger;
import com.jme3.input.controls.MouseButtonTrigger;
import com.jme3.light.AmbientLight;
import com.jme3.light.DirectionalLight;
import com.jme3.material.Material;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Ray;
import com.jme3.math.Vector3f;
import com.jme3.renderer.Camera;
import com.jme3.renderer.ViewPort;
import com.jme3.scene.Node;
import com.jme3.scene.Spatial;
import com.jme3.shadow.DirectionalLightShadowRenderer;
import com.jme3.texture.Texture;
import konstruk.entities.Player;
import konstruk.events.BlastEvent;
import konstruk.events.KonstrukEventHandler;
import konstruk.events.PlayerCreationEvent;
import konstruk.events.SingleBlockEditEvent;
import konstruk.render.VoxelRenderer;
import konstruk.utils.ChunkUpdateInfo;
import konstruk.utils.Vector3I;
import konstruk.utils.VoxelMath;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class LiveState extends AbstractAppState{
    
    public static LiveState theLiveState = new LiveState();
    
    private SimpleApplication app;
    private Node              rootNode;
    private AssetManager      assetManager;
    private AppStateManager   stateManager;
    private InputManager      inputManager;
    private ViewPort          viewPort;
    private BulletAppState    physics;
    private Camera            cam;
    private Player            player;
    
    private Node landscapeChunks = new Node();
    public KonstrukEventHandler konstrukEventHandler;
    private ActionListener mouseActionListener;
    private ActionListener movementActionListener;
    private ActionListener commandActionListener;
    private VoxelWorld vw;
    
    DirectionalLightShadowRenderer dlsRenderer;
    AmbientLight al;
    
    //Custom Methods    
    private void updateChunks() {
        ChunkUpdateInfo cui = konstrukEventHandler.pollUpdate();
        if (cui == null) {
            return;
        }
        Spatial oldChunk = landscapeChunks.getChild(cui.geo.getName());
        if(oldChunk != null){
            landscapeChunks.detachChild(oldChunk);
            
            physics.getPhysicsSpace().removeAll(oldChunk);
        }
        if(cui.control != null){
            cui.geo.addControl(cui.control);
            physics.getPhysicsSpace().add(cui.control);
            landscapeChunks.attachChild(cui.geo);
        }
//        System.out.println(landscapeChunks.getChildren().size());
    }
    
    
    
  @Override
  public void initialize(AppStateManager stateManager, Application app) {
    super.initialize(stateManager, app);
    SimpleApplication sApp = (SimpleApplication)app;
    this.app =  sApp; // can cast Application to something more specific
    this.rootNode     = this.app.getRootNode();
    this.assetManager = this.app.getAssetManager();
    this.stateManager = this.app.getStateManager();
    this.inputManager = this.app.getInputManager();
    this.viewPort     = this.app.getViewPort();
    this.physics      = this.stateManager.getState(BulletAppState.class);
    this.cam          = this.app.getCamera();
    
    
    physics = new BulletAppState();
    physics.setThreadingType(BulletAppState.ThreadingType.PARALLEL);
    stateManager.attach(physics);
    rootNode.attachChild(landscapeChunks);


    vw = new VoxelWorld("Default_World");
    konstrukEventHandler = new KonstrukEventHandler(vw);

    initTexture();
    initWorld();
    initPlayer();
  }
  
  
  private void initWorld() {
        viewPort.setBackgroundColor(new ColorRGBA(0.7f, 0.8f, 1f, 1f));
    }

    private void initPlayer() {
        konstrukEventHandler.addEvent(new PlayerCreationEvent(cam,new Vector3f(10,50,10),"Barry"));
        while(true){
            player = konstrukEventHandler.pollPlayer();
            if(player != null) break;
        }
        physics.getPhysicsSpace().add(player.getPlayerControl());
        setUpKeys();
    }

    private void initTexture() {
        Material mat = new Material(assetManager, "Shaders/Lighting.j3md");//BlockShader.j3md");
        Texture textureAtlas = assetManager.loadTexture("Textures/atlas.png");
//        Texture strengthAtlas = assetManager.loadTexture("Textures/strength_atlas.png");

        textureAtlas.setMagFilter(Texture.MagFilter.Nearest);
        textureAtlas.setMinFilter(Texture.MinFilter.NearestNoMipMaps);
        mat.setTexture("DiffuseMap", textureAtlas);
        mat.setBoolean("UseMaterialColors", true);
        mat.setColor("Ambient",  ColorRGBA.White);
        mat.setColor("Diffuse",  ColorRGBA.White);
        mat.setColor("Specular", ColorRGBA.White);
        mat.setFloat("Shininess", 0);
        VoxelRenderer.setBlockMaterial(mat);
        
        al = new AmbientLight();
        al.setColor(ColorRGBA.White.mult(1.3f));
        rootNode.addLight(al);
        
        // Drop shadows
        dlsRenderer = new DirectionalLightShadowRenderer(assetManager,4096,3);
        DirectionalLight light = new DirectionalLight();
        light.setDirection(new Vector3f(-0.5f,-0.5f,-0.5f).normalizeLocal());
        dlsRenderer.setLight(light);
        viewPort.addProcessor(dlsRenderer);
        
        
    }

    private void setUpKeys() {
        
        mouseActionListener = new ActionListener() {
            @Override
            public void onAction(String name, boolean keyPressed, float tpf) {
                if(!keyPressed) return;
                CollisionResults results = new CollisionResults();
                Ray ray = new Ray(cam.getLocation(), cam.getDirection());
                if (landscapeChunks.collideWith(ray, results) == 0) {
                    return;
                }

                CollisionResult cr = results.getClosestCollision();

                Vector3I blockLocator;
                switch (name) {
                    case "LeftClick":
                        blockLocator = VoxelMath.getBlockFromSurfacePoint(cr.getContactPoint(),
                                cr.getContactNormal());
                        konstrukEventHandler.addEvent(new BlastEvent(blockLocator, 10f, 1000f));
//                        konstrukEventHandler.addEvent(new SingleBlockEditEvent(blockLocator, 0xFE000));
                        break;
                    case "RightClick":
                        blockLocator = VoxelMath.getAdjacentBlockFromSurfacePoint(cr.getContactPoint(),
                                cr.getContactNormal());
                        konstrukEventHandler.addEvent(new SingleBlockEditEvent(blockLocator, 0xFE001));
                        break;
                }
            }
        };
        
        movementActionListener = new ActionListener() {
            @Override
            public void onAction(String name, boolean keyPressed, float tpf) {
                switch (name) {
                    case "Left":
                        player.setLeft(keyPressed);
                        break;
                    case "Right":
                        player.setRight(keyPressed);
                        break;
                    case "Up":
                        player.setUp(keyPressed);
                        break;
                    case "Down":
                        player.setDown(keyPressed);
                        break;
                    case "Jump":
                        player.jump();
                        break;
                }
            }
        };
        commandActionListener = new ActionListener() {
            @Override
            public void onAction(String name, boolean keyPressed, float tpf) {
                switch (name) {
                    case "ResetPlayer":
                        player.setPosition(new Vector3f(1000, 80, 1000));
                        break;
                }
            }
        };
        inputManager.addMapping("Left", new KeyTrigger(KeyInput.KEY_A));
        inputManager.addMapping("Right", new KeyTrigger(KeyInput.KEY_D));
        inputManager.addMapping("Up", new KeyTrigger(KeyInput.KEY_W));
        inputManager.addMapping("Down", new KeyTrigger(KeyInput.KEY_S));
        inputManager.addMapping("Jump", new KeyTrigger(KeyInput.KEY_SPACE));
        inputManager.addListener(movementActionListener, "Left");
        inputManager.addListener(movementActionListener, "Right");
        inputManager.addListener(movementActionListener, "Up");
        inputManager.addListener(movementActionListener, "Down");
        inputManager.addListener(movementActionListener, "Jump");
        
        inputManager.addMapping("SaveWorld", new KeyTrigger(KeyInput.KEY_K));
        inputManager.addMapping("LoadWorld", new KeyTrigger(KeyInput.KEY_L));
        inputManager.addMapping("ResetPlayer", new KeyTrigger(KeyInput.KEY_R));
        inputManager.addListener(commandActionListener, "LoadWorld");
        inputManager.addListener(commandActionListener, "SaveWorld");
        inputManager.addListener(commandActionListener, "ResetPlayer");
        
        inputManager.addMapping("LeftClick", 
                new MouseButtonTrigger(MouseInput.BUTTON_LEFT));
        inputManager.addMapping("RightClick", 
                new MouseButtonTrigger(MouseInput.BUTTON_RIGHT));
        inputManager.addListener(mouseActionListener, "LeftClick");
        inputManager.addListener(mouseActionListener, "RightClick");
    }
  
  @Override
  public void cleanup(){
        konstrukEventHandler.stop();
  }
  
  @Override
  public void setEnabled(boolean enabled){
      super.setEnabled(enabled);
      
      if(enabled){
          rootNode.attachChild(landscapeChunks);
      } else{
          rootNode.detachChild(landscapeChunks);
      }
      
  }
    float timeCounter = 0;

    @Override
    public void update(float tpf) {
        timeCounter += tpf;
        player.update();
        vw.updateCenter(player.getPosition());
        updateChunks();
        
        float x = (float)Math.sin(timeCounter * 0.1);
        float y = (float)Math.cos(timeCounter * 0.1);
        
        DirectionalLight light = new DirectionalLight();
        light.setDirection(new Vector3f(x,y,-0.3f).normalizeLocal());
        dlsRenderer.setLight(light);
        al.setColor(ColorRGBA.White.mult(((1-y)+1f)/2f));
    }
  
}
