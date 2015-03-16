package konstruk;

import com.jme3.app.SimpleApplication;
import com.jme3.app.state.ScreenshotAppState;
import com.jme3.system.AppSettings;
import konstruk.gameStates.LiveState;

public class Main extends SimpleApplication {

//    LiveState liveState;
    
    
    public static void main(String[] args) {
        Main app = new Main();
        AppSettings newSetting = new AppSettings(true);
        newSetting.setFrameRate(30);
        newSetting.setResolution(1920, 1080);
        newSetting.setFullscreen(false);
        app.setSettings(newSetting);
        app.start();
    }

    @Override
    public void simpleInitApp() {
        stateManager.attach(LiveState.theLiveState);
        //ScreenshotAppState screenShotState = new ScreenshotAppState();
        //stateManager.attach(screenShotState);
    }
}
