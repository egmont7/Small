package konstruk.events;

import java.util.Comparator;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import konstruk.entities.Player;
import konstruk.render.VoxelRenderer;
import konstruk.utils.ChunkUpdateInfo;
import konstruk.voxel.VoxelWorld;

/**
 *
 * @author Caleb
 */
public class KonstrukEventHandler implements Runnable {

    BlockingQueue<KonstrukEvent> eventQueue;
    BlockingQueue<ChunkUpdateInfo> geoQueue;
    BlockingQueue<Player> playerQueue;
    VoxelWorld vw;
    Thread t;
    boolean stopRequested = false;

    public KonstrukEventHandler(VoxelWorld vw) {
        this.vw = vw;
        eventQueue = new PriorityBlockingQueue<>(100, new Comparator<KonstrukEvent>() {
            public int compare(KonstrukEvent a, KonstrukEvent b) {
                return a.priority - b.priority;
            }
        });
        geoQueue = new LinkedBlockingQueue<>();
        playerQueue = new LinkedBlockingQueue<>();

        t = new Thread(this, "KonstrukEventHandler");
        t.start();
    }

    public void addEvent(KonstrukEvent e) {
        eventQueue.offer(e);
    }

    public ChunkUpdateInfo pollUpdate() {
        return geoQueue.poll();
    }

    public Player pollPlayer() {
        return playerQueue.poll();
    }

    public void stop() {
        eventQueue.add(new QuitEvent());
    }

    @Override
    public void run() {
        while (true) {
            try {
                KonstrukEvent event = eventQueue.take();
                Object result = null;
                try {
                    result = event.run(vw);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                if (result instanceof Player) {
                    playerQueue.offer((Player) result);
                } else if( result instanceof QuitEvent){
                    return;
                } else {
                    for (ChunkUpdateInfo cui : VoxelRenderer.prepareChunks(vw, vw.getDirtyChunks())) {
                        geoQueue.offer(cui);
                    }
                }
            } catch (InterruptedException e) {
                continue;
            }
        }
    }
}
