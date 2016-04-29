Tiny TV Torrent Manager
=======================

A dead-simple script to manage automated downloading of TV shows
supplied by an rss feed.

## Setup Instructions
  1. Setup your personal RSS feed via http://new.showrss.info/
  2. Install Transmission
  3. Figure out where Transmission is downloading files to, change it if necessary.
  4. Edit the global vars in manage_torrents to reflect your local setup.
  5. Test it by simply running `sudo manage_torrents`. You should see a few torrents get added to transmission.
  6. Test it again by running the same command after a few torrents have finished.
  7. If all works ok, setup the script to be run periodically via either `cron` or a `systemd` timer. Note that `manage_torrents` must be set to run as root so that it can change the ownership of the downloaded files from `transmission` to you. If your local Transmission setup creates downloaded files with you as the owner then running as root is not needed and you can remove any `uid` related lines from manage_torrents.

## Dependencies
  - feedparser
  - transmissionrpc
