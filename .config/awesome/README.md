Installation
------------
To use this configuration, git clone this, and mv awesome-config to ~/.config/awesome

  * `cd ~/.config; git clone https://github.com/primitivorm/awesome.git awesome`
  * `cd ~/.config/awesome`
  * `git submodule init && git submodule update` - download vicious module

Try the one liner:

```bash
git clone https://github.com/primitivorm/awesome.git ~/.config/awesome && cd ~/.config/awesome && git submodule init && git submodule update && less ~/.config/awesome/README.md
```

Configuration
-------------
  Create a file called `personal.lua` in `~/.config/awesome`. Here are some things you can place in
  your `~/.config/awesome/personal.lua` file:

```lua
terminal = 'xterm' -- can be app in path, or full path e.g. /usr/bin/xterm
editor = "vim"

wallpaper_dir = os.getenv("HOME") .. "/yourwallpaper_dir/" -- grabs a random bg

taglist_numbers = "arabic" -- we support arabic (1,2,3...),
-- arabic, chinese, {east|persian}_arabic, roman, thai, random

cpugraph_enable = true -- show CPU graph
cputext_format = " $1%" -- %1 average cpu, %[2..] every other thread individually

membar_enable = true -- show memory bar
memtext_format = " $1%" -- %1 percentage, %2 used %3 total %4 free

date_format = "%a %m/%d/%Y %l:%M%p" -- refer to http://en.wikipedia.org/wiki/Date_(Unix) specifiers

networks = {'eth0', 'wlan0'} -- Add your devices network interface here netwidget, only show one that works
```

  save.

  You can use `Mod + Control + r` to reload configuation.

### Autorun
  Create a file called `autorun.lua` in `~/.config/awesome`.

  First is the binary of the app, the second is the arguments. So to load `xscreensaver -no-splash` we do:
  `run_once("xscreensaver", "-no-splash")`.

```lua
run_once("xscreensaver", "-no-splash")         -- starts screensaver daemon
run_once("xsetroot", "-cursor_name left_ptr")  -- sets the cursor icon

--run_once("redshift", "-o -l 0:0 -b 0.5 -t 6500:6500") -- brightness
run_once("ibus-daemon", "--xim") -- ibus
run_once(os.getenv("HOME") .. "/.dropbox-dist/dropboxd") -- dropbox
run_once("nm-applet") -- networking

run_once("wmname", "LG3D") -- java fix

run_once("sh " .. os.getenv("HOME") .. "/.screenlayout/dual-monitor.sh") -- set screens up
