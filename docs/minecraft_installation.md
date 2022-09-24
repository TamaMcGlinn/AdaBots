# Minecraft install instructions

### 1) Choose latest versions of the following that work together

Note: If you already installed [DoBots](http://github.com/TamaMcGlinn/DoBots), that should work fine for AdaBots as well. You only need to
do step 6 - Configure computercraft to allow http. DoBots uses a slightly older version of Minecraft in order to support Plethora for
keyboards - while AdaBots doesn't use keyboards, you may still want to use Plethora for its other peripherals.

Currently the latest supported version is Minecraft and
[Minecraft Forge 1.18.2-40.1.60](https://files.minecraftforge.net/net/minecraftforge/forge/index_1.18.2.html)
with [CC:Tweaked 1.18.2-1.100.8](https://www.curseforge.com/minecraft/mc-mods/cc-tweaked/files/3845705).

### 2) Install [minecraft](https://minecraft.net/)

You will need to purchase the game.

### 3) Install [minecraft forge](https://files.minecraftforge.net/net/minecraftforge/forge/)

Download the installer for your operating system and minecraft version. Run it:

- on linux, issue `java -jar ~/Downloads/forge-*.jar` on the terminal
- on windows or mac, double-click the jar file

Select the forge game version in the launcher and press play, letting it load until you
are presented with the main game menu.

### 4) Install [CC:Tweaked](https://www.curseforge.com/minecraft/mc-mods/cc-tweaked)

The minecraft directory is 

- Windows => %appdata%\\.minecraft\
- Mac     => ~/Library/Application Support/minecraft/
- Linux   => ~/.minecraft/

It contains mods/, and you should put the cc-tweaked .jar file in there and restart.

### 5) Give yourself a turtle in-game

I recommend you create a survival mode world set to peaceful with cheats enabled. 
You may also wish to enter these commands in-game to avoid distractions:

```
/gamerule doDayNightCycle off
/time set day
/weather clear
```

With cheats enabled, you can type `/give [yourname] computercraft:turtle_normal` (with tab-completion).
If you [craft it](https://www.minecraft-crafting.net/) beside a diamond pickaxe, you get a mining turtle which can also dig.

Inside the minecraft directory, edit saves/[save_name]/serverconfig/computercraft-server.toml:

- Remove all [[http.rules]] blocks that say 'deny'
- (optional) Set need_fuel = false so that turtles can move without fuel

Note: I am going to assume you set need_fuel = false. The provided example programs won't run
if you left this at true and didn't manually refuel the turtles.

If that file doesn't exist, you may instead need to find the following in config/computercraft.cfg:

```
    # A list of wildcards for domains or IP ranges that cannot be accessed through the "http" API on Computers.
    # If this is empty then all explicitly allowed domains will be accessible. Example: "*.github.com" will block access to all subdomains of github.com.
    # You can use domain names ("pastebin.com"), wildcards ("*.pastebin.com") or CIDR notation ("127.0.0.0/8").
    S:blocked_domains <
        127.0.0.0/8
        10.0.0.0/8
        172.16.0.0/12
        192.168.0.0/16
        fd00::/8
     >
```

And remove the blocked domains.

### 6) Get listen program onto the turtle

Right click the turtle to open an in-game terminal. Issue this command to download and then execute listen:

```
pastebin get A9tniRA9 listen
```

That's the easiest way, but alternatively you can copy or symlink the file manually as follows.

In the minecraft directory, under saves/[save_name]/computercraft/computer/ there is a numbered
directory for every turtle in computercraft. If not, you need to give the turtle a label first by
issuing `label set [name]` on the turtle, and then `edit somefile` and save the file (press cntrl to bring up the menu).
This will create the turtle's directory, and you can copy or symlink lua/listen.lua into that directory,
and then issue `listen` on the turtle's terminal. 

It will ask for an IP and port to listen to;
press enter for the default; I recommend you edit the file (either on the turtle, or under saves/ as above) so that
the default ip points to your usual development computer.
If you want multiple turtles, give each a different port in your Ada program.
