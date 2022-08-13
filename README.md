# Learn Ada with programmable bots!

Program Text               |  Demo
:-------------------------:|:-------------------------:
![](demo/build_wall_program.png)  |  ![](demo/small_gif.gif)

The computercraft mod for Minecraft adds programmable robots called turtles,
which can move around, dig tunnels, build walls and interact with anything in-game.

AdaBots helps to teach children and adults alike the Ada programming language.
If your child cannot read yet, I recommend you start him / her on
[DoBots](http://github.com/TamaMcGlinn/DoBots) first.

It is possible to use the small terminal window in-game and the built-in scripting
language Lua to write programs, but I wanted to make this easier. This is a standalone
program you can run on your computer locally, outside of minecraft, which issues commands
to the in-game turtles. This means you can use a real programming environment, with
code-completion, a debugger, static-analysis and even mathematical proofs of program
correctness using SPARK.

## Why Ada?

Ada is a very safe, easy to learn language. It was designed from the ground up to be easy
to understand and difficult to make mistakes in. It is case-insensitive, disallows assignment
inside conditional statements, has explicit `end if;`, `end loop;` etc. instead of the popular
but confusing `}` in most programming languages, disallows all implicit type conversions
(which also makes it possible to overload on return type), and even makes multithreading easy with
its simple tasking and rendez-vous idiom. Since you can create tasks and multiple turtles in a single
program, you could explain these concepts intuitively using AdaBots.

In addition, the GCC Ada compiler is an unusually helpful one, saying things like:

```
lovelace.adb:10:04: warning: variable "Blocks" is never read and never assigned

lovelace.adb:20:30: error: "Dat" is undefined
lovelace.adb:20:30: error: possible misspelling of "Data"
```

To teach (children) using AdaBots, you need at least a basic understanding of Ada. I recommend
the AdaCore [Introduction to Ada](https://learn.adacore.com/courses/intro-to-ada/) (html / pdf)
as a course syllabus.

# Example programs

There is a collection of example programs [here](https://github.com/TamaMcGlinn/AdaBots_examples).
I recommend you use them as a starting point for using AdaBots.

## Minetest install instructions

You can use either minecraft or minetest to run a commandlistener for Adabots programs.
If using minetest, install [minetest-adabots](https://github.com/TamaMcGlinn/minetest-adabots) and follow its README.
In that case of course you can skip the Minecraft install instructions below, and read on from "Ada environment".

## Minecraft install instructions

On the minecraft side of things, you need the following one-time setup:

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

### 6) Copy, symlink or pastebin-get lua/listen.lua onto the turtle

Right click the turtle to open an in-game terminal. Issue this command to download and then execute listen:

```
pastebin get A9tniRA9 listen
```

That's the easiest way, but alternatively you can copy or symlink the file manually.

In the minecraft directory, under saves/[save_name]/computercraft/computer/ there is a numbered
directory for every turtle in computercraft. If not, you need to give the turtle a label first by
issuing `label set [name]` on the turtle, and then `edit somefile` and save the file. This will create
the turtle's directory, and you can copy or symlink lua/listen.lua into that directory,
and then issue `listen.lua` on the turtle's terminal. 

It will ask for an IP and port to listen to;
press enter for the default. If you want multiple turtles, give each a different port in your
Ada program.

## Ada environment

For Ada programming, you will need to install:

- An Ada Compiler
- A development environment such as GPS (GNAT Programming Studio)
- [Alire (alr)](https://alire.ada.dev/)

For the compiler and development environment,
go to [AdaCore.com > Community > Download](https://www.adacore.com/download)
And run the installer for your platform.

Clone (and fork) the [Adabots_examples repo](https://github.com/TamaMcGlinn/adabots_examples) to have a project up and running quickly.
Just open adabots_examples.gpr in GPS (GNAT Programming Studio).

### Compile & Run

Open a terminal and issue `make`. It should be around ten minutes the first time, but should only take seconds afterwards.

To start the program, do `./bin/[program_name]`.

Assuming listen.lua is already running on a minecraft turtle,
or `turtle:listen()` on a minetest turtle,
it should start moving.
