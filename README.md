# Learn Ada by programming Minecraft robots!

The computercraft mod for Minecraft adds programmable robots called turtles,
which can move around, dig tunnels, build walls and interact with anything in-game.

This repository intends to teach children and adults alike the Ada programming language.
If your child cannot read yet, I recommend you start him / her on
[DoBots](http://github.com/TamaMcGlinn/DoBots) first.

It is possible to use the small terminal window in-game and the built-in scripting
language Lua to write programs, but I wanted to make this easier. This is a standalone
program you can run on your computer locally, outside of minecraft, which issues commands
to the in-game turtles. This means you can use a real programming environment, with
code-completion, a debugger, static-analysis and even mathematical proofs of program
correctness.

## Ada

Ada is a very safe, easy to learn language. It was designed from the ground up to be easy
to understand, and difficult to make mistakes in. It is case-insensitive, disallows assignment
inside conditional statements, has explicit `end if;`, `end loop;` etc. instead of the popular
but confusing `}` in most programming languages. The tasking and rendez-vous idiom supported
by Ada is far easier than the explicit locks and threading prevalent in the software industry,
and makes it very difficult to create data-races 

Program Text               |  Demo
:-------------------------:|:-------------------------:
![](demo/build_wall_program.png)  |  ![](demo/small_gif.gif)

## Install instructions (minecraft)

On the minecraft side of things, you need the following one-time setup:

1) Choose latest versions of the following that work together

Note: If you already installed [DoBots](http://github.com/TamaMcGlinn/DoBots), that should work fine for AdaBots as well. You only need to
do step 6 - Configure computercraft to allow http. DoBots uses a slightly older version of Minecraft in order to support Plethora for
keyboards - while AdaBots doesn't use keyboards, you may still want to use Plethora for its other peripherals.

Currently the latest supported version is Minecraft and
[Minecraft Forge 1.16.4](https://files.minecraftforge.net/net/minecraftforge/forge/index_1.16.4.html)
with [CC:Tweaked 1.96.0](https://www.curseforge.com/minecraft/mc-mods/cc-tweaked/files)

2) Install [minecraft](https://minecraft.net/)

You will need to purchase the game.

3) Install [minecraft forge](https://files.minecraftforge.net/net/minecraftforge/forge/)

Download the installer for your operating system and minecraft version. Run it:

- on linux, issue `java -jar ~/Downloads/forge-*.jar` on the terminal
- on windows or mac, double-click the jar file

Select the forge game version in the launcher and press play, letting it load until you
are presented with the main game menu.

4) Install [CC:Tweaked](https://www.curseforge.com/minecraft/mc-mods/cc-tweaked)

The minecraft directory is 

- Windows => %appdata%\.minecraft\
- Mac     => ~/Library/Application Support/minecraft/
- Linux   => ~/.minecraft/

It contains mods/, and you should put the cc-tweaked .jar file in there and restart.

5) Give yourself a turtle in-game

I recommend you create a survival mode world set to peaceful with cheats enabled. 
You may also wish to enter these commands in-game to avoid distractions:

```
/gamerule doDayNightCycle off
/time set day
/weather off
/weather clear
```

With cheats enabled, you can type `/give [yourname] computercraft:turtle_normal` (with tab-completion).
Place it and right click it to open an in-game terminal on the turtle. Issue this command to give
the turtle a name, and also to create the turtle's program directory:

```
label set [name]
```

Inside the minecraft directory, edit saves/[save_name]/serverconfig/computercraft-server.toml:

- Remove all [[http.rules]] blocks that say 'deny'
- (optional) Set need_fuel = false so that turtles can move without fuel

7) Copy (or symlink) lua/httpslave onto the turtle

In the minecraft directory, under saves/[save_name]/computercraft/computer/ there is a numbered
directory for every turtle in computercraft. You can copy lua/httpslave into that directory,
and then issue `httpslave` on the turtle's terminal. It will ask for an IP and port to listen to;
press enter for the default. If you want multiple turtles, give each a different port in your
Ada program like so:

```

```

## Ada environment

For Ada programming, you will need to install:

- An Ada Compiler
- A development environment such as GPS

Both come with the GNAT Community edition.
Go to [AdaCore.com > Community > Download](https://www.adacore.com/download)
And run the installer for your platform.

Finally, run GPS and open adabots.gpr to edit the code.

# Compile

To compile, issue `alr build` from the (out-of-game) terminal in the root directory of this repository.

# Run

To start the program, issue `./bin/main`. Assuming httpslave is already running on a turtle,
it should start spinning.

