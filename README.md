# Learn Ada by programming Minecraft robots!

The computercraft mod for Minecraft adds programmable robots called turtles,
which can move around, dig tunnels, build walls and interact with anything in-game.

It is possible to use the small terminal window in-game and the built-in scripting
language Lua to write programs, but I wanted to make this easier. This is a standalone
program you can run on your computer locally, outside of minecraft, which issues commands
to the in-game turtles. This means you can use a real programming environment, with
code-completion, a debugger, static-analysis and mathematical proofs of program correctness.

## Ada

Ada is a very safe, easy to learn language. It was designed from the ground up to be easy
to understand, and difficult to make mistakes in.

Program Text               |  Demo
:-------------------------:|:-------------------------:
![](demo/build_wall_program.png)  |  ![](demo/small_gif.gif)

## Install instructions (minecraft)

On the minecraft side of things, you need the following one-time setup:

1) Choose latest versions of the following that work together

Currently that is Minecraft 1.16.4, CC:Tweaked 

2) Install [minecraft](https://minecraft.net/)

You will need to purchase the game.

3) Install [minecraft forge](https://files.minecraftforge.net/net/minecraftforge/forge/)

Download the installer for your operating system and minecraft version. After running it,
select the forge game version in the launcher and press play, letting it load until you
are presented with the main game menu.

4) Install [CC:Tweaked](https://www.curseforge.com/minecraft/mc-mods/cc-tweaked)

The minecraft directory is 

- Windows => %appdata%\.minecraft\
- Mac     => ~/Library/Application Support/minecraft/
- Linux   => ~/.minecraft/

It contains mods/, and you should put the cc-tweaked .jar file in there and restart.

5) Give yourself a turtle in-game

I recommend you create a survival mode world set to peaceful with cheats enabled. If so,
you can type `/give [yourname] computercraft:turtle_normal` (with tab-completion). Place it
and right click it to open an in-game terminal on the turtle.

6) Configure computercraft to allow http

Inside the minecraft directory, edit saves/[save_name]/serverconfig/computercraft-server.toml:

- Remove all [[http.rules]] blocks that say 'deny'
- (optional) Set need_fuel = false so that turtles can move without fuel

7) Copy (or symlink) lua/httpslave onto the turtle

In the minecraft directory, under saves/[save_name]/computercraft/computer/ there is a numbered
directory for every turtle in computercraft. You can copy lua/httpslave into that directory,
and then issue `httpslave` on the turtle's terminal. If you name the program startup instead,
you won't need to issue any command on the turtle terminal, it will start the program automatically.

During development you may instead wish to symlink into this repository, so that the script
automatically updates on the turtle. For instance:

```
ln -s $PWD/lua/httpslave ~/.minecraft/saves/cctweaked_mc1_16_4/computercraft/computer/0/httpslave
(or)
ln -s $PWD/lua/httpslave ~/.minecraft/saves/cctweaked_mc1_16_4/computercraft/computer/0/startup
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

