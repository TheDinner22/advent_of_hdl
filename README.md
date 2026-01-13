# free t-shirt

I'm competeing for a free t-shirt, which is super serious.

One of the rules is that I have to open source (feed AI) my code so here it is :P

## what is this

This is a (sort of WIP) set of hardcaml projects for aoc 2025

As I'm writing this, I have only done ***Day one Part 1***, but I'm planning on doing more.

```
Sadly, I made the mistake of playing the hit game Factorio and started spending my winter break playing that instead of doing more AOC challenges.
```

## docs

### folder structure

- d1p1 - day one part 1
- in general: dnpn = day n part n


Inside each folder there is a dune project and a pdf I wrote. 
The pdf documents my thought process, bugs encountered, opionions, etc.

### how to run

```
step zero: Follow the pretty awesome setup instructions for setting up hardcaml
```

1. cd into one of the folders
2. `dune runtest`
3. send me free t-shirt

### My thoughts after a little coding and a lot of factorio

I like hardcaml even if some of the ocaml syntax is mortifying for someone like me who is very used to C-style syntax and OOP.

My favorite part of using hardcaml is the test benches (expect tests are awesome). I also like that you can export a waveform so that larger simulations aren't constrained to the terminal (even if those external waveform viewers aren't always the best).

The documentation is awesome and I really like that the setup instructions worked (never happens).

In general, the fact that I have the low level control that HDL languages like VHDL and Verilog provide while not being stuck in 2008 is pretty outstanding.

#### Problems I feel like I had to solve myself

TLDR:
Most of my complaints are waveform stuff that probably comes from my lack of experiance with ocaml. I also think there could be better tooling for writing skeleton code.

Also the person who told me about this challenge said "hardcaml has a steep learning curve" which I think is generally a bad thing (and I'm not sure I 100% agree since it could be that ocaml has a steep learning curve).

Pretty frequently, some part of my design would compile and simulate but wouldn't behave as expected. I like to debug this by comparing different variables to a correct software implementation (per each clock cycle).

I ended up writing ~2 python scripts that extract different signals from vho files in different ways (each clock cycle, each change, etc.).

I feel like this is commen enough that hardcaml should have it as an option/feature. (at the same time I feel like it might have be easier to do this in ocaml during simulation, but idk)

I also feel like making new components/modules for a design could be streamlined with some kind of command line tool. Maybe a "make an and gate and tb for it" command and then I can go in and change the interface, test bench, and implementation.


