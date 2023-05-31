
## Development Log

### Day 1

Today is May 30th 2023, Spring Lisp Game Jam 2023 has started 4 days ago, 6 days left.

That means: I don't have much time, so I have to find something easy to achieve, and interesting to play with.

#### Save Pupu

How about this: 

There is a kitty whose name is Pupu, she is young and energetic, so she goes out everyday, and get lost. The player's task is to find Pupu in a maze, and take her back home.

#### The Maze

I don't know how to make a maze, or even how to represent one in Lisp, so I searched around, here is something I found:

1. https://en.wikipedia.org/wiki/Maze_generation_algorithm
2. https://github.com/keesiemeijer/maze-generator
3. https://github.com/joewing/maze/blob/master/maze.lisp

The first link will take me more than 1 day to understand and implement; the second one is in JavaScript, may take me more than half a day to understand and rewrite to Lisp; the third one, is in Lisp, so be it.

But, it does not have a typical open source license, and I am not a lawyer, here is its license:

https://github.com/joewing/maze/blob/20b9e7ddfdfac96cd43f11b52f14924ceb1c25bc/LICENSE

Can I use the code with GPL-2.0 or WTFPL? I don't know.

So I have to read and understand the code, and close the page, and write my own version.

Then I write my own version, and rendered it with [CALM](https://github.com/VitoVan/calm/):

![maze](README.assets/d1-maze-1.png)

This seems promising, but:

![maze](README.assets/d1-maze-2.png)

![maze](README.assets/d1-maze-3.png)



![maze](README.assets/d1-maze-4.png)

Sometimes it generates unreasonable maze. 

This may take me a week to debug, maybe it's just a very little detail, but still, I don't have time to debug this. So the current decision should be:

Choose some better looking maze, and hard code their data in my game.

I gotta go, life comes in, I will leave this task to day 2.

### Day 2

5 days left.

I need to finish the prototype:

- ~~player moving (already done by day 1)~~

- Canned Cat Food

  player can pick up the can first, then Pupu will follow the player, but only in a cute way, that Pupu will not find the right path, but try to cross the wall (but she won't)

- Shit

  if the player stepped on a piece of shit, Pupu will try move away from the player.

- Win / Loose

  Timed game? There are 4 mazes, 8 levels in total. 

  - the first 4 levels are in daytime, so the player just play as usual.
    - level 1: player + Pupu
    - level 2: player + Pupu + canned cat food
    - level 3: player + Pupu + shit
    - level 4: player + Pupu + canned cat food + shit
  - the later 4 levels are in nighttime, so the player will only see the 8? blocks around

- position

  the initial position of Pupu and the player should be placed meticulously, not randomly.

#### Abandon

The above plan was abandoned, the game was redesigned.

Because the cat food and shit parts makes me feel trivial and uninteresting to implement. They feel like some hard-coded features, may take a lot of time to implement and debug, so I left them.

The story of this game should be told like this:

> It was getting dark and darker, some kitties were lost in a maze, how many of them can you save before the sunset? Or, do you have the ability to "see" in the dark? Give it a try.

![maze](README.assets/d2-maze-1.png)

![maze](README.assets/d2-maze-2.png)

![maze](README.assets/d2-maze-3.png)

![maze](README.assets/d2-maze-4.png)

![maze](README.assets/d2-maze-5.png)

![maze](README.assets/d2-maze-6.png)

![maze](README.assets/d2-maze-7.png)

![maze](README.assets/d2-maze-8.png)

![maze](README.assets/d2-maze-9.png)

There is no limit of rounds, the player could try as many times as the player wants, but the surrounding light will be dimmer and dimmer, at last it will turn into complete darkness.

Can a player play in the darkness?

I think it's possible if the kitty could meow and there are sound effects when the player hits the wall or walks through the maze.

Let's leave them tomorrow.
