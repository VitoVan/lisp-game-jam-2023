# The Maze and Lost Cat

## Spring Lisp Game Jam 2023

Submitted:

https://itch.io/jam/spring-lisp-game-jam-2023/rate/2101077

Game Page:

https://vitovan.itch.io/maze

## The Final Version

Please check the [afterwards](./afterwards/) directory, I fixed some bugs after the jam.

## Development Log

This is my development log for [Spring Lisp Game Jam 2023](https://itch.io/jam/spring-lisp-game-jam-2023), I started my work a bit late, but maybe I can finally make it.

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

![maze](README.assets/d1-maze-2.png)![maze](README.assets/d1-maze-3.png)![maze](README.assets/d1-maze-4.png)

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

![maze](README.assets/d2-maze-1.png)![maze](README.assets/d2-maze-2.png)![maze](README.assets/d2-maze-3.png)![maze](README.assets/d2-maze-4.png)![maze](README.assets/d2-maze-5.png)![maze](README.assets/d2-maze-6.png)![maze](README.assets/d2-maze-7.png)![maze](README.assets/d2-maze-8.png)![maze](README.assets/d2-maze-9.png)

There is no limit of rounds, the player could try as many times as the player wants, but the surrounding light will be dimmer and dimmer, at last it will turn into complete darkness.

Can a player play in the darkness?

I think it's possible if the kitty could meow and there are sound effects when the player hits the wall or walks through the maze.

Let's leave them tomorrow.


### Day 3

4 days left, actually, it's 3 days 20 hours left.

Sound effects added:

1. Kitty will meow, the closer you get, the louder the meow is
2. Walk will generate walking sound, if you hit the wall, there will be no sound
3. When you collected the kitty, there will be a special meow
4. When you successfully lead the kitty out of the maze, there will be purring.

Now kitty will follow the player, if they have met each other (overlapped), and some other small improvement on the avatars.

![d3-maze-1](README.assets/d3-maze-1.png)![d3-maze-2](README.assets/d3-maze-2.png)![d3-maze-3](README.assets/d3-maze-3.png)![d3-maze-4](README.assets/d3-maze-4.png)![d3-maze-5](README.assets/d3-maze-5.png)![d3-maze-6](README.assets/d3-maze-6.png)

Now, added dark mode, even in complete darkness, the game is also playable:

![d3-maze-dark](README.assets/d3-maze-dark-1.png) ![d3-maze-dark-2](README.assets/d4-maze-dark-2.png)

But it's far more difficult.

And the day can turn into night, the night can turn into day.

I should call it a day, tomorrow should be:

- doing more sound effect, like BGM?

- should add more user incitements? But how?
- random kitty position

Oh, I also added a `auto-move` feature, which will save the player some key presses by auto-moving ahead, but it's buggy, could move ahead to much, should be fixed tomorrow.


### Day 4

3 days 3 hours left.

Let's sort out today's work:

1. BGM
2. Random Kitty Location
3. Fix Auto-move feature (should pause at the intersection)
4. More user incitement

#### day 4 update - 1

Let's see what we have done:

1. BGM, done, using one of GarageBand sound pack loop forever

   it's good to know that all [sound pack in GarageBand is free as freedom and free-charge to use anywhere](https://support.apple.com/en-us/HT201808)

2. Auto-move fixed

What's left?

1. Random Kitty Location
2. More user incitement

I found a poem by Edward Lear, it's weird and interesting: *The Owl and the Pussycat*

Maybe I could use this in my game, slice it into small pieces, show one piece of it each round, print out all of it at the end.

#### day 4 update -2

I integrated Edward Lear's text into my game! As an user incitement, the parts of the poem will show up after the player finished a maze. I also added a finish page.

![d4-poem-page](README.assets/d4-poem-page.png)![d4-finish-page](README.assets/d4-finish-page.png)

#### day 4 update -2

I manually picked some nice spots for the kitties, and randomly pick from the list for the kitties to appear.

I think it's time to call it a day.

### Day 5

2 days left.

It's about time to wrap it up. So today I should:

- fix knowns bugs
- publish binaries
  - Linux
  - macOS
  - Windows
  - Web
- write documents

As for tomorrow, maybe I will send it to Jack as the first player, if he said something and I still have time, maybe I will fix it and re-publish again.

#### producing binaries

1

[CALM](https://github.com/VitoVan/calm/) is already capable of publishing binaries for Linux / Windows / macOS, albeit with a little higher OS requirements, like:

- on Linux, GCC should >= 2.33
- on macOS, only the most recent three version of macOS supported, the earliest one is Big Sur for now
- on Windows, it should be Windows 10 and 11

It might be enough for non-wizard users, but some BSD wizards won't be happy, so a Web version is a must-have. Of course, a document to show how to run from source is also needed.

2

Web version targets [JSCL](https://github.com/jscl-project/jscl/).

It turns out JSCL has a poor support for `array`:

https://github.com/jscl-project/jscl/issues/482

https://github.com/jscl-project/jscl/issues/127

But for this small game, `list` is more than enough, so I could rewrite `array` related stuff to `list`.

3

JSCL lacks of `equalp`, which is fine, I can compare manually. I hope one day I could close this issue:

https://github.com/jscl-project/jscl/issues/479

4

Add touch screen support, since some user may use their phone to play this game (within a web browser).

5

SDL2_Mixer on the browser performs weirdly, sound glitches during canvas repainting, why?

Maybe, threads?

By default, I disabled pthreads with `-s USE_PTHREADS=0` , but this makes the canvas painting and audio playing in the same thread, so while painting, the audio will be blocked.

No luck with thread enabled, maybe I didn't really enabled it, since my Cairo build was not built with pthread.

I'm not gonna build Cairo with pthread and try it for now, since it also requires extra [HTTP header](https://web.dev/coop-coep/) to be working, it's [overburden](https://github.com/gzuidhof/coi-serviceworker) for Github pages.

6

it's the double `loop` inside my `draw-*` functions, they loop the maze 13x13 times. It causes the browser hang. 

WTF?

13 x 13 = 169 loop will hang? I even tried empty `(format t "hang?~%")` inside the loop, it still blocks the browser.

7

Ah, I don't have time to debug the WebAssembly multithreading thing, so I made a workaround:

For all the audio in the browser, I call `new Audio()` to play it, like this in JSCL:

```lisp
#+jscl
(defun play-audio (audio-url &key (loop-audio-p nil) (volume 1))
  (format t "playing audio: ~A~%" audio-url)
  (let* ((audio-object-cache
           (cdr (assoc audio-url calm::*calm-state-loaded-audio* :test #'string=)))
         (audio-object
           (or audio-object-cache
               ;; https://github.com/jscl-project/jscl/wiki/JSCL-and-manipulations-with-JS-objects
               (#j:window:eval (concatenate 'string "new Audio('" audio-url "')"))
               )))
    (unless audio-object-cache
      (push (cons audio-url audio-object) calm::*calm-state-loaded-audio*))
    (when loop-audio-p
      (setf (jscl::oget audio-object "loop") t))
    (setf (jscl::oget audio-object "volume") volume)
    ((jscl::oget audio-object "play"))))
```

And it worked. So be it.

Life came in, I don't have too much time left, I must wrap it up and submit, today.


### Day 6

20 hours left, a huge bug was found in the Web version, fuck.

The game will hang at the end of fourth maze. Why?

```lisp
Welcome to JSCL (version 25e0341 built on 18 May 2023)

JSCL is a Common Lisp implementation on Javascript.
For more information, visit the project page at GitHub.

CL-USER> (let ((n 0) 
...         (x (list 'a 'b 'c 'd 'e 'f 'g))) 
...     (rotatef (nth (incf n) x) 
...              (nth (incf n) x) 
...              (nth (incf n) x)) 
...     x) 
ERROR: Function 'ROTATEF' undefined
CL-USER>  
```

When it's the fourth maze, I will randomise the maze list and give out one again. In the randomisation function:

```lisp
;; https://rosettacode.org/wiki/Knuth_shuffle#Common_Lisp
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)
```

I called `rotatef` which does not exist in [JSCL](https://github.com/jscl-project/jscl/issues/483).

What do I do?

Ah, ha! Delete this feature in Web version!

This is the shortest path to the new working version.

#### Another giant BUG!!!!

High CPU usage enven when idling!

Fixing and publishing!
