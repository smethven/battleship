# CPSC 312 Project

Project Template for CPSC 312 Projects. Use this for both your proposal and final project submission.

(Since you are submitting a link to a specific commit, we will be able to tell the difference.)

The template to edit begins below. You can delete this header section once you've begun.

We will post some additional material to include when you reach the final project stage.

# BattleShips


We will be re-creating the classic popular board game of BattleShips. Playable through the command line
over the internet.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Nicholas Kao 47741301
+ Ruchit Palrecha ________
+ Skye Methven 11305109
+ Grant Sutton ________

We call ourselves: BattleShips

## Product Pitch


Replace this with a pitch for your project and the problem it solves. This is your vision for what the project
would like like as a complete product, ready for awesome action. (Yes, awesomeness seems to be a theme.)
It may be as short as a couple of paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Be sure that this touches clearly on the [project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html#project-requirements).

Good goals to aim for are from the top two rubric items for proposal grading:

> Exciting and language-appropriate product idea tackling a problem that is clearly compelling to a significant audience.

Or:

> Solid and language-appropriate product idea with a problem that is of real interest to the submitting team.

(It's easy to focus on the product and not the problem. Remember to include both!)

DRAFT(feel free to scrap if you have something better):

The Problem: We all know how fun it is to sink your friend’s battleship on game night, but what if we were living in a dystopian future where a virus has caused a global pandemic, and game night is never the same again. You must quarantine to keep safe but the human psyche relies on connections with others in order to keep sane. You haven’t sunk your friend’s battleship in weeks and you feel your sense of sanity slipping away. You just wish you could play your favourite game with your friend to connect without compromising on safety.

The Solution: Online battleship. We will harness the power of the internet to create an online battleship game. Our product will allow two people to connect from anywhere in the world, whether they are quarantining or simply separated by land and sea. No longer will game night or your sanity need to be compromised. Hosted on a website, our product with have a visually exciting UI complete with animated graphics of battleships sinking. It will also have a chat feature to fully support a game night experience. Written in Haskell, this product expands on our 312 learning by applying what we learned in Lecture 5 about representing games, and expanding it by incorporating networking to allow a multiplayer game. 


## Minimal Viable Project

Replace this with a description of the minimal viable project you will actually build for CPSC 312 (if this becomes your final project).
It may be as short as a few paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Make clear:
+ how this builds meaningfully toward your product pitch above, without being nearly as much work,
+ how it builds on the strength and power of the language, and
+ how it leads naturally to learning and applying some new element of the language (including what that element is!)

Good goals to aim for are from the top two rubric items for proposal grading:

> The minimal viable project (MVP) builds on the strengths and power of the language in exciting ways that will clearly lead to excellent learning for students.

Or:

> The MVP clearly builds significantly on the language and will lead in interesting and natural ways to learning for the students.

Our MVP will be a command line game in which a user will be able to play a game of battleships, through the internet.
We will have create a standard language in which users can interface with our game and get feedback on the result of their move. 
Additionally we will communicate and maintain consistency between the two players game states throughout play.

## Proof of Concept

Replace this with a description of your proof-of-concept. This may be as short as a few paragraphs, or it may be longer.
It should **definitely** take less than 4 minutes to read carefully and thoroughly, though working through and running the
code may take an extra 4 minutes. (Your guidance and links should make it easy for us to work through the code.)

Tell us:

+ what key element of your project the proof-of-concept focuses on
+ what makes that such an important element
+ how completing this gives you confidence that, with sufficient work, you could complete the full (minimal viable) project


Include links (likely even line-level links, which are easy to create in Github) throughout to critical pieces of
the code to make it easy for us to understand what you've accomplished and how it fulfills the requirements.

Also include instructions for us to test and run your code. (See our guidelines below.)

A good goal to aim for is the top rubric item from proposal grading:

> Fully functional proof-of-concept is easy to use and review, and it clearly demonstrates a key element necessary for the overall project.



### How to test and run the code: Haskell

Replace this section with instructions to us for how to test and run your code.

As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

### How to test and run the code: Prolog

Replace this section with instructions to us for how to test and run your code.

Instructions coming soon, but we expect you'll use the [Prolog Unit Testing](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) library for testing and that we'll be able to run your code with `swipl`.


