# BattleShips

We will be re-creating the classic popular board game of Battleship, making it playable through the command line
over the internet.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Nicholas Kao 47741301
+ Ruchit Palrecha 40913857
+ Skye Methven 11305109
+ Grant Sutton 79229522

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

DRAFT:

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


### Draft

Our proof of concept is, at minimum, opening a socket and listening for connections, accepting a connection, sending a message, and closing the socket.

This addresses the networking component of our project, which one of the more complicated aspects of our idea since networking is a side-effect and Haskell is pure. Without networking, our project is reduced to a basic, local representation of Battlehip through the command line. That's just boring. With networking, two players can be anywhere in the world and still connect to each other for a game. That's pretty cool!

Now that we have a basic working server/socket accepting connections, we are confident that we can implement all the necessary networking (client, expanded server) to create a simple, turn-based, networking applicaiton that allows us to initialize a game state and send updates between the players.

Server socket creation: This function is the entry point to the server. It binds several helpers together.
https://github.students.cs.ubc.ca/grantms/cpsc-312-battleship/blob/6699968a3e07c5593f941c5d537627dd901012a7/pvp-battleship/src/Lib.hs#L38  

This function gets the proper info to create the socket, like the address family, which addresses to accept connections from, etc.  
https://github.students.cs.ubc.ca/grantms/cpsc-312-battleship/blob/6699968a3e07c5593f941c5d537627dd901012a7/pvp-battleship/src/Lib.hs#L11

This function creates the socket, sets some options like one for easier port reuse, binds the socket to the port, and listens for incoming connections.
https://github.students.cs.ubc.ca/grantms/cpsc-312-battleship/blob/6699968a3e07c5593f941c5d537627dd901012a7/pvp-battleship/src/Lib.hs#L20

This function accepts a connection, sends "Hello World!" to it, and closes the socket.
https://github.students.cs.ubc.ca/grantms/cpsc-312-battleship/blob/6699968a3e07c5593f941c5d537627dd901012a7/pvp-battleship/src/Lib.hs#L31

### How to test and run the code: Haskell

**Since our proof of concept sets up a socket, the best way to test it is running our program and connecting with a network utility like netcat.**

> Note: To fully see a connection, this should be done on the department's Linux servers or a Mac. While the Network.Socket Module works with Windows via msys, connecting to the opened port with a tool like netcat works most easily on Linux or Unix. On Windows/without netcat, you will only be able to see the print statements that occur after networking functions have been called and completed without throwing errors rather than the message sent when a connection is made.

1. Clone this repo onto a Mac or a department Linux server. From inside the main directory, `cd haskell`
2. Run `stack build` and then `stack exec battleship-exe`
3. On another terminal on the same machine (for the simplicity of using 'localhost' rather than identifying the ip address of the server), run `nc localhost 3000`
4. Observe the message sent to the client!
> Note that the use of `localhost` still indicates a true network connection has been created, as the traffic is still traveling down through the Transport layer to the IP layer and back up.

You can still run `make haskell-eval` from root or `stack test`, but because it is hard (not possible?) to test a network connection with Tasty, there are no informative tests as of yet.