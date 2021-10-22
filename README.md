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

The Problem: We all know how fun it is to sink your friend’s battleship on game night, but what if we were living in a dystopian future where a virus has caused a global pandemic, and game night is never the same again. You must quarantine to keep safe but the human psyche relies on connections with others in order to keep sane. You haven’t sunk your friend’s battleship in weeks and you feel your sense of sanity slipping away. You just wish you could play your favourite game with your friend to connect without compromising on safety.

The Solution: Online battleship. We will harness the power of the internet to create an online battleship game. Our product will allow two people to connect from anywhere in the world, whether they are quarantining or simply separated by land and sea. No longer will game night or your sanity need to be compromised. Hosted on a website, our product with have a visually exciting GUI complete with animated graphics of battleships sinking. It will also have a chat feature to fully support a game night experience. The user will be able to place their ships on our oceanic game board, and taunt their friends over chat about their brilliant strategy. They can even invite more friends to join as spectators of the game who will also have the ability to chat and share their thoughts. Or, if you’re looking to make brand new connections, you can join a game with a random player from anywhere in the world and get to know one another over battleship. Written in Haskell, this product expands on our 312 learning by applying what we learned in Lecture 5 about representing games, and expanding it by incorporating networking to allow a multiplayer game. Networking is a vital part of computing that we will learn to use in Haskell for this project. 

## Minimal Viable Project

Our MVP will be a command line game in which a user will be able to play a game of battleships, through the internet. Our MVP will set up the game board for our players, and we will create a standard language in which users can interface with our game and get feedback on the result of their move. Additionally we will communicate and maintain consistency between the two players game states throughout play. This meaningfully builds towards our product as we will be creating a fully playable, 2-player game of battleship. We will utilize Haskell’s functional power to represent game states and modify those game states, and we have seen in class that Haskell is great at this task. As we will be making an online multiplayer game, naturally we will learn how to apply networking to Haskell. 

## Proof of Concept

Our proof of concept is, at minimum, opening a socket and listening for connections, accepting a connection, sending a message, and closing the socket.

This addresses the networking component of our project. Without networking, our project is reduced to a basic, local representation of Battlehip through the command line. That's just boring. With networking, two players can be anywhere in the world and still connect to each other for a game. That's pretty cool!

Now that we have a basic working server/socket accepting connections, we are confident that we can implement all the necessary networking (client, expanded server) to create a simple, turn-based, networking applicaiton that allows us to initialize a game state and send updates between the players.

Server socket creation: This function is the entry point to the server. It binds several helpers together.
https://github.students.cs.ubc.ca/grantms/cpsc-312-battleship/blob/71a39d40e931e1fd329b050ef5b5ef5eddd7a31e/haskell/src/Lib.hs#L38

This function gets the proper info to create the socket, like the address family, which addresses to accept connections from, etc.  
https://github.students.cs.ubc.ca/grantms/cpsc-312-battleship/blob/71a39d40e931e1fd329b050ef5b5ef5eddd7a31e/haskell/src/Lib.hs#L11

This function creates the socket, sets some options like one for easier port reuse, binds the socket to the port, and listens for incoming connections.
https://github.students.cs.ubc.ca/grantms/cpsc-312-battleship/blob/71a39d40e931e1fd329b050ef5b5ef5eddd7a31e/haskell/src/Lib.hs#L20

This function accepts a connection, sends "Hello World!" to it, and closes the socket.
https://github.students.cs.ubc.ca/grantms/cpsc-312-battleship/blob/71a39d40e931e1fd329b050ef5b5ef5eddd7a31e/haskell/src/Lib.hs#L31

### How to test and run the code: Haskell

**Since our proof of concept sets up a socket, the best way to test it is running our program and connecting with a network utility like netcat.**

> Note: To fully see a connection, this should be done on the department's Linux servers or a Mac. While the Network.Socket Module works with Windows via msys, connecting to the opened port with a tool like netcat works most easily on Linux or Unix. On Windows/without netcat, you will only be able to see the print statements that occur after networking functions have been called and completed without throwing errors rather than the message sent when a connection is made.

1. Clone this repo onto a Mac or a department Linux server. From inside the main directory, `cd haskell`
2. Run `stack build` and then `stack exec battleship-exe`
3. On another terminal on the same machine (for the simplicity of using 'localhost' rather than identifying the ip address of the server), run `nc localhost 3000`
4. Observe the message sent to the client!
> Note that the use of `localhost` still indicates a true network connection has been created, as the traffic is still traveling down through the Transport layer to the IP layer and back up.
