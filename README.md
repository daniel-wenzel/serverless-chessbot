# Serverless Chessbot

Serverless chess bot written in Haskell. Try it out on my [website](http://wenzel.space/) (just scroll down until you see a chess board). The bot is deployed to the compute service AWS Lambda. The service executes the bot logic whenever a request is made to its HTTP API. That means this bot can be hosted maintenance free and without any running cost and the need of running a web server.

_Please note that is is not a UI and not a chess rules implementation. It is designed to work well with Chess.js and Chessboard.js. If this is what you are looking for have a look [here](http://chessboardjs.com/examples#5000)_.

## About the bot
I used to play chess in a chess club for a few years as a child but eventually stopped when I got older. Since then, I always liked to play a game when I had the opportunity. In the second year of college, we programmed chess bots which were to compete in a chess tournament by the end of the term. Together with two classmates, I created this bot and it won the tournament. Creating a program which was able to challenge me in something in which I had years of training was an amazing experience for me. One year later I learned about the theoretical background of the bot (Minimax algorithm + Alpha-Beta-Pruning) and decided to update the bot with this knowledge.

## Getting Started

### Prerequisites
You need three prerequisites to run this project.
1. An AWS Account in order to use AWS Lambda. AWS gives you 1 million of free Lambda requests, so running this project should not inflict any cost.
2. You need a Haskell compiler in order to compile the actual bot logic. Follow [this guide](https://serverless.com/framework/docs/providers/aws/guide/quick-start/) to install one.
3. For deploying the bot to AWS Lambda and exposing an HTTP API the Serverless framework is used. Get it [here](https://serverless.com/framework/docs/providers/aws/guide/installation/).

### Running the project
1. Build the bot logic. It lives in the folder /ai
```
cd ai
./build.sh
cd ..
```
2. Deploy the bot logic with the serverless framework
```
serverless deploy -v
```

3. In the output, you will see a link to the endpoint that looks like `https://{something}.execute-api.eu-west-1.amazonaws.com/dev/`. That's the endpoint of the chess bot you just deployed. Test it by executing (see API section for details):
```
curl "https://{something}.execute-api.eu-west-1.amazonaws.com/dev/move?color=w&fen=rnbqkbnr/ppppppp
p/8/8/8/8/PPPPPPPP/RNBQKBNR"
```
It will reply with a move:
```
"b2 b4"
```

4. When you now make changes and want to update the deployment, you can just run `./deploy.sh`

## How the Bot works
In chess, two people make moves in succession. All information is available, there is no hidden information like the cards on your hand in a poker game and no random factor like the dice in monopoly. The objective of the game is put the other player in checkmate, that means to beat the king piece of the other party. Your short term objective is to beat pieces of your opponent without losing your own pieces.

In chess, every piece has a [value](https://en.wikipedia.org/wiki/Chess_piece_relative_value) and we can score a game position by summing the values of all white pieces left on the board and substracting the value of all black pieces left on this board. The short-term objective of white is to make a move that leads to a position with a higher score, the objective of black is to make a move that leads to a position with a lower score. 

Both players move in succession. At first, white tries to find the move that maximizes the game score, then black tries to find the score that minimizes it. Afterward white tries to maximize it again and so on. This situation is common in game theory and can be solved with the [Minimax algorithm](https://en.wikipedia.org/wiki/Minimax).

It considers the goals of both parties and finds the move that lets to an optimal position a few moves ahead (in my implementation 4 moves). The standard minimax algorithm analyzes all possible move combinations, however, we can stop analyzing a move as soon as we know that it is worse than another move we found before. Further analyzing it to determine _how_ bad it is is just a waste of time. This concept is called [Alpha Beta Pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning).

### Shortcommings

The bot plays very strong on a short-term level, if its opponent made a mistake that leads to a short term piece gain, it will find it. However, it is very weak at an overall strategy for the game. Not it every move a piece can be won, in a lot of moves you need to bring your pieces in better positions so you have the opportunity to win something _later_. This long term process is called _strategy_ and requires a lot of implicit experience. I taught the bot some knowledge about a good position for pieces like the center of the board for pawns, but the current program is not able to understand a strategically good position. 

The bot does not consider en passant and castling because they require knowledge of the game history (e.g. castling is not allowed anymore when the king moved before) and were left out of the rules for the tournament for which the bot was originally written. Furthermore, it does not consider draws in any way.


## API
The program exposes a single HTTP endpoint */move?color={w|b}&fen={fenstring}*. It takes two parameters:
* color: Color of the active player. Allowed values are "w" or "b".
* fen: Fenstring of the current game position. Fenstrings are a standard for denoting chess positions. You can get the fen string for a chess position [here](https://lichess.org/editor). For this API, the parts of the fen string which consider en passant and castling must be removed. These parts are everything which follows after the first whitespace. For example for the starting position `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -` has to be changed to ´rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR´.

## Contributers:
Created by [Daniel Wenzel](https://wenzel.space/) with the help of [Roman Taypov](https://www.linkedin.com/in/roman-tayupov/) and [Paul Visser](https://www.linkedin.com/in/paul-visser-176a29119/).
