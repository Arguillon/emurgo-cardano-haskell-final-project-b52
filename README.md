# emurgo-finale
Finale Haskell project for EMURGO Batch 52
The goal is to make a brick breaker game between 1st May and June 5th

To play the game
download the project
cabal build
cabal run

This is the flow we need to follow for the project:

Week 1 

— rendering function 
— change state
— change env

Week 2

— working on next function — collision detection & what to after that…
—  modelling the user input — construct — update next function based on user input
— run the game for a list of user inputs — eg. LLLRRRLLLLNNLLS


Week 3

— handle shared memory in environment 
— how to put and read input in shared memory — with manual input
— poll the list of user inputs from a specific location — using shared memory and async for user inputs — eg. LLLRRRLLLLNNLLS
— left right for board
— in case of no input, ball is moving
— add stop signal for stopping the name

Week 4 

— taking user input from keyboard
—  read user inputs from KB, put in shared memory, read from it and game should work on that (polling)

Week 5

— presentation


