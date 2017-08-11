# Simon Game in PureScript

This is a replica project of the freeCodeCamp challenge <a target="_blank" href="https://www.freecodecamp.org/challenges/build-a-simon-game">Build a Simon Game</a>. The reason for doing this project was to gain a better understanding of PureScript and the front-end framework Pux. Pux takes many of it's ideas from the Elm architecture so having been familiar with Elm & React/Redux, I was, at least in the outset, comfortable with using this design pattern. There were plenty struggles along the way though, as PureScript's type system is much more expressive than Elm. That means there is a lot more overhead, however it pushed me to think harder about type systems and how valuable they are developing complex applications. It forced me to know what is happening at each step of the process, which is invaluable.

Special thank you to the PureScript community. I frequently looked for support from the PureScript folks in Slack and Gitter. Without their support I would not have been able to finish this project. Thank you!

## User Stories

As a user:

- I am presented with a random series of button presses
- Each time I input a series of button presses correctly, I see the same series of button presses but with an additional step
- I hear a sound that corresponds to each button when I click the button and when the series of buttons plays
- If I press the wrong button, I am notified I have done so and the series of button presses starts over
- I can see how many steps are in the current series
- If I want to restart, I can hit a button to do so, and I return to a single step
- I can play strict mode where if I get a button press wrong, the game notifies me and the game restarts the current random series from one
- I can win the game by getting a series of 20 steps correct. I am notified of my victory and I restart with a new random series of buttons presses
