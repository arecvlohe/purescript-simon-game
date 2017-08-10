"use strict";

function audio(url) {
  return new Audio(url);
}

const red = audio("https://s3.amazonaws.com/freecodecamp/simonSound1.mp3");
const blue = audio("https://s3.amazonaws.com/freecodecamp/simonSound2.mp3");
const green = audio("https://s3.amazonaws.com/freecodecamp/simonSound3.mp3");
const yellow = audio("https://s3.amazonaws.com/freecodecamp/simonSound4.mp3");
const error = audio("/error.wav");

const sounds = {
  red: red,
  blue: blue,
  green: green,
  yellow: yellow,
  error: error
};

exports.play = function(sound) {
  return function() {
    sounds[sound].play();
  };
};
