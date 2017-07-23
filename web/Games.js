const uuidv4 = require('uuid/v4');

const games={}

function newGame(userIsWhite) {
  let game={
    id: uuidv4(),
    userIsWhite: userIsWhite,
    moves: []
  }
}
