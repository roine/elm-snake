const crypto = window.crypto || window.msCrypto
const getRandomInts = (n) => {
  const randInts = new Uint32Array(n)
  crypto.getRandomValues(randInts)
  return Array.from(randInts)
}

const randInts = getRandomInts(5)

var app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: [randInts[0], randInts.slice(1)]
})
// Initialize Firebase
var config = {
  // Unprotected, need to add a condom
  apiKey: "AIzaSyBHggFzovOj0xtDzYtK6H-haMdMSFlF0B4",
  authDomain: "snake-leaderboard-a3b3d.firebaseapp.com",
  databaseURL: "https://snake-leaderboard-a3b3d.firebaseio.com",
  projectId: "snake-leaderboard-a3b3d",
  messagingSenderId: "769091537994"
}
firebase.initializeApp(config)

var snakeRef = firebase.database().ref('/leaderboard')
snakeRef.on('value', function (snapshot) {
  var payload;
  if (snapshot.val()) {
    payload = Object.values(snapshot.val())

  } else {
    payload = []
  }
  console.log('updated')
  app.ports.leaderboard.send(payload)
})

app.ports.sendScore.subscribe(function (user) {
  var push = snakeRef.push()
  push.set(user)
})
