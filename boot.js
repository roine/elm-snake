const crypto = window.crypto || window.msCrypto
const getRandomInts = (n) => {
  const randInts = new Uint32Array(n)
  crypto.getRandomValues(randInts)
  return Array.from(randInts)
}

const randInts = getRandomInts(5)

var app = Elm.Main.init({
  node: document.getElementById('app')
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
  var payload = snapshot.val()
  if (!payload) {
    app.ports.leaderboard.send([])
  }else{
    var tupledPayload = Object.keys(payload).map(k => [k, payload[k]])
    app.ports.leaderboard.send(tupledPayload)
  }

})

app.ports.sendScore.subscribe(function (userPayload) {
  var [id, user] = userPayload
  if (id === "") {
    var push = snakeRef.push()
    app.ports.sentScore.send([push.getKey(), user])
    push.set(user)
  } else {
    var update = {}
    update[id] = user
    snakeRef.update(update)
  }
})
