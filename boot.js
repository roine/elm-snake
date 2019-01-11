const crypto = window.crypto || window.msCrypto;
const getRandomInts = (n) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
};

const randInts = getRandomInts(5);

var app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: [randInts[0], randInts.slice(1)]
})
// Initialize Firebase
// TODO: Replace with your project's customized code snippet
var config = {
  apiKey: "AIzaSyBHggFzovOj0xtDzYtK6H-haMdMSFlF0B4",
  authDomain: "snake-leaderboard-a3b3d.firebaseapp.com",
  databaseURL: "https://snake-leaderboard-a3b3d.firebaseio.com",
  projectId: "snake-leaderboard-a3b3d",
  storageBucket: "snake-leaderboard-a3b3d.appspot.com",
  messagingSenderId: "769091537994"
}
firebase.initializeApp(config)

var snakeRef = firebase.database().ref('/leaderboard')
snakeRef.once('value').then(function (snapshot) {
    var payload = snapshot.val()
    app.ports.leaderboard.send(Array.from(payload))
  }
  , function (err) {
    console.log(err)
  })