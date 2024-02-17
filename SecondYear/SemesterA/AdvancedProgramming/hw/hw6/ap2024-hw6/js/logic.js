function setClickableTooltip(target, content){
  $( target ).tooltip({
    show: null, // show immediately
    position: { my: "center top", at: "center bottom+10"},
    content: content, //from params
    hide: { effect: "" }, //fadeOut
    tooltipClass: "bib_tooltip",
    close: function(event, ui){
      ui.tooltip.hover(
        function () {
          $(this).stop(true).fadeTo(400, 1);
        },
        function () {
          $(this).fadeOut("400", function(){
            $(this).remove();
          })
        }
      );
    }
  });
}

function listLastTenYears() {
  /**
   * function that helps get values for the dropDown in the exam form
   * @type {number}
   */
  let curYear = new Date().getFullYear();
  let yearSelect = document.getElementById('year');

  for (let i = curYear; i >= curYear - 9; i--) {
    let option = document.createElement('option');
    option.text = i;
    option.value = i;
    yearSelect.add(option);
  }
}

listLastTenYears();

function setupFormPopup() {
  /**
   * setting up the form popup
   *
   * @type {HTMLElement}
   */
  let wraper = document.getElementById('formWraping');
  let popup = document.getElementById('formPopup');
  let formBtn = document.getElementById('formBtn');

  // helper function to show the popup form
  function openFormPopup(event) {
    event.preventDefault(); // Prevent default form submission behavior
    wraper.style.display = 'block';
    popup.style.display = 'block';
  }

  // Event listener for the button click event
  formBtn.addEventListener('click', openFormPopup);

  // Function to close the popup form
  function closeFormPopup() {
    wraper.style.display = 'none';
    popup.style.display = 'none';
  }

  // will close the popup when clicking outside the form
  wraper.addEventListener('click', closeFormPopup);
}

// calling the setupFormPopup function to initialize the form popup
setupFormPopup();


function initializeMap() {
  /**
   * a function to initialize a map
   *
   */
  let map = L.map('map').setView([32.0853, 34.7818], 13); // set initial coordinates and zoom level

  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
    attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  }).addTo(map);

  // adding a marker to the map
  L.marker([32.0853, 34.7818]).addTo(map)
      .bindPopup('Hello, I am just a marker');
}

// calling the map function
document.addEventListener('DOMContentLoaded', function () {
  initializeMap();
});

// calling the initializeMap function
function setupGuessGame() {
  /**
   * guessing game
   *
   * @type {number}
   */
  // random number to guess
  let rndNum = Math.floor(Math.random() * 100) + 1;

  // add event listener for pressing Enter
  document.addEventListener("keyup", function(event) {
    if (event.key === 'Enter') {
      checkGuess(rndNum);
    }
  })

  document.getElementById('guessBtn').addEventListener('click', function() {
    checkGuess(rndNum);
  });

  // reset game function
  function resetGame() {
    document.getElementById('guessInput').value = '';
    document.getElementById('gameMsg').textContent = '';
    document.getElementById('guessInput').classList.remove('correctGuess');
    document.getElementById('guessInput').classList.remove('correctGuessClear');
  }

  // checking user's guess
  function checkGuess(randomNumber) {
    // get user's guess from the input
    const userGuess = parseInt(document.getElementById('guessInput').value);

    // check if the guess is correct
    if (userGuess === randomNumber) {
      showMessage('Congratulations! You guessed the correct number :)');
      document.getElementById('guessInput').classList.add('correctGuess');
      setTimeout(function() {
        resetGame();
        rndNum = Math.floor(Math.random() * 10) + 1; // Generate a new random number
      }, 2000); // Reset after 2 seconds
    } else if (userGuess < randomNumber) {
      showMessage('Too low!');
    } else {
      showMessage('Too high!');
    }
  }

  // helper function to display messages
  function showMessage(message) {
    document.getElementById('gameMsg').textContent = message;
  }
}

// calling the guess the number game function
document.addEventListener('DOMContentLoaded', function () {
  setupGuessGame();
});


