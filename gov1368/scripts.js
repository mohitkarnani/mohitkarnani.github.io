document.addEventListener('DOMContentLoaded', function() {
    // Initialize Typed.js
    const options1 = {
        strings: [
            'Welcome to GOV 1368!'
        ],
        typeSpeed: 50, // Typing speed in milliseconds
        backSpeed: 25, // Backspacing speed in milliseconds
        backDelay: 1000, // Delay before starting backspace
        startDelay: 500, // Delay before starting typing
        loop: false, // Loop the text
        showCursor: false // Show the cursor
    };

    const options2 = {
        strings: [
            'The section material is below.'
        ],
        typeSpeed: 50, // Typing speed in milliseconds
        backSpeed: 25, // Backspacing speed in milliseconds
        backDelay: 1000, // Delay before starting backspace
        startDelay: 3250, // Delay before starting typing
        loop: false, // Loop the text
        showCursor: false // Show the cursor
    };

    new Typed('#typed-output1', options1);

    new Typed('#typed-output2', options2);

    // Handle user input
    const inputField = document.getElementById('terminal-input');
    const historyContainer = document.getElementById('command-history');

    inputField.addEventListener('keydown', function(event) {
        if (event.key === 'Enter') {
            const command = inputField.value;
            inputField.value = ''; // Clear input field
            
            if (command.trim()) {
                // Append user command to history
                const commandLine = document.createElement('div');
                commandLine.textContent = `. ${command}`;
                historyContainer.appendChild(commandLine);
                linebreak = document.createElement("br");
                historyContainer.appendChild(linebreak);
                // Ensure history container scrolls to the bottom
                historyContainer.scrollTop = historyContainer.scrollHeight;
            }
        }
    });
});
