document.addEventListener('DOMContentLoaded', () => {
    const content = document.getElementById('content');
    const aboutBtn = document.getElementById('about');
    const albumBtn = document.getElementById('album');
    const funnyBtn = document.getElementById('funny');
    const add1Btn = document.getElementById('add1');

    aboutBtn.addEventListener('click', () => {
        content.innerHTML = '<h2>About Me</h2><p>' +
            'I am a computer science student at Reichman University. Prior to that, I was working full time in the tech' +
            ' industry for Vayyar. In the army I served in the technological unit of the intelligence department. ' +
            'In high-school I majored in computer science and physics in Tichonet. Also, took part in the FRC program ' +
            'of FIRST (for inspiration and recognition of science and technology).</p>';
    });

    albumBtn.addEventListener('click', () => {
        content.innerHTML = '<h2>Album</h2><img src="images/me.png" alt="image of Orian">';
    });

    funnyBtn.addEventListener('click', () => {
        content.innerHTML = '<h2>Funny</h2><img src="images/meme.png" alt="meme about js">';
    });

    // handling the count button. adding 1 for each click
    add1Btn.addEventListener('click', async () => {
        const response = await fetch('/api/count', { method: 'POST' });
        const data = await response.json();
        content.innerHTML = `<h2>Count</h2><p>You clicked ${data.count} times</p>`;
    });

    (async () => {
        const response = await fetch('/api/count');
        const data = await response.json();
        content.innerHTML = `<h2>Count</h2><p>You clicked ${data.count} times</p>`;
    })();
});
