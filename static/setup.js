// Register service worker
// Code taken from https://developers.google.com/web/fundamentals/primers/service-workers/
if ('serviceWorker' in navigator) {
    window.addEventListener('load', function () {
        navigator.serviceWorker.register('service-worker.js').then(function (registration) {
            // Registration was successful
            console.debug('ServiceWorker registration successful with scope: ', registration.scope);
        }, function (err) {
            // registration failed :(
            console.debug('ServiceWorker registration failed: ', err);
        });
    });
}

window.addEventListener('load', function () {
    var app = Elm.Main.init();
})