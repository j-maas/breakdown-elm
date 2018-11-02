// Register service worker
// Code taken from https://developers.google.com/web/fundamentals/primers/service-workers/
if ('serviceWorker' in navigator) {
    window.addEventListener('load', function () {
        navigator.serviceWorker.register('service-worker.js').then(function (registration) {
            console.debug('ServiceWorker registration successful with scope: ', registration.scope);
        }, function (err) {
            console.error('ServiceWorker registration failed: ', err);
        });
    });
}


window.addEventListener('load', function () {
    var STORAGE_KEY = "breakdown-state"
    // Restore state
    var stringState = this.localStorage.getItem(STORAGE_KEY);
    var state = stringState == null ? null : JSON.parse(stringState);

    var app = Elm.Main.init({
        flags: state
    });

    // saveRaw
    app.ports.saveRaw.subscribe(function (model) {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(model));
    })
})