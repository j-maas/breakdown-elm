window.addEventListener('load', function() {
    var STORAGE_KEY = "breakdown-state"
    // Restore state
    var stringState = this.localStorage.getItem(STORAGE_KEY);
    var state = stringState == null ? null : JSON.parse(stringState);

    var app = Elm.Main.init({
        flags: state
    });

    // saveRaw
    app.ports.saveRaw.subscribe(function(model) {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(model));
    })
})