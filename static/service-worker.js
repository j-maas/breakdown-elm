var CACHE = 'app-shell';

self.addEventListener('install', function (evt) {
    evt.waitUntil(precache());
});

function precache() {
    return caches.open(CACHE).then(function (cache) {
        return cache.addAll([
            '.'
        ]);
    });
}

self.addEventListener('fetch', function (evt) {
    evt.respondWith(fromCache(evt.request));
    evt.waitUntil(update(evt.request));
});

/** Open the cache where the assets were stored and search for the requested
  * resource. Notice that in case of no matching, the promise still resolves
  * but it does with `undefined` as value.
  */
function fromCache(request) {
    return caches.open(CACHE).then(function (cache) {
        return cache.match(request).then(function (matching) {
            return matching || Promise.reject('no-match');
        });
    });
}

function update(request) {
    return caches.open(CACHE).then(function (cache) {
        return fetch(request).then(function (response) {
            return cache.put(request, response);
        });
    });
}
