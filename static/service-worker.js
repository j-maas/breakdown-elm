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
    var cached = fromCache(evt.request);
    evt.respondWith(
        cached.catch(function () {
            return update(evt.request)
                .then(function () { return fromCache(evt.request); });
        })
    );

    /*  We want to update the cache, even if we found something. But we
     *  can't to it in respondWith, as that would delay the response.
     *  However, we can simply swallow the error case, because it means
     *  that we either did not hit the cache, which is handled in respondWith,
     *  or we had an error fetching the resource, in which case we can do nothing.
     */
    evt.waitUntil(
        cached.then(function () {
            return update(evt.request)
        }).catch(function () {
            return undefined;
        })
    );
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
        return fetch(request)
            .then(function (response) {
                return cache.put(request, response)
                    .then(function () { return response; });
            });
    });
}
