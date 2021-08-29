addEventListener("install", function (event) {
    event.waitUntil(
        caches.open("butter-pwa-v1").then(function (cache) {
            return cache.addAll([
                "/",
                "/build/main.js",
                "/manifest.json",
            ]);
        })
    );
});

addEventListener("fetch", function (event) {
    event.respondWith(
        caches.match(event.request).then(function (response) {
            return response || fetch(event.request);
        })
    );
});