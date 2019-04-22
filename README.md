# Breakdown Todo [![(try the live demo)](https://img.shields.io/badge/live%20demo-try%20it-brightgreen.svg)](https://y0hy0h.github.io/breakdown/)

[![build status](https://travis-ci.org/Y0hy0h/breakdown.svg?branch=master)](https://travis-ci.org/Y0hy0h/breakdown)

Divide and conquer your tasks.

# Development

After cloning the repository, you might need to initialize the [Git submodule] with `git submodule init && git submodule update`.

Install the dependencies with `yarn install`.

Launch a development server with live reload using the npm script.
```bash
yarn watch
```

To test just the Elm app, without a service worker's cache, run
```bash
yarn watch:elm
```

[Git submodule]: https://git-scm.com/book/en/v2/Git-Tools-Submodules