#!/usr/bin/env bash
# Sets your user account as the owner of the /usr/local directory to avoid managing packages with sudo
sudo chown -R $USER /usr/local
npm update
npm install brewer -g
npm install coffee-script -g
npm install less -g
npm install grunt-cli -g
npm install -g git-issues
# npm install jscoverage -g
# npm install mongoskin -g
# npm install nodemon -g
# npm install npm -g
#npm install phantomjs -g
# npm install uglify-js -g
# npm install uglifycss -g
npm install bower -g
npm install bunyan -g
# https://www.npmjs.org/package/cheater
npm install -g cheater
# Docco is a quick-and-dirty, hundred-line-long, literate-programming-style
# documentation generator. For more information, see: https://github.com/jashkenas/docco
npm install -g docco
