eqlizr
======
makes hierarchies flat.
A simple mochiweb-server storing blip.fm feeds (RSS and public API Endpoints) into couchdb. 
This does **not** download any music, it's _just_ a metadata store.

Motivation
----------
um, the blip.fm web-interface sucks. I want to search my feeds, maybe build more than one playlist, perhaps even hear the music locally instead of youtube-streaming (given I have this song in my local music library). 

Requirements
------------
This is a dirty hack, don't expect it to run flawlessly.
Systemwide:
* Erlang OTP
* CouchDB (>= 0.10.1)

As project dependency:
* ecouch (in this case my (fork)[http://github.com/lennart/ecouch])
* mochiweb
* erlsom (for XML-Parsing)



Known Bugs
----------
When concurrently accessing stale feeds there's a race condition, since each request tries to create the feed and update all entries. It ends in a near recursive writing of "feeds" into blips
