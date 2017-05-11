# reduelctl

`reduelctl` is a script to manage a pool of Red Eclipse servers. It
starts one more server from the pool whenever a number of players on
each running server reaches a specified threshold. When players leave
it stops all empty servers and keeps at least one that is non-full.

Configuration file is in `ini` format. See `example.ini`.
