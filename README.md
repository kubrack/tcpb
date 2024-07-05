### Quick start
```
opam switch create 4.14.0
opam switch 4.14.0
opam install lwt
git clone https://github.com/kubrack/tcpb.git && cd tcpb
dune build && ./_build/install/default/bin/tcpb <num_of_clients>
# in another console:
tail -f log
```
