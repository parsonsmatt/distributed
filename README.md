# distributed

To build: `stack build`

To run: `stack exec -- distributed-exe`

Help text:

```bash
$ stack build && stack exec -- distributed-exe --help
Funtimes with Cloud Haskell!

Usage: distributed-exe --send-for DOUBLE --wait-for DOUBLE [--with-seed SEED]
  Run the distributed example

Available options:
  --send-for DOUBLE        How long to send messagers to neighbor nodes.
  --wait-for DOUBLE        How long to wait after sending messages.
  --with-seed SEED         The seed for the randon number generator
  -h,--help                Show this help text
```
