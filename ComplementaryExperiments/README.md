## Directories structure

```
📂 ComplementaryExperiments
├── 📂 src: the [Source Code](https://infsec.ethz.ch/research/software/anb.html) of the tool (Michel Keller, ETH)
│ ├── anb: Here we supply a execuable file after compliation. Use command `./anb test.anb`, the output is placed in test.spthy
│ ├── anb.o
│ └── ...
├── 📂 models
│ ├── nspk.anb: the Alice&Bob specification of the protocol
│ ├── nspk.spthy: Corresponding output (Tamarin model) of the tool
│ ├── ...
