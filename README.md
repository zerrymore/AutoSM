# 🤔⚒️: A Framework for Translating Natural Language to Protocol's Symbolic Model with LLMs⚙️

This repo contains a benchmark for symbolic model synthesis and a tool with web-based frontend.

## 💡 Introduction

This tool can generate formal specifications (symbolic model) for a protocol automatically from unstructed natural language, empowered by LLMs' powered ability for semantic parsing. Comparing with existing text-to-code tasks, we pay more attention on the soundness of the general translation process, i.e., the output of the tool should be consistent with the unstructed natural language description semantically. We try to make as much control as possible for the overall process, though "black-box" LLM is introduced.

<!-- ![My Image](static/images/workflow.jpg){ width=50% } -->
<img src="static/images/workflow.jpg" style="width: 50%; height: auto;">

1. **Lccg:** a LLM-powered CCG parser, which takes protocol documents as input, parses them into lambda calculus expressions (that are defined specifically for modeling security protocols).
2. **L-repair:** which repairs the broken specifications with static analysis techniques and user interaction to make them well-formed.
3. **Algorithm T:** which transforms the lambda expressions into **Sapic+** specification **P**.
4. **Compiler:** which takes the well-formed **Sapic+** process **P** as input and compiles it into models **R** accepted by the protocol verifiers (**Tamarin**, **DeepSec**, and **ProVerif**) directly.


## 🛠️ Setup

1. Install Tamarin-prover
- Follow the [Tamarin manual](https://tamarin-prover.com/manual/master/book/002_installation.html).
    ```bash
    brew install tamarin-prover/tap/tamarin-prover
    ```
- Make sure the prover equipped with a Sapic+ [1] platform.


2. Setup the conda environments, and install the related packages.
    ```bash
    conda create -n llm4V python=3.10
    conda activate llm4V
    pip install -r requirements.txt
    ```

## 🚀 Get Started

- Configure openai API key in src/oai_key.txt,
- run the tool's frontend
    ```bash
    cd src
    python -m flask --app tool.py run
    ```
- add ```--debug``` for debug mode.
- Then open web-based tool at http://127.0.0.1:5000


## Directories structure
```
⚒️ AutoSM 
├── 📂 benchmark
│ ├── 📝 nsl.txt 
│ ├── ...
│ ├── 📜 nsl_verified.spthy
│ └── ...
├── 📂 src
| ├── 📂 gpt
| | ├── 📜 BNF
| | ├── ⚙️ parser.py
| | ├── ⚙️ analysizer.py
| | ├── ⚙️ translator.py
| | └── ⚙️ prompts.py
| └── 📂 utils
├── 📂 static
└── 📂 templates
  └── 📜 home.html
```

- 📂 benchmark
    - 📝 .txt: the protocol's description in natural langauge (**input** of our tool)
    - 📜 _verified.spthy: the complete **Sapic+** file including the symbolic model and the properties encoded in first-order-logic (**FOL**).

- 📂 src: the source code of our implementation
- 📂 static: static configurations including images and .css file.
- 📂 templates: html page of web-based frontend 

## 💬 User tutorial

Here gives an overivew for the general workflow of the tool. We use a toy example to illustrate how user can interact with the tool and how tool can generate formal specificaions and check the results automatically.


