# 🤔⚒️: A Framework for Translating Natural Language to Protocol's Symbolic Model with LLMs⚙️

This repo contains a benchmark for symbolic model synthesis and a tool with web-based frontend.

## 💡 Introduction

This tool can generate formal specifications (symbolic model) for a protocol automatically from unstructed natural language, empowered by LLMs' powered ability for semantic parsing. Comparing with existing text-to-code tasks, we pay more attention on the soundness of the general translation process, i.e., the output of the tool should be consistent with the unstructed natural language description semantically. We try to make as much control as possible for the overall process, though "black-box" LLM is introduced.

The tool can be divided into two parts, parser and translator roughly. The parser translates input natural language into @$\lambda$ lambda calculus. To capture and resolve the ambiguity and incompleteness of natural language, it generates a set of sub-calculus corrresponding to the original language fragments and allow users to amend (including add, delete and edit) them manually. The generated @$\lambda$ is transformed to an executable code snippet, which is checked by applying some program analysis techniques further. Once it passes the checks, an alice&bob style specification will be extracted, then it is translated to Multiset Rewriting Rules (MSRs), which can be accepted by the state-of-art protocol verifier-Tamarin.

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

## 💬 User tutorial

Here gives an overivew for the general workflow of the tool. We use a toy example to illustrate how user can interact with the tool and how tool can generate formal specificaions and check the results automatically.


