LLM-Aided Automatic Modelling for Security Protocol Verification
===
---

This repository corresponds to the artifact of our ICSE submission and consists of three main components:

- A new benchmark containing 18 protocol cases for evaluation.
- A self-contained tool with a web frontend.
- Other comparison experiments 
- Input-output log files which may help to ease the understanding of the workflow of the tool

First, we provide an overview of the directory structure of this repository. Then, we explain how to use the tool, followed by an example demonstrating how user interaction is involved. Finally, we provide a detailed description of the benchmark introduced in this work.


# Directories structure
```
📂 AutoSM
├── 📂 ComplementaryExperiments
├── 📂 Input_output
├── 📂 src
├── 📂 static
└── 📂 templates
```
- `ComplementaryExperiments`: Comparisons with one correct-by-construction approach
- `Input_output`: The examples used to present the workflow.
- `src`: the source code of our implementation
- `static`: static configurations including images and .css file.
- `templates`: html page of web-based frontend 


# Introduction

This tool can generate formal symbolic model for a protocol automatically from unstructed natural language, equipped with LLMs' powered ability for semantic parsing. Comparing with existing text-to-code tasks, we pay more attention on the trustworthiness
of the general translation process, i.e., the output of the tool should be consistent with the unstructed natural language description semantically. We try to make as much control as possible for the overall process (at least, provide some evidence of the trustworthiness for a non-expert user), though "black-box" LLM is introduced. 
The tool is composed of four stages, transitioning from natural language input to a Tamarin model:


1. Parser, a LLM-powered CCG parser, which takes protocol documents as input, parses them into lambda calculus expressions (that are defined specifically for modeling security protocols).
2. Repairer, which repairs the broken specifications with static analysis techniques and user interaction to make them well-formed.
3. Rewriter, which transforms the lambda expressions into Sapic+ [1] specification.
4. Compiler, which is designed and implemented by Cheval et al., taking the well-formed Sapic+ process as input and compiles it into models accepted by the protocol verifiers (Tamarin, DeepSec, and ProVerif) directly.

# How to use the tool?
---

## Setup

1. Install Tamarin-prover
- Follow the [Tamarin manual](https://tamarin-prover.com/manual/master/book/002_installation.html).
    ```bash
    $ brew install tamarin-prover/tap/tamarin-prover
    ```
- Make sure the prover equipped with a `Sapic+` platform.
- To check the installation of Tamarin, enter the command `tamarin-prover --version` in the command line. The output should resemble the following:
  ```
  tamarin-prover 1.8.0, (C) David Basin, Cas Cremers, Jannik Dreier, Simon Meier, Ralf Sasse, Benedikt Schmidt, 2010-2023

  This program comes with ABSOLUTELY NO WARRANTY. It is free software, and you
  are welcome to redistribute it according to its LICENSE, see
  'https://github.com/tamarin-prover/tamarin-prover/blob/master/LICENSE'.

  maude tool: 'maude'
  checking version: 2.7.1. OK.
  checking installation: OK.
  Generated from:
  Tamarin version 1.8.0
  Maude version 2.7.1
  ```

2. Setup the conda environments, and install the related packages.
    ```bash
    $ conda create -n llm4V python=3.10
    $ conda activate llm4V
    $ pip install -r requirements.txt
    ```

## Configuration

- Configure openai API key in file `src/conf/config.json`,
    ```json
    {
      "API_URL_BASE": "YOUR API URL BASE",
      "openai_api_key": "YOUR OPENAI KEY",
    }
    ```
- Run the tool's frontend
    ```bash
    $ cd src
    $ python -m flask --app tool run
    ```
**I recommend use ```python -m flask --app rewrite run``` to use the lastest version of the tool.**
- (Optional) Add ```--debug``` flag to enable debugging mode.
- Then open web-based tool at http://127.0.0.1:5000

## User example

Here we take protocol `CCITT X509.1` as example to show how user can use the tool generate symbolic model semi-automatically.

1. Load the protocol document.

2. Hire LLM as a parser to start reading the document, generating "reading logs" 

3. Check the parsing result with hints, which include the message sequence chart (MSC) to the protocol shown in the left panel.

4. Rewrite to the Sapic+ specification.


# How we construct the Benchmark?
---

Here, we provide more details about the benchmark introduced in this work. The benchmark consists of a set of three tuples ($N$, $\mathcal{P}$, $\mathcal{R}$), where $N$ represents the protocol description, $\mathcal{P}$ is the Sapic+ model, and $\mathcal{R}$ is the Tamarin model. We begin with $\mathcal{R}$, which is available in the Tamarin GitHub repository. To derive their corresponding $\mathit{N}$, we read the related documentation, such as RFCs, and extract and reformulate the core parts relevant to the model. As a result, some of the $N$ in our benchmark may not be identical to the original paper. For each given model $\mathcal{R}$, a set of safety properties $\Phi$ have been verified on $\mathcal{R}$. We manually build the Sapic+ model $\mathcal{P}$, ensuring that every property $\varphi \in \Phi$ which have been verified on $\mathcal{R}$ still holds on $\mathcal{P}$.

We use a table to document the sources of our protocol descriptions and how we reformulated the original content.

| No. | Protocol                     | Source                                                                                                                                   | Title                                                                                         | Authors                                             | Note                                                                                                                                                                                        |
|-----|------------------------------|------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|-----------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1   | Otway-Rees                   | [GIAC paper](https://www.giac.org/paper/gcih/81/man-in-the-middle-attack-initiator-otway-rees-key-exchange-protocol/100561)              | Otway-Rees Key Exchange Protocol Specification                                                |                                                     | We have rearranged the content order on page 6 of the original paper. Specifically, we have moved Table 2, which illustrates the messages in each step, to the description section.        |
| 2   | NSSK                         | [Wikipedia](https://en.wikipedia.org/wiki/Needham%E2%80%93Schroeder_protocol)                                                            | Needham-Schroeder Symmetric protocol                                                          |                                                     |                                                                                                                                                                                             |
| 3   | NSPK                         | [Teaching Assignment](https://members.loria.fr/VCortier/files/School/NS.pdf)                                                             | Description of the Needham Schroeder public key protocol and its attack                       | Véronique Cortier                                   |                                                                                                                                                                                             |
| 4   | KEMTLS                       | [CCS 2020](https://eprint.iacr.org/2020/534.pdf)                                                                                         | Post-Quantum TLS Without Handshake Signatures (Full version, March 15, 2022)                  | Peter Schwabe, Douglas Stebila, Thom Wiggers        | The protocol description is excerpted from Section 3, page 5 of the original paper.                                                                                                         |
| 5   | Example                      | [Tamarin-manual](https://tamarin-prover.com/manual/master/book/005_protocol-specification-rules.html)                                    |                                                                                               |                                                     |                                                                                                                                                                                             |
| 6   | SSH                          | [RFC 4253](https://datatracker.ietf.org/doc/html/rfc4253)                                                                                | The Secure Shell (SSH) Transport Layer Protocol                                               | T. Ylonen, C. Lonvick, Ed.                          | We only select Section 8 and Section 7.2 as input, excluding other parts. These sections are reordered to reflect the actual order of protocol execution; that is, first performing the key exchange (Section 8), followed by retrieving the key from shared keys (Section 7.2). |
| 7   | EDHOC                        | [draft-ietf-lake-edhoc-02](https://datatracker.ietf.org/doc/html/draft-ietf-lake-edhoc-02)                                               | A lightweight DH based key exchange                                                           | G. Selander, J. Mattsson, F. Palombini, Ericsson AB |                                                                                                                                                                                             |
| 8   | NAXOS                        | [Tamarin-manual](https://tamarin-prover.com/manual/master/book/005_protocol-specification-rules.html)                                    |                                                                                               |                                                     |                                                                                                                                                                                             |
| 9   | Toy                          | [Teaching material of Tamarin-prover](https://github.com/benjaminkiesl/tamarin_toy_protocol)                                             | Tamarin Toy Protocol                                                                          | Benjamin Kiesl                                      |                                                                                                                                                                                                 |
| 10  | Yahalom                      | [Wikipedia](https://en.wikipedia.org/wiki/Yahalom_(protocol))                                                                            | Yahalom Protocol                                                                              |                                                     |                                                                                                                                                                                             |
| 11  | Kao Chow Authentication v.1  | [Operating Systems Review](https://dl.acm.org/doi/pdf/10.1145/206826.206832)                                                             | An Efficient and Secure Authentication Protocol Using Uncertified Keys                        | I-Lung Kao and Randy Chow                           | The protocol description is excerpted from Section 2, page 2 of the original paper. We do not modify any contents given by the paper, but we moved the message format given in the Alice&bob notation next to the corresponding natural language description. |
| 12  | Sigfox                       | [ACSAC 2017](https://dl.acm.org/doi/10.1145/3134600.3134624)                                                                             | Automated Analysis of Secure Internet of Things Protocols                                     | Jun Young Kim, Ralph Holz, Wen Hu                   | The text is excerpted from Section 3.1, page 5 in the original paper. The original description contains some explanation about the `fact` and MSRs in Tamarin, which are excluded here.   |
| 13  | Neuman-Stubblebine           | [Wikipedia](https://en.wikipedia.org/wiki/Neuman%E2%80%93Stubblebine_protocol)                                                           | Neuman-Stubblebine protocol                                                                   |                                                     |                                                                                                                                                                                             |
| 14  | Woo-Lam                      | [International Workshop on Security In Information Systems](https://www.scitepress.org/PublishedPapers/2005/25570/25570.pdf)             | Analysing the Woo-Lam Protocol Using CSP and Rank Functions                                   | Siraj Shaikh and Vicky Bush                         |                                                                                                                                                                                             |
| 15  | SPLICE/AS                    | [IEICE Transactions on Information and Systems](https://www.scitepress.org/PublishedPapers/2005/25570/25570.pdf)                         | Design and Implementation of an Authentication System in WIDE Internet Environment            | Suguru Yamaguchi, Kiyohiko Okayama, Hideo Miyahara  | The protocol description is excerpted from Section 3.1, "Requesting a Server," which corresponds to Figure 3. The figure illustrates the message format exchanged between the parties, and this information has been integrated into the corresponding descriptive sentence. |
| 16  | CCITT X.509-1			           | [The Directory-Authentication Framework](https://1f8a81b9b0707b63-19211.webchannel-proxy.scarabresearch.com/rec/T-REC-X.509-198811-S/en) | CCITT X.509 (11/1988) AND Security Defects in CCITT Recommendation X .509                       | ITU                                                 | We focus on One-way authentication, Section 9.2. The original protocol description is located in page 18.                                                                 |
| 17  | Denning-Sacco                | [International Conference on Rewriting Techniques and Applications (RTA'11)](https://people.inf.ethz.ch/basin/pubs/rta11.pdf) | FAST: An Efficient Decision Procedure for Deduction and Static Equivalence                      | Bruno Conchinha, David Basin, Carlos Caleiro       | This protocol originates from [2]. But the protocol description is excerpted from [3], Section 4.3. Besides, we find an error in the Alice and Bob notation in [2], that has been corrected here. |
| 18  | LAK06_state                        | [SCIS06](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=262d8634634e0a227e4c2a7f3651baed4bf67707)                       | RFID Mutual Authentication Scheme based on Synchronized Secret Information                    | Sangshin Lee, Tomoyuki Asano, and Kwangjo Kim      | This is the raw protocol description excerpted the the orignial paper. The protocol contains a key chaining mechanism, which need loop to charactrize. |



## User tutorial

Here gives an overivew for the general workflow of the tool. We use a NSPK example to illustrate how user can interact with the tool and how tool can generate formal specificaions and check the results automatically.

<!-- ## Reference

[1] Cheval, Vincent, Charlie Jacomme, Steve Kremer, and Robert Künnemann. 2022. ''SAPIC+: Protocol Verifiers of the World, Unite!'' In 31st USENIX Security Symposium (USENIX Security 22), 3935–52.
[2] Wikipedia. "Needham–Schroeder protocol." https://en.wikipedia.org/wiki/Needham%E2%80%93Schroeder_protocol
[3] Global Information Assurance Certification Paper.
[4] Schwabe, Peter, Douglas Stebila, and Thom Wiggers. "Post-quantum TLS without handshake signatures." Proceedings of the 2020 ACM SIGSAC Conference on Computer and Communications Security. 2020.
[5] Tamarin Prover Manual. https://tamarin-prover.com/manual/.
[6] Ylonen, Tatu. "RFC 4253: The secure shell (SSH) transport layer protocol." (2006).
[7] Göran Selander, John Preuß Mattsson, and Francesca Palombini. "Ephemeral           Diffie-Hellman Over COSE (EDHOC)." Internet-Draft draft-ietf-lake-edhoc-02, Internet Engineering Task Force, May 6, 2021.
[8] Benjamin Kiesl. "Tamarin Toy Protocol." https://github.com/benjaminkiesl/tamarin_toy_protocol
[9] Wikipedia. "Yahalom (protocol)" https://en.wikipedia.org/wiki/Yahalom_(protocol)
[10] Kao, I-Lung, and Randy Chow. "An efficient and secure authentication protocol using uncertified keys." ACM SIGOPS Operating Systems Review 29.3 (1995): 14-21.
[11] Kim, Jun Young, et al. "Automated analysis of secure internet of things protocols." Proceedings of the 33rd Annual Computer Security Applications Conference. 2017.
[12] Wikipedia. "Neuman–Stubblebine protocol" https://en.wikipedia.org/wiki/Neuman%E2%80%93Stubblebine_protocol
[13] Shaikh, Siraj, and Vicky Bush. "Analysing the Woo-Lam protocol using CSP and rank functions." Journal of Research and Practice in Information Technology 38.1 (2006): 19-29.
[14] CCITT Recommendation X.509, The Directory - Authentication Framework, CCITT, December 1988.
[15] I'Anson, Collin, and Chris Mitchell. "Security defects in CCITT recommendation X. 509: the directory authentication framework." ACM SIGCOMM Computer Communication Review 20.2 (1990): 30-34.
[16] Conchinha, Bruno, David A. Basin, and Carlos Caleiro. "FAST: an efficient decision procedure for deduction and static equivalence." 22nd International Conference on Rewriting Techniques and Applications (RTA'11)(2011). Schloss-Dagstuhl-Leibniz Zentrum für Informatik, 2011.
[17] Denning, Dorothy E., and Giovanni Maria Sacco. "Timestamps in key distribution protocols." Communications of the ACM 24.8 (1981): 533-536.
[18]  -->


### References
1. Cheval, Vincent, Charlie Jacomme, Steve Kremer, and Robert Künnemann. 2022. **"SAPIC+: Protocol Verifiers of the World, Unite!"** In *31st USENIX Security Symposium (USENIX Security 22)*, 3935–52.

2. Wikipedia. **"Needham-Schroeder protocol."** https://en.wikipedia.org/wiki/Needham%E2%80%93Schroeder_protocol

3. Global Information Assurance Certification Paper.

4. Schwabe, Peter, Douglas Stebila, and Thom Wiggers. **"Post-quantum TLS without handshake signatures."** *Proceedings of the 2020 ACM SIGSAC Conference on Computer and Communications Security*, 2020.

5. Tamarin Prover Manual. https://tamarin-prover.com/manual/

6. Ylonen, Tatu. **"RFC 4253: The secure shell (SSH) transport layer protocol."** (2006).

7. Göran Selander, John Preuß Mattsson, and Francesca Palombini. **"Ephemeral Diffie-Hellman Over COSE (EDHOC)."** *Internet-Draft draft-ietf-lake-edhoc-02*, Internet Engineering Task Force, May 6, 2021.

8. Benjamin Kiesl. **"Tamarin Toy Protocol."** [https://github.com/benjaminkiesl/tamarin_toy_protocol](https://github.com/benjaminkiesl/tamarin_toy_protocol)

9. Wikipedia. **"Yahalom (protocol)"** https://en.wikipedia.org/wiki/Yahalom_(protocol)

10. Kao, I-Lung, and Randy Chow. **"An efficient and secure authentication protocol using uncertified keys."** *ACM SIGOPS Operating Systems Review* 29.3 (1995): 14-21.

11. Kim, Jun Young, et al. **"Automated analysis of secure internet of things protocols."** *Proceedings of the 33rd Annual Computer Security Applications Conference*, 2017.

12. Wikipedia. **"Neuman-Stubblebine protocol"** https://en.wikipedia.org/wiki/Neuman%E2%80%93Stubblebine_protocol

13. Shaikh, Siraj, and Vicky Bush. **"Analysing the Woo-Lam protocol using CSP and rank functions."** *Journal of Research and Practice in Information Technology* 38.1 (2006): 19-29.

14. CCITT Recommendation X.509, *The Directory - Authentication Framework*, CCITT, December 1988.

15. I'Anson, Collin, and Chris Mitchell. **"Security defects in CCITT recommendation X. 509: the directory authentication framework."** *ACM SIGCOMM Computer Communication Review* 20.2 (1990): 30-34.

16. Conchinha, Bruno, David A. Basin, and Carlos Caleiro. **"FAST: an efficient decision procedure for deduction and static equivalence."** *22nd International Conference on Rewriting Techniques and Applications (RTA'11)*, 
Schloss-Dagstuhl-Leibniz Zentrum für Informatik, 2011.

17. Denning, Dorothy E., and Giovanni Maria Sacco. **"Timestamps in key distribution protocols."** *Communications of the ACM* 24.8 (1981): 533-536.

18. Sangshin Lee, Tomoyuki Asano, and Kwangjo Kim. **"RFID mutual authentication scheme based on synchronized secret information."** SCIS 2006. Institute of Electronics, Information and Communication Engineers, 2006.