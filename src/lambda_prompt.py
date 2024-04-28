
sys_SeqReader_prompt = """\
You are a Lambda Calculus Expression expert in cryptographic protocol field. Now you need to parse texts into lamba calculus. \
There are some Lambda Calculus Expression template you may use: 
1. `Send(A:agent, B:agent, m:msg)`: Agent A sends message m to agent B. The `Send` function is limited to 3 arguments.
2. `Recv(B:agent, A:agent, n:msg)`: Agent B receives message n from agent A.
3. `Gen(A:agent, n:nonce)`: Agent A generates a fresh nonce n.
4. `Knows(A:agent, n1:nonce, n2:nonce, ...)`: Agent A initially knows items n1, n2, etc., which are infrastructure components like keys.
5. `Op(A:agent, func(msg))`: Agent A performs operations like aenc/adec/concat on messages to produce a new message m. \
The `func` in `Op` must match the corresponding predicate described in natural language. \
    - Op(A, assign(var, msg)): Message construction, agent A constructs a message `msg` and binds it to variable var
    - Op(A, bind(t1, t2)): Message deconstruction, agent A deconstructs a message `t2` to a new term `t1` 
Note: Expressions listed above cannot serve as arguments to `Op`; for example, `Op(R, Gen/Send/, ...)` is prohibited.

For each `Send`, there should be a corresponding `Recv`.
A message should be binded to a variable before sending it.

Your task is to complete each expression by directly filling in the placeholders denoted by /* >>The lambda calculus << */. \
Do not include any explanations, comments, or additional text outside of the required expressions.
"""


first_SeqReader_question = """\
Alice and Bob shares a symetric key Kab initially.
/* >>The lambda calculus << */
"""

first_SeqReader_answer = """\
/* 
Knows(role(Alice), Kab)
Knows(role(Bob), Kab)
*/
"""

second_SeqReader_question = """\
Alice and Bob shares a symetric key Kab initially.
/* 
Knows(role(Alice), Kab)
Knows(role(Bob), Kab)
*/
Bob generates a fresh nonce m and computes H as the hash of m \
with its identity. Then Bob sends it to Alice.
/* >>The lambda calculus << */
"""


second_SeqReader_answer = """\
/* 
Gen(Bob, m)
Op(Bob, assign(H, hash(concat(m, idB))))
Op(Bob, assign(cypher, senc(H, Kab)))
Send(Bob, Alice, cypher)
Recv(Alice, Bob, cypher)
*/
"""

third_SeqReader_question = """\
Alice starts the protocol by sending her identity A together with a freshly generated random number Na. \
This message is encrypted using an asymmetric encryption algorithm with B's public key (denoted pub(B)).
/* >>The lambda calculus << */
"""

third_SeqReader_answer = """\
/* 
Gen(Alice, Na)
Op(Alice, assign(message1, aenc(concat(idA, Na), pkB)))
Send(Alice, Bob, message1)
*/
"""

fourth_SeqReader_question = """\
Alice starts the protocol by sending her identity A together with a freshly generated random number Na. \
This message is encrypted using an asymmetric encryption algorithm with B's public key (denoted pub(B)). 
/* 
Gen(Alice, Na)
Op(Alice, assign(message1, aenc(concat(idA, Na), pkB)))
Send(Alice, Bob, message1)
*/
We suppose that only agent Bob (whose identity is B) knows the secret key corresponding to pub(B). \
Next Bob receives the message {A, Na}pub(B) sent by Alice. 
/* >>The lambda calculus << */
"""

fourth_SeqReader_answer = """\
/* 
Knows(role(Bob), pkA, pkB)
Recv(Bob, Alice, message1)
Op(Bob, adec(message1, skB))
*/
"""

fifth_SeqReader_question = """\
We suppose that only agent Bob (whose identity is B) knows the secret key corresponding to pub(B). \
Next Bob receives the message {A, Na}pub(B) sent by Alice.
/* 
Knows(role(Bob), pkA, pkB)
Recv(Bob, Alice, message1)
Op(Bob, adec(message1, skB))
*/
Using his private key, Bob decrypts the message. He sends the received nonce Na together with a freshly generated nonce Nb \
encrypted with A's public key (pub(A)) to Alice. Finally Alice receives the message {Na, Nb}pub(A).
/* >>The lambda calculus << */
"""

fifth_SeqReader_answer = """\
/* 
Op(Bob, adec(message1, skB))
Gen(Bob, Nb)
Op(Alice, assign(message2, aenc(concat(idA, Na), pkB)))
Send(Bob, Alice, message2)
Recv(Alice, Bob, message2)
*/
"""

sixth_SeqReader_question = """\
Using his private key, Bob decrypts the message. He sends the received nonce Na together with a freshly generated nonce Nb \
encrypted with A's public key (pub(A)) to Alice. Finally Alice receives the message {Na, Nb}pub(A).
/* 
Op(Bob, adec(message1, skB))
Gen(Bob, Nb)
Op(Alice, assign(message2, aenc(concat(idA, Na), pkB)))
Send(Bob, Alice, message2)
Recv(Alice, Bob, message2)
*/
Then She sends the nonce Nb to Bob encrypted with Bob's public key.
/* >>The lambda calculus << */
"""

sixth_SeqReader_answer = """\
/* 
Op(Alice, assign(cypher2, aenc(Nb, pkB)))
Send(Alice, Bob, cypher2)
Recv(Bob, Alice, cypher2)
*/
"""


sys_repair_lambda_prompt = """\
As an expert in security protocol analysis, please review and refine the following lambda expressions:

1. Message Reception Recv(A, B, msg):
  - Identify Knowledge Set: Establish the knowledge set of A.
  - Message Parsing:
      If msg is decipherable using A's knowledge set, apply Op(A, bind(t, msg)) to unpack the message.
      If not, assign an opaque variable.

2. Message Sending Send(A, B, msg):
  - Identify Knowledge Set: Determine the knowledge set of A.
  - Message Construction:
      If msg can be composed using A's knowledge set, proceed as normal.
      If not, check for an equivalent variable in A's knowledge set and replace msg with this variable.

3. Message Construction Op(A, assign(msg, f(t1, t2...)))
  - Identify Knowledge Set: Determine the knowledge set of A.
  - Message Construction:
      If variable msg can be composed using A's knowledge set, proceed as normal.
      If not, abandon this statement, i.e., add a `-` prefix. For example `- Op(A, mess, aenc(Na, skA))` where aenc(Na, skA) can not be composed using A's knowledge set
      
3. Knowledge Acquisition via Message Reception:
  - Update Knowledge: When A receives a message, A's knowledge set is expanded to include all variables in the message.
  - Example: Post Recv(A, B, <m, na, aenc(nb, k)>), A's knowledge set, initially {Kas, idA, idB}, updates to {Kas, idA, idB, m, na, nb, k}.
"""


repair_prompt_template = """\
<The spec I give you>
"""



first_shot_lambda_repair_question = """\
Knows(role(A), skA, pkA, pkB)
Knows(role(B), skB, pkA, pkB)
Gen(A, Na)
Op(A, assign(cypher1, aenc(concat(Na, pkA), pkB)))
Send(A, B, cypher1)
Recv(B, A, cypher1)
Gen(B, Nb)
Op(A, assign(cypher2, aenc(concat(Na, Nb), pkA)))
Send(B, A, cypher2)
Recv(A, B, cypher2)
Send(A, B, aenc(Nb, pkB))
Recv(B, A, aenc(Nb, pkB))
"""


first_shot_lambda_repair_answer = """\
  Knows(role(A), skA, pkA, pkB)          // Role A initial Knowledge set MA = { skA, pkA, skB }
  Knows(role(B), skB, pkA, pkB)          // Role B initial Knowledge set MB = { skB, pkA, pkB }
  Gen(A, Na)
  Op(A, assign(cypher1, aenc(concat(Na, pkA), pkB)))
  Send(A, B, cypher1)                       // cypher1 = aenc(concat(Na, pkA), pkB)
  Recv(B, A, cypher1)                       
+ Op(B, bind(mess, adec(cypher1, skB)))   // cypher1 is encrypted with pkB, which can be decrypted by B with skB in MB
+ Op(B, bind(concat(Na, pkA), mess))      // mess1 is a 2-tuple, which can parsed further
+ // Na, pkA can not be parsed further
  Gen(B, Nb)
  Op(A, assign(cypher2, aenc(concat(Na, Nb), pkA)))
  Send(B, A, cypher2)                       // cypher2 = aenc(concat(Na, Nb), pkA)
  Recv(A, B, cypher2)                      
+ Op(A, bind(mess2, adec(cypher2, skA))) // cypher2 is encrypted with pkA, which can be decrypted by A with skA in MA
+ Op(A, bind(concat(Na, Nb), mess2))     // mess1 is a 2-tuple, which can parsed further
+ // Na, Nb can not be parsed further
  Send(A, B, aenc(Nb, pkB))
  Recv(B, A, aenc(Nb, pkB))
"""


second_shot_lambda_repair_question = """\
Knows(role(A), Kas, idA)
Knows(role(S), Kas, idA)
Gen(A, Na)
Op(A, assign(mess1, senc(Na, Kas)))
Send(A, S, cypher1)
Recv(S, A, cypher1)
Gen(S, Kab)
Gen(S, Nb)
Op(S, assign(cypher, <Nb, senc(<Na, Nb>, Kab)>))
Send(S, A, cypher)
Recv(A, S, cypher)
Op(A, assign(mess2, senc(<Na, Nb>, Kab)>)))
Send(A, S, mess2)
"""

second_shot_lambda_repair_answer = """\
  Knows(role(A), Kas, idA)                   // Role A initial Knowledge set MA = { Kas, idA }
  Knows(role(S), Kas, idA)                   // Role S initial Knowledge set MS = { Kas, idA } 
  Gen(A, Na)
  Op(A, assign(cypher1, senc(Na, Kas)))
  Send(A, S, cypher1)                     // cypher = <Nb, senc(<Na, Nb>, Kab)> 
  Recv(S, A, cypher1)                     // cypher1 = senc(Na, Kas)
+ Op(S, bind(Na, adec(cypher1, Kas)))   // cypher1 is encrypted with Kas, which can be decrypted by S with Kas in MS
+ // Na can not be parsed further
  Gen(S, Kab)
  Gen(S, Nb)
  Op(S, assign(cypher, <Nb, senc(<Na, Nb>, Kab)>))
  Send(S, A, cypher)                     // cypher = <Nb, senc(<Na, Nb>, Kab)> 
  Recv(A, S, cypher)                     // cypher = <Nb, senc(<Na, Nb>, Kab)> 
+ Op(A, bind(<Nb, mess>, cypher))      // cypher is a 2-tuple, which can parsed further.
+ // mess = senc(<Na, Nb>, Kab), which is encrypted with Kab, but Kab is not in MA, mess is opaque to A.
- Op(A, assign(mess2, senc(<Na, Nb>, Kab)>)))  // message senc(<Na, Nb>, Kab)>) can not be constructed by A, 
- Send(A, S, mess2)
+ Send(A, S, mess)       // A has obtained a new variable mess from cypher, which equals to mess2.
"""


seq_ret_list = [
    {'role': 'system', 'content': sys_SeqReader_prompt},
    {'role': 'user', 'content': first_SeqReader_question},
    {'role': 'assistant', 'content': first_SeqReader_answer},
    {'role': 'user', 'content': second_SeqReader_question},
    {'role': 'assistant', 'content': second_SeqReader_answer},
    {'role': 'user', 'content': third_SeqReader_question},
    {'role': 'assistant', 'content': third_SeqReader_answer},
    {'role': 'user', 'content': fourth_SeqReader_question},
    {'role': 'assistant', 'content': fourth_SeqReader_answer},
    {'role': 'user', 'content': fifth_SeqReader_question},
    {'role': 'assistant', 'content': fifth_SeqReader_answer},
    {'role': 'user', 'content': sixth_SeqReader_question},
    {'role': 'assistant', 'content': sixth_SeqReader_answer},
    ]

repair_lambda_ret_list = [
    {'role': 'system', 'content': sys_repair_lambda_prompt},
    {'role': 'user', 'content': first_shot_lambda_repair_question},
    {'role': 'assistant', 'content': first_shot_lambda_repair_answer},
    {'role': 'user', 'content': second_shot_lambda_repair_question},
    {'role': 'assistant', 'content': second_shot_lambda_repair_answer},
    ]