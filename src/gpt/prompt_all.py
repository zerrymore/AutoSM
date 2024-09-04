sys_ccg_prompt = """\
You are a Lambda Calculus Expression expert in cryptographic protocol field. Now you need to parse texts into lamba calculus. \
There are some Lambda Calculus Expression template you may use: 
1. Send(A:agent, B:agent, m:msg), represents agent A send message m to B. The Send only allows 3 arguments given;
2. Recv(B:agent, A:agent, n:msg), represents agnet B receive message n from A;
3. Gen(A:agent, n:nonce), represents A generates a fresh nonce n;
5. Knows(A, n1, n2,...), represents A has initial knowledge, n1, n2 should be infrastruture like keys;
6. Op(A:agent, func(msg)), represents A performs some operations like aenc/adec/concat... to form a message m;
func in Op should correspond to the predicate in natural language. Given expressions above can not be arguments of `Op` !!,
i.e., Op(R, send/recv..,) is not allowed. 
"""


sys_SeqReader_prompt = """\
You are a Lambda Calculus Expression expert in cryptographic protocol field. Now you need to parse texts into lamba calculus. \
There are some Lambda Calculus Expression template you may use: 
1. `Send(A:agent, B:agent, m:msg)`: Agent A sends message m to agent B. The `Send` function is limited to 3 arguments.
2. `Recv(B:agent, A:agent, n:msg)`: Agent B receives message n from agent A.
3. `Gen(A:agent, n:nonce)`: Agent A generates a fresh nonce n.
4. `Knows(A:agent, n1:nonce, n2:nonce, ...)`: Agent A initially knows items n1, n2, etc., which are infrastructure components like keys.
5. `Op(A:agent, func(msg))`: Agent A performs operations like aenc/senc/concat on messages to produce a new message m. \
The `func` in `Op` must match the corresponding predicate described in natural language. \
Note: Expressions listed above cannot serve as arguments to `Op`; for example, `Op(R, Gen/Send/, ...)` is prohibited. 
Do not parse any decryption operation, like Op(A, assign(msg, dec(...)))


Common knowledge: 
for Diffie-Hellman key exchange, we often use function `exp(g, x)` without considering mod operation (e.g., mod p).

our task is to complete each expression by directly filling in the placeholders denoted by /* >>The lambda calculus << */. \
Do not include any explanations, comments, or additional text outside of the required expressions.

!! For each `Send`, there should be a corresponding `Recv`.
!! A message should be first binded to a variable before sending it. e.g.,
  Op(A, assign(V, t))  // binds term t to a variable V
  Send(A, V)
"""



sys_sapic_prompt = """\
You are an expert in writing Sapic specification (a kind of process algebra).\
Translate given natural language description for protocol into Sapic Specification.
"""

ccg_parser_prompt_template = """\
Translate given natural language description for protocol into lambda calculus expressions. Think step by step.
Description: <The protocol text I give you>
"""

fine_repair_prompt = """\
# Spec with errors:
<The spec with errors I give you>
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
*/
"""

fifth_SeqReader_question = """\
We suppose that only agent Bob (whose identity is B) knows the secret key corresponding to pub(B). \
Next Bob receives the message {A, Na}pub(B) sent by Alice.
/* 
Knows(role(Bob), pkA, pkB)
Recv(Bob, Alice, message1)
*/
Using his private key, Bob decrypts the message. He sends the received nonce Na together with a freshly generated nonce Nb \
encrypted with A's public key (pub(A)) to Alice. Finally Alice receives the message {Na, Nb}pub(A).
/* >>The lambda calculus << */
"""

fifth_SeqReader_answer = """\
/* 
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


first_shot_ccg_question = """\
Translate given natural language description for protocol into lambda calculus expressions. Think step by step.
Description: Alice and Bob shares a symetric key Kab initially. \
Bob generates a fresh nonce m and computes H as the hash of m \
with its identity string bob, i.e., H=m || idB. Then Bob sends it to Alice.
"""

first_shot_ccg_answer = """\
Step1: Determine the roles and related function.
In this case:
Roles: { "Alice": "role(A)", "Bob": "role(B)" }
Functions: { "encrypts": "senc/2", "hash": "hash/1"}
Terms: { "a symetric key": "Kab", "identity": "idB", "hash value": "hash(concat(m, idB))" }
InitKnow: { "role(C)": "Kab", "role(S)": "Kab"}
Step2: Give lambda calculus to describe the protocol's behavior.
In this case:
### Lambda Calculus Expression
Knows(role(A), Kab)
Knows(role(B), Kab)
Gen(B, m)
Op(B, assign(H, hash(concat(m, idB))))
Send(B, A, senc(H, Kab))
Recv(A, B, senc(H, Kab))
Op(A, claim(H, 'sec'))
"""

second_shot_ccg_question = """\
Translate given natural language description for protocol into lambda calculus expressions. Think step by step.
Description: Alice starts the protocol by sending her identity A together with a freshly generated random number Na. \
This message is encrypted using an asymmetric encryption algorithm with B's public key (denoted pub(B)). \
We suppose that only agent Bob (whose identity is B) knows the secret key corresponding to pub(B). \
Next Bob receives the message {A, Na}pub(B) sent by Alice. \
Using his private key, Bob decrypts the message. He sends the received nonce Na together with a freshly generated nonce Nb \
encrypted with A's public key (pub(A)) to Alice. Finally Alice receives the message {Na, Nb}pub(A).\
She decrypts the message and checks that the nonce NA corresponds to the nonce previously generated and sent to Bob. \
She sends the nonce Nb to Bob encrypted with Bob's public key. Upon reception of this message Bob \
decrypts it and checks that the nonce corresponds to the one previously generated.\
"""

second_shot_ccg_answer = """\
Step1: Determine the roles, related functions and terms and initial knowledge.
In this case:
Roles: { "Alice": "role(A)", "Bob": "role(b)" }
Functions: { "encrypts": "aenc/2", "public key": "pk/1", "decryptes":"adec/2" }
Terms: { "public key of Alice": "pkA", "public key of Bob": "pkB", "identity of A":"idA" }
InitKnow: { "role(A)": "pkA, pkB",  "role(b)": "pkA, pkB"}
Step2: Give lambda calculus to describe the protocol's behavior with the dicts in Step1.
Before sending a message, fresh nonces should be identified and generated firstly.
In this case:
### Lambda Calculus Expression
Knows(role(A), pkB, pkA)
Knows(role(B), pkA, pkB)
Gen(A, Na)
Op(A, assign(message1, aenc(concat(idA, Na), pkB)))
Send(A, B, message1)
Recv(B, A, message1)
Op(B, adec(message1, skB))
Gen(B, Nb)
Op(A, assign(message2, aenc(concat(idA, Na), pkB)))
Send(B, A, message2)
Recv(A, B, message2)
Op(A, adec(message2, skA))
Send(A, B, aenc(Nb, pkB))
Recv(B, A, aenc(Nb, pkB))
"""

sapic_prompt_template = """\
Description: <The protocol text I give you>
"""


first_shot_sapic_question = """\
Description: Alice and Bob shares a symetric key Kab initially. \
Bob generates a fresh nonce m and computes H as the hash of m \
with its identity. Then Bob sends it to Alice.
"""

# first_shot_sapic_question = """\
# Alice and Bob shares a symetric key Kab initially. \
# Bob generates a fresh nonce m and computes H as the hash of m \
# with its identity string bob, i.e., H=m || idB. Then Bob sends it to Alice.
# """



# first_shot_sapic_answer = """\
# theory protocol
# begin
# functions: pk/1, aenc/2, adec/2, h/1
# equations: adec(aenc(m,pk(sk)),sk)=m
# let Client(skC, pkS) = 
#   new k;
#   let message = aenc(k, pkS) in
#   out(message);
#   in(cipher);
#   let h(=k) = cipher in 
#   event SessKeyC(pkS, k)     

# let Server(skS, pkC) =
#   in(request);
#   let message = adec(request, skS) in
#   event AnswerRequest(pk(skS), message);
#   out(h(message))

# process:
# !(
#   new sks; out(pk(sks)); event Client_init(pk(sks));
#   !(
#     new skc; out(pk(skc)); event Server_init(pk(skc));
#     (!Client(skc, pk(sks))  | !Server(sks, pk(skc)))
#   )
# )
# end
# """

first_shot_sapic_answer = """\
theory protocol
begin
functions: id/1, h/1
equations: sdec(senc(m, k), k) = m
let Alice(Kab, idA, idB) = 
  new m;
  let H = <h(m), idB> in
  ley cypher = senc(H, Kab) in
  out(H);    

let Bob(Kab, idA) =
  in(cypher);
  let <mess, =idA> = sdec(cypher, Kab) in
  0

process:
!(
    new Kab; 
    let idA = id(Kab); out(idA);
    (!Alice(Kab, idA)  | !Bob(Kab, idA))
  )
end
"""

second_shot_sapic_question = """\
Description: Alice starts the protocol by sending her identity A together with a freshly generated random number Na. \
This message is encrypted using an asymmetric encryption algorithm with B's public key (denoted pub(B)). \
We suppose that only agent Bob (whose identity is B) knows the secret key corresponding to pub(B). \
Next Bob receives the message {A, Na}pub(B) sent by Alice. \
Using his private key, Bob decrypts the message. He sends the received nonce Na together with a freshly generated nonce Nb \
encrypted with A's public key (pub(A)) to Alice. Finally Alice receives the message {Na, Nb}pub(A).\
She decrypts the message and checks that the nonce NA corresponds to the nonce previously generated and sent to Bob. \
She sends the nonce Nb to Bob encrypted with Bob's public key. Upon reception of this message Bob \
decrypts it and checks that the nonce corresponds to the one previously generated.\
"""



second_shot_sapic_answer = """\
theory Needham_Schroeder_Public_Key_protocol
begin

functions: pk/1, aenc/2, adec/2
equations: adec(aenc(m,pk(sk)),sk)=m

let P(skA, pkB)=      
  new Na;
  event OUT_I_1(aenc(<Na, pk(skA)>,pkB));
  out(aenc( <Na,pk(skA)> ,pkB));  y
  in(cypher);
  let mess = adec(cypher,skA) in
  let <=Na,xNb> = mess in
  event IN_I_2_nr(xNb,aenc(<Na,xNb>,pk(skA)));
  event OUT_I_2(aenc(xNb,pkB));
  out(aenc(xNb, pkB));
  event SessionBuiltA(pk(skA), Na, xNb);
  event SessionA(pk(skA), pkB, Na) 
  
let Q(skB, pkA) =  
  in(cypher1);
  let mess1 = adec(cypher1,skB) in
  let <xNa, =pkA> = mess1 in
  event IN_R_1_ni(xNa,aenc(<xNa,pkA>,pk(skB)));
  new Nb;
  event OUT_R_1(aenc(<xNa,Nb>,pkA));
  out(aenc(<xNa,Nb>,pkA));
  in(cypher2);
  let mess2 = adec(cypher2,skB) in
  let =Nb = mess2 in
  event IN_R_2(Nb, aenc(Nb, pk(skB)));
  event SessionBuiltB(pk(skB),xNa,Nb);
  event SessionB(pkA,pk(skB),xNa)

process:
!(
  new skA; out(pk(skA)); event HonestA(pk(skA));
  !(
    new skB; out(pk(skB)); event HonestB(pk(skB));
    (!P(skA, pk(skB)) | !Q(skB, pk(skA)))
  )
)   
end
"""
third_shot_sapic_question = """\
Here, Alice A initiates the communication to Bob B. 
S is a server trusted by both parties. In the communication:
1. A and B are identities of Alice and Bob repectively.
2. Kas is a symmetric key known only to  A and S
3. Kbs is a symmetric key known only to  B and S
4. Na and Nb are nonces generated by A and B repectively.
5. Kab is a symmetric, generated key, which will be the session key of the 
session between A and B.
6. The encryption and decryption in this protocol is performed by senc/sdec. 

Both A, B and S knows the identities of A and B.

The protocol can be specified as follows:
1. Alice sends a message <A, B, Na> (where A, B indicates the identities and Na a fresh nonce) \
and a fresh nonce Na to the server, telling the server she wants to \
communicates wit Bob.

2. The server generates Kab and sends back to Alice a copy encrypted under \
Kbs for Alice to forward to Bob and also a copy for Alice. \
Since Alice may be requesting keys for several different people, \
the nonce assures Alice that the message is fresh and that \
the server is replying to that particular message and the inclusion of Bob's name \
tells Alice who she is to share this key with. The message sent out in this phase \
is senc(<Na, B, Kab, senc(<Kab, A>, Kbs)>, Kas).

3. Alice forwards the message senc(<Kab, A>, Kbs) to Bob who can decrypt it \
with the key he shares with the server Kbs, thus authenticating the data. 

4. Bob sends Alice a nonce Nb encrypted under Kab to show that he has the key.

5. Alice performs a simple operation 'dec' on the nonce, re-encrypts it \
and sends it back verifying that she is still alive and that she holds the key.
"""
third_shot_sapic_answer = """\
theory nssk
begin

functions: id/1, dec/1, inc/1
equations: inc(dec(x)) = x
functions: senc/2, sdec/2[destructor]
equations: sdec(senc(m,k),k)=m

let A(Kas, idA, idB) =
  new Na;
  out(<idA, idB, Na>);
  in(cypher);
  let <=Na, =idB, Kab, message2> = sdec(cypher, Kas) in
  out(message2);
  in(cypher2);
  let Nb = sdec(cypher2, Kab) in
  event Running_A(idA, idB, <'A', 'B', dec(Nb), Kab>);
  event Commit_A(idA, idB, <'A', 'B', Nb, Kab>);
  //out(senc(Nb_dec, Kab))
  // modified:
  out(senc(dec(Nb),Kab))

let B(Kbs, idA) =
  in(cypher3);
  let <Kab, =idA> = sdec(cypher3, Kbs) in
  new Nb;
  event Running_B(idA, id(Kbs), <'A', 'B', Nb, Kab>);
  event Secret(idA, id(Kbs), Kab);
  out(senc(Nb, Kab));
  event B_OUT_4(senc(<'4', Nb>, Kab));
  in(cypher4);
  let Nb_dec = sdec(cypher4, Kab) in 
  let dec(=Nb) = Nb_dec in
  event Commit_B(idA, id(Kbs), <'A', 'B', dec(Nb), Kab>)

let S(Kas, Kbs, idA, idB) =
  new Kab;
  in(<=idA, =idB, Na>);
  let message1 = <Na, idB, Kab, senc(<Kab, idA>, Kbs)> in
  let cypher = senc(message1, Kas) in
  out(cypher); 0

process:
!(   
  new Kas; new Kbs; out(id(Kas)); out(id(Kbs));  
    (!A(Kas, id(Kas), id(Kbs))  | !B(Kbs, id(Kas)) | !S(Kas, Kbs, id(Kas), id(Kbs)))  
  )

end
"""
# sys_init_prompt = """\
# You are an expert in writing Sapic specification (a kind of process algebra).\
# Give you serveral role signatures of local process whose argument is the \
# initial knowledge of the role and some modeling trick hints, \
# generate the top specification for different roles. Think step by step.
# The top specification still needs to follow the basic syntax of Sapic.
# It is not allowed use `new n1, n2...;` for multiple fresh nonce declaration. 
# """

sys_init_prompt = """\
Task: 
Given signatures of role process and some hints, write the top specification for them.

1. Given a signature `let ROLE(t1, t2...) = `, identify the meaning of every variable.
Any variable should be either a fresh nonce or a variable binding to other ground terms.

2. Modeling Guidelines:
   - Incorporate modeling tricks and strategies suggested in the hints provided.
   - Ensure all specifications adhere to Sapic's syntax rules.
   - Syntax: 
     ⟨P, Q⟩ ::=
        0 
        | event F; P 
        | new n; P 
        | !P  
        | P | Q 
        | out(t); P 
        | in(t); P 
        | let p = t in P else Q 

Please proceed by structuring each role's specification clearly and logically, keeping in mind the constraints and requirements specified.
"""

init_prompt_template = """\
Hints: <The hints text I give you>
Role signatures:
```
<The signature I give you>
```
"""

first_shot_init_question = """\
Hints: There can be unbounded number of servers and clients in the protocol, \
the identitis of both can represented with the `id(Kas)` where `Kas` is the \
symmetric key.
Role sigunatures: 
```
let C(Kcs, idC) = 
let S(Kcs, idC) = 
```
"""


first_shot_init_answer = """\
process:
!(    
  new Kcs; 
  let idC = id(Kcs) in out(id(Kcs));  
    (!Client(Kcs, id(Kcs))  | !Server(Kcs, id(Kcs))  )  
)
"""

second_shot_init_question = """\
Hints: There can be unbounded number of servers and clients in the protocol, \
the identity of each role can be represented by the its public key.
Role sigunatures: 
```
let A(skA, pkB, idA, idB) =      
let B(skB, pkA, idA, idB) =
```
"""


second_shot_init_answer = """\
process:
!(   
  new skA; out(pk(skA)); event HonestA(pk(skA));
  !(   
    new skB; out(pk(skB)); event HonestB(pk(skB));
    let idA = pk(skA) in  out(idA);   // Both the identities are known publicly.
    let idB = pk(skB) in  out(idB); 
    (   !A(skA, pk(skB), idA, idB) | !B(skB, pk(skA)), idA, idB)
  )
)  
"""

# sys_fine_repair_prompt = """\
# As an expert in writing process specifications for protocol, \
# review above specifications cafelly and correct it. \
# The specification is given with error reports,  with given documents \
# please carefull revise it. 
# Just show me the revised specification and don't explain it.

# There are 2 main error types:
# 1. Variables that are unbound. 
# The variables can only be used after receiving \
# from outside, or it is a generated fresh nonce by the role, or it is an element \
# in the initial knowledge set (the argument of the role's signature.) When you 
# encounter such errors, consider the solutions: S1. If you think it is a fresh nonce,
# add a `new n` statement before using it, `n` is the unbounded variable; \
# S2. If you think it should come from a received message, review the the term within \
# near `in(msg)` statement cafefully, check whether the form of `msg` is correct, \
# the unbounded variable may be included in it;
# S3. If you think the variable should be known by the role initially, add it into \
# the initial knowledge set of the role, i.e., the argument of the signature of the \
# role specification like `let A(t1, t2, t3)` where `t3` is added.
  
# 2. Pattern matching error. 
# In the specification, pattern matching operator `=` is used to match the variable \
# to local one. There are two kinds errors for pattern matching: (1) False positives matching, \
# i.e., a non-local variable is matched and (2) False negative mathcing, i.e., a local \
# variable should be matched use `=` but haven't. For (1), you should consider if the variable \
# should be a local variable but not, if so handle the variable as unbounded problems as above. \
# In other case, the message should be a new incoming variable, if so just remove the `=` operator. 
# For (2), If you think the local variable should be matched without doubt, add `=` operator.

# Adjust the specifications based on these instructions without adding extra explanations.
# """

# sys_repair_prompt = """\
# Review and correct the following process specifications for protocol implementation, focusing on three primary aspects:

# 1. **Local Perspective on Message Reception:** 
# - An incoming message should be in correct form for the specific role.
# - If the incoming message is tuple, then all the element can be obtained.
#     * Example: in(<msg1, msg2>), the variable msg1, msg2 can be obtained by the role.
# - If the obtained message is a cypher which can be decrypted by the role, then use `let` constructs to decrypt it. 
#     * Example: in(cypher), if the the cypher = aenc(m, pkA), and the 
# - If the obtained cypher can not be decypted 
# - Understand that roles lacking decryption capabilities perceive certain message parts as opaque, denoted by "msg". 
# - Example correction:
#     - Original: `in(aenc(aenc(m, pkB), pkA))`
#     - Corrected: `in(cipher); let message = adec(cipher, skA) in`

# 2. **Knowledge Acquisition Through Message Reception:** 
# - A role receiving a message acquires all variables contained within it, enriching its knowledge base.
# - Example scenario: Upon receiving `<m, na, aenc(nb, k)>`, role A gains knowledge of "m", "na", "nb", "k".

# 3. **Variable Binding with `let` Constructs** 
# - The `let` construct is crucial for binding variables within messages to local variables for further manipulation.
# - A variable can only be binded once, can not be updated.
    
# Output the the specifications after adjusting, do not include any other information.
# """



sys_repair_prompt = """\
Task: Review and revise the following protocol implementation specifications, focusing on improving three key aspects:

1. Local Perspective on Message Reception:
- For the messages in `in(message)` statement, you should parse them thorougly and legitimately.
  * For example, if a statement is `in(cypher)`, you should figure out the source of it. Suppose you find that, \
    `cypher` equals to `aenc(m, pkA)` and you possesses `skA`, which allowing you to decrypt it, you should replace it with `in(cypher); let m = adec(cypher, skA)`:
    
- Ensure incoming messages are in the correct format for the specific role.
  * For tuple messages (e.g., in(<msg1, msg2>)), all elements are accessible to the role.
  * Decrypt messages where possible using let constructs. For example, for in(cypher) where cypher = aenc(m, pkA), use: \
    `in(cypher); let message = adec(cypher, skA)` in
  * If decryption is not possible, treat parts of the message as opaque (denoted by new variable).
  * Correct errors such as nested encryptions not aligned with decryption capabilities:
    Incorrect: in(aenc(aenc(m, pkB), pkA))
    Corrected: in(cipher); let message = adec(cipher, skA) in

2.Knowledge Acquisition Through Message Reception:
- A role gains knowledge of all variables within received messages. For instance, if role A receives <m, na, aenc(nb, k)>, it acquires m, na, nb, and k.
Variable Binding with let Constructs:

3.Use let constructs to bind message variables to local variables for manipulation. 
- Ensure that each variable is bound once and not updated.
"""

sys_repair_lambda_prompt = """\
As an expert in security protocol analysis, please review and refine the following lambda expressions:

1. Message Reception Recv(A, B, msg):
  - Identify Knowledge Set: Establish the knowledge set of A.
  - Message Parsing:
      If msg is decipherable using A's knowledge set, apply Op(A, assign(var, msg)) to unpack the message.
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


# sys_repair_prompt = """\
# As an expert in writing process specifications for protocol, \
# you should review above specifications cafelly and correct it. The main aspects you should consider:
# 1. Local Perspective on Message Reception: When a role lacks the knowledge to decrypt a part of a message, \
# that part is considered opaque, represented by a variable like msg. \
# This opacity reflects the role's inability to decrypt and directly understand the contents. \
# For example, if role A only possesses the public key of C (pkC), when receives a message "aenc(m, skB)", \
# the local perspective for the message is "msg". The variable "msg" represents that the receiving message is totally opaque, \
# as A can not decrypt. More complex, if A possesses the secrect key skA, when receives message "aenc(aenc(m, pkB), pkA)", \
# the local perspective for it is "aenc(cipher, pkA)", i.e., 
# ```
# - in(aenc(aenc(m, pkB), pkA))
# + in(cipher);
# + let message = adec(cipher, skA) in
# ```

# 2. Knowledge Acquisition Through Message Reception: Upon receiving a message, a role acquires all variables within it. \
# This acquisition enriches the role's knowledge set, potentially enabling decryption and further processing in subsequent steps.\
# For example, if role A receives a message "<m, na, aenc(nb, k)>", i.e., "in(<m, na, aenc(nb, k)>);..." \
# all variables "m", "na", "nb", "k" can be used in let constructs and message construction for sending.

# 3. Variable Binding with let Constructs: The let construct is used to match and bind variables within messages to local variables, \
# facilitating the manipulation and examination of message contents. For example, "let m = n in" states that terms m matches n, \
# e.g., "let msg = aenc(m, k)" representing term "aenc(m, k)" is binded to variable "msg". More complex, if role A receives a message \
# "aenc(<Na, Nb>, pkA)", i.e., "in(aenc(<Na, Nb>, pkA))" statement in process, as the role possessses the corresponding secrect key for pkA, \
# i.e., skA, the statements should be replaced with:
# ```
# - in(aenc(<Na, Nb>, pkA))
# + in(cypher);
# + let mess = adec(cypher,skA) in
# + let <=Na,Nb> = mess in
# ```
# Opertor `=Na` means the pattern matching on local variable Na, Note that the operator can \
# only operate on variables! `=aenc(...)` is not allowed.
# Just show me the revised specification and don't explain and include any other information.
# """


repair_prompt_template = """\
<The spec I give you>
"""

# first_shot_repair_question = """\
# let P(skA, pkB)=      
#   new Na;
#   out(aenc(<Na,pk(skA)> ,pkB));  
#   in(aenc(<Na,Nb>,pkA));
#   out(aenc(Nb, pkB))
# """

# first_shot_repair_answer = """\
# let P(skA, pkB)=      
#   new Na;
#   out(aenc(<Na,pk(skA)>, pkB)); 
# - in(aenc(<Na,Nb>,pkA));
# + in(cypher);
# + let mess = adec(cypher,skA) in
# + let <=Na,Nb> = mess in
#   out(aenc(Nb, pkB))
# """


# second_shot_repair_question = """\
# let Q(skB, pkA) =  
#   in(aenc(<Na,pk(skA)> ,pkB));  
#   new Nb;
#   out(aenc(<Na,Nb>,pkA));
#   in(aenc(Nb, pkB))
# """

# second_shot_repair_answer = """\
# let Q(skB, pkA) =  
# - in(aenc(<Na,pk(skA)> ,pkB));
# + in(cypher1);
# + let mess1 = adec(cypher1,skB) in
# + let <Na, =pkA> = mess1 in
#   new Nb;
#   out(aenc(<Na,Nb>,pkA));
# - in(aenc(Nb, pkB))
# + in(cypher2);
# + let mess2 = adec(cypher2,skB) in
# + let =Nb = mess2 in 0
# """

# third_shot_repair_question = """\
# let Alice(Kab)=      
#   new Na;
#   out(senc(Na, Kab));  
#   in(<Nb, senc(<Na, Nb>, Kab)>)
# """

# third_shot_repair_answer = """\
# let Alice(Kab)=      
#   new Na;
#   out(senc(Na, Kab));  
# - in(<Nb, senc(<Na, Nb>, Kab)>)
# + in(cypher);
# + let <Nb, message> = cypher in
# + let <=Na, =Nb> = message in 0
# """



second_shot_fine_repair_answer = """\
let Alice(Kab)=      
  new Na;
  out(senc(Na, Kab));  
  in(cypher);
  let <Nb, message> = cypher in
  let <=Na, =Nb> = message in 0
"""


first_shot_repair_question = """\
let Alice(skA, pkB)=      
  new Na;
  let cypher1 = aenc(<Na,pk(skA)>, pkB) in 
  out(cypher1);  
  in(aenc(<Na,Nb>,pkA));
  out(aenc(Nb, pkB))
  
let Bob(skB, pkA) =  
  in(cypher1);  
  new Nb;
  out(aenc(<Na,Nb>,pkA));
  in(aenc(Nb, pkB))
"""

# first_shot_repair_answer = """\
# let Alice(skA, pkB)=      
#   new Na;
#   let cypher1 = aenc(<Na,pk(skA)>, pkB) in
#   out(cypher1); 
# - in(aenc(<Na,Nb>,pkA));   // Alice possesses `skA`, so the message should be decrypted with skA
# + in(cypher);
# + let mess = adec(cypher,skA) in  // mess is tuple, which can be parsed further
# + let <Na,Nb> = mess in
#   out(aenc(Nb, pkB))
  
# let Bob(skB, pkA) =  
#   in(cypher1);
# + let mess1 = adec(cypher1,skB) in
# + let <Na, pkA> = mess1 in
#   new Nb;
#   out(aenc(<Na,Nb>,pkA));
# - in(aenc(Nb, pkB))
# + in(cypher2);
# + let mess2 = adec(cypher2,skB) in
# + let Nb = mess2 in 0
# """

first_shot_repair_answer = """\
let Alice(skA, pkB)=      // initial knowledge: M = {skA, pkB}
  new Na;
  let cypher1 = aenc(<Na,pk(skA)>, pkB) in
  out(cypher1); 
- in(aenc(<Na,Nb>,pkA));   // Alice possesses `skA`, so the message should be decrypted with skA
+ in(cypher);
+ let mess = adec(cypher,skA) in  // cypher is encrypted with pkA, which can be decrypted by Alice with skA in 
+ let <Na,Nb> = mess in     // message is a 2-tuple, which can parsed further
  out(aenc(Nb, pkB))
  
let Bob(skB, pkA) =  // initial knowledge: M = {skB, pkA}
  in(cypher1);
+ let mess1 = adec(cypher1,skB) in
+ let <Na, pkA> = mess1 in    // mess1 is a 2-tuple, which can parsed further
  new Nb;
  out(aenc(<Na,Nb>,pkA));
- in(aenc(Nb, pkB))
+ in(cypher2);
+ let mess2 = adec(cypher2,skB) in   // cypher2 is encrypted with pkB, which can be decrypted by Bob with skB in M
+ let Nb = mess2 in 
+ 0
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
+ Op(B, assign(mess, adec(cypher1, skB)))   // cypher1 is encrypted with pkB, which can be decrypted by B with skB in MB
+ Op(B, assign(concat(Na, pkA), mess))      // mess1 is a 2-tuple, which can parsed further
  Gen(B, Nb)
  Op(A, assign(cypher2, aenc(concat(Na, Nb), pkA)))
  Send(B, A, cypher2)                       // cypher2 = aenc(concat(Na, Nb), pkA)
  Recv(A, B, cypher2)                      
+ Op(A, assign(mess2, adec(cypher2, skA))) // cypher2 is encrypted with pkA, which can be decrypted by A with skA in MA
+ Op(A, assign(concat(Na, Nb), mess2))     // mess1 is a 2-tuple, which can parsed further
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
+ Op(S, assign(Na, adec(cypher1, Kas)))   // cypher1 is encrypted with Kas, which can be decrypted by S with Kas in MS
  Gen(S, Kab)
  Gen(S, Nb)
  Op(S, assign(cypher, <Nb, senc(<Na, Nb>, Kab)>))
  Send(S, A, cypher)                     // cypher = <Nb, senc(<Na, Nb>, Kab)> 
  Recv(A, S, cypher)                     // cypher = <Nb, senc(<Na, Nb>, Kab)> 
+ Op(A, assign(<Nb, mess>, cypher))      // cypher is a 2-tuple, which can parsed further.
+ // mess = senc(<Na, Nb>, Kab), which is encrypted with Kab, but Kab is not in MA, mess is opaque to A.
- Op(A, assign(mess2, senc(<Na, Nb>, Kab)>)))  // message senc(<Na, Nb>, Kab)>) can not be constructed by A, 
- Send(A, S, mess2)
+ Send(A, S, mess)       // A has obtained a new variable mess from cypher, which equals to mess2.
"""

third_shot_repair_question = """\
let Alice(Kas)=     
  new Na;
  out(senc(Na, Kas));  
  in(<Nb, senc(<Na, Nb>, Kab)>)
"""

third_shot_repair_answer = """\
let Alice(Kas)=      // initial knowledge: M = { Kas }
  new Na;
  out(senc(Na, Kas));  
- in(<Nb, senc(<Na, Nb>, Kab)>) 
+ in(cypher);      // cypher is a 2-tuple <Nb, mess>
+ let <Nb, mess> = cypher in  // cypher is a 2-tuple <Nb, mess>
+ // mess = senc(<Na, Nb>, Kab), which is encrypted with Kab, but Kab not in M, so can not be decrypted further 
+ 0   
"""

# third_shot_repair_question = """\
# let Alice(Kas, idA, idB)=     
#   new Na;
#   out(<Na, idA>);  
#   in(senc(<Na, Nb>, Kas)) 
# """

# third_shot_repair_answer = """\
# let Alice(Kas)=      //initial knowledge: M = { Kab }
#   new Na;
#   out(<Na, idA>); 
# - in(senc(<Na, Nb>, Kas)) 
# + in(cypher);    
# + let <Na, Nb> = sdec(cypher, Kas) in  // cypher is encrypted with symmetric key Kas in knowledge set, so Alice can decrypt it.
# + 0   
# """

# sys_variable_prompt = """\
# Task Description:
# You are an expert in security protocol analysis tasked with determining the initial knowledge \
# requirements for roles within a given protocol. You will receive a natural language description \
# of the protocol, an incomplete applied-pi calculus style specification, and a list of variables \
# used in the role specification.

# Your job is to:
# - Identify the meaning of each variable in the provided set.
# - Determine which variables should be initially known by the specific role.

# Common Knowledge:
# - Role identities and public keys are typically publicly known.
# - Symmetric keys are known only to the roles that share them.
# - Secret key can not be known by other roles.
# """

sys_variable_prompt = """\
Task Description:
You are an expert in security protocol analysis tasked with determining the initial knowledge \
requirements for roles within a given protocol. You will receive a natural language description \
of the protocol, an incomplete applied-pi calculus style specification.

Your job is to:
- Identify the meaning of each variable in the provided set.
- Determine which variables should be initially known by the specific role.
Though some initial knowledge may not be explicit, you should figure them out.

Common Knowledge:
- Role identities and public keys are typically publicly known.
- Symmetric keys are known only to the roles that share them.
- Secret key can not be known by other roles.
"""


first_determine_var_question = """\
Description: Alice starts the protocol by sending her identity A together with a freshly generated random number Na. \
This message is encrypted using an asymmetric encryption algorithm with B's public key (denoted pub(B)). \
We suppose that only agent Bob (whose identity is B) knows the secret key corresponding to pub(B). \
Next Bob receives the message {A, Na}pub(B) sent by Alice. \
Using his private key, Bob decrypts the message. He sends the received nonce Na together with a freshly generated nonce Nb \
encrypted with A's public key (pub(A)) to Alice. Finally Alice receives the message {Na, Nb}pub(A).\
She decrypts the message and checks that the nonce NA corresponds to the nonce previously generated and sent to Bob. \
She sends the nonce Nb to Bob encrypted with Bob's public key. Upon reception of this message Bob \
decrypts it and checks that the nonce corresponds to the one previously generated.\
  
Incomplete spec:
let Alice(skA)=      
  new Na;
  out(aenc(<Na, A, B> ,pkB)); 
  in(cypher);
  let mess = adec(cypher,skA) in
  let <Na, Nb> = mess in
  out(aenc(Nb, pkB));
  
let Bob(skB) =  
  in(cypher1);
  let mess1 = adec(cypher1,skB) in
  let <Na, A, B> = mess1 in
  new Nb;
  out(aenc(<Na,Nb>,pkA));
  in(cypher2);
  let mess2 = adec(cypher2,skB) in
  let Nb = mess2 in 0
"""



# first_determine_var_answer = """\
# In role Alice, `Na` is fresh nonce, `mess`, `cypher` is intermediate variable, `pkB` is the public key, \
# `Nb` is a variable extracted from the received message, `A` is the identity of himself, \
# `B` is the identity of Bob. Alice should know the public key of bob `pkB` initially, also both identities `A` and `B`. \
# So the initial knowledge should extend from {"skA"} to {"skA", "pkB", "A", "B"}.
# The similar is for Bob. The initial knowledege of Bob should extend from {"skB"} to {"skB", "pkA", "A", "B"} 
# ```
   
# Conclude your analysis in a dictionary format; 
# the key in the dictionary should be consistent to the role name in given incomplete spec.
# In this case:
# {  "Alice":{"skA", "pkB", "A"}, "Bob": {"skB", "A", "pkA"} }
# """

first_determine_var_answer = """\
Initial Knowledge Analysis:
For Alice:
- Alice must initially know her own secret key (skA) to decrypt messages.
- Alice must know Bob's public key (pkB) to encrypt her nonce and send it to Bob.
- She should also be aware of her own and Bob's identity ('A' and 'B').
- Variables 'mess', 'cypher' are intermediate variables, which is not the intial knowledge.

For Bob:
- Bob must initially know his own secret key (skB) for decrypting messages received from Alice.
- He needs Alice's public key (pkA) to encrypt responses.
- Bob should also know both identities ('A' and 'B') to properly handle encrypted communications.

Conclusion:
Based on the provided specification, the initial knowledge for each role should be as follows:
{
  "Alice": {"skA", "pkB", "A", "B"},
  "Bob": {"skB", "pkA", "A", "B"}
}
"""