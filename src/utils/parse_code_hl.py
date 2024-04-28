from pygments import highlight
from pygments.lexers import PythonLexer
from pygments.formatters import HtmlFormatter
import pygments.lexer
from pathlib import Path
from pygments.lexer import RegexLexer, bygroups
from pygments.formatters import TerminalFormatter
from pygments.token import Name, Comment, Text, Keyword, Punctuation


def loadText(code:str) -> str:
    class CustomLexer(RegexLexer):
        tokens = {
            'root': [
                # (r'\b(let)\b', Keyword),
                (r'IKE_.*:$', Name.Namespace),
                (r'//.*$', Comment.Single),
                (r'.', Text),  
                (r'/\*', Comment.Multiline, 'comment'),  # 添加这一行来匹配多行注释的开始，并转移到 'comment' 状态
            ],
            'comment': [
                (r'[^*/]+', Comment.Multiline),
                (r'/\*', Comment.Multiline, '#push'),
                (r'\*/', Comment.Multiline, '#pop'),
                (r'[*/]', Comment.Multiline)
            ]
        }
    lexer = CustomLexer()

    hl = pygments.highlight(code.replace('\r\n', '\n').replace('   ', ' '),
                lexer,
                formatter=pygments.formatters.html.HtmlFormatter(
                    style='xcode',
                    linenos='inline',
                    linespans=True,
                    lineanchors='line',
                    nobackground=True
                ),
        )
    # print(hl)
    """Here we replace \n with space in HTML, to make different
    lines diplay sperated by space."""
    return hl
    return hl.replace("\n", "")

def genCheckIR(lamSeq:str) -> str:
    """ the argument lamSeq is lambda calculus sequence """
    class CustomLexer(RegexLexer):
        tokens = {
            'root': [
                (r'\b(Role|Fresh)\b', Name.Class),
                (r'\b(Send|Recv|Gen|Equals|fresh)\b', Name.Constant),
                (r'\b(aenc|adec|pk|sk|role|concat)\b', Name.Function),
                (r'/\*', Comment.Multiline, 'comment'),
                (r'#.*$', Comment.Single),
                (r'.', Text),  
            ],
            'comment': [
                (r'[^*/]+', Comment.Multiline),
                (r'/\*', Comment.Multiline, '#push'),
                (r'\*/', Comment.Multiline, '#pop'),
                (r'[*/]', Comment.Multiline)
            ]
        }
    lexer = CustomLexer()
    
    hl = pygments.highlight(lamSeq.replace('\r\n', '\n').replace('   ', ' '),
                lexer,
                formatter=pygments.formatters.html.HtmlFormatter(
                    style='xcode',
                    linenos='inline',
                    linespans=True,
                    lineanchors='line',
                    nobackground=True
                ),
        )
    # print(hl)
    return hl

def sapic_hl_gen(lamSeq:str) -> str:
    """ the argument lamSeq is lambda calculus sequence """
    class CustomLexer(RegexLexer):
        tokens = {
            # 'root': [
            #     (r'\b(let|begin|functions|equations|end)\b', Keyword),
            #     (r'\b(in|out|event|new)\b', Name.Function),
            #     (r'//.*$', Comment.Single),
            #     (r'.', Text),  # 匹配其他文本，将其标记为 Text 类型
            # ],
            'root': [
                (r'\b(let)\b', Keyword),
                (r'\b(in)\b(?=\()', Name.Function),  # in 后面紧跟 ( 的情况
                (r'\b(in)\b(?!\()', Keyword),  # 单独的 in
                (r'\b(out|event|new)\b', Name.Function),
                (r'\b(process)\b', Name.Class),
                (r'\b(end|begin|functions|equations|theory|builtins)\b', Keyword),
                (r'//.*$', Comment.Single),
                (r'/\*', Comment.Multiline, 'comment'),  # 添加这一行来匹配多行注释的开始，并转移到 'comment' 状态
                (r'.', Text),
            ],
            'comment': [
                (r'[^*/]+', Comment.Multiline),
                (r'/\*', Comment.Multiline, '#push'),
                (r'\*/', Comment.Multiline, '#pop'),
                (r'[*/]', Comment.Multiline)
            ]
        }
    lexer = CustomLexer()

    hl = pygments.highlight(lamSeq.replace('\r\n', '\n').replace('   ', ' '),
                lexer,
                formatter=pygments.formatters.html.HtmlFormatter(
                    style='xcode',
                    linenos='inline',
                    linespans=True,
                    lineanchors='line',
                    nobackground=True
                ),
        )
    # print(hl)
    return hl


if __name__ == "__main__":
    text = '''In this protocol, a client C generates a fresh symmetric key k, encrypts it with the public key pkS of a server S (aenc stands for asymmetric encryption),
and sends it to S. The server confirms the key's receipt by sending the hash of the key back to the client.'''
    text = '''Alice starts the protocol by sending her identity A together with a freshly generated random number Na. 
This message is encrypted using an asymmetric encryption algorithm with B's public key (denoted pub(B)).
We suppose that only agent Bob (whose identity is B) knows the secret key corresponding to pub(B).
Next Bob receives the message {A, Na}pub(B) sent by Alice. Using his private key, Bob decrypts the message. 
He sends the received nonce Na together with a freshly generated nonce Nb encrypted with A's public key (pub(A)) to Alice.
Finally Alice receives the message {Na, Nb}pub(A). 
She decrypts the message and checks that the nonce NA corresponds to the nonce previously generated and sent to Bob. 
She sends the nonce Nb to Bob encrypted with Bob's public key.
Upon reception of this message Bob decrypts it and checks that the nonce corresponds to the one previously generated.'''
    text = '''Alex computes a nonce and sends it to Blake. (A -> B: ANonce)
When Blake receives Alex's nonce, Blake computes their own nonce and sends it to Alex. (B -> A: BNonce)
When Alex receives Blake's nonce, Alex does two things:
Alex installs a session key SK, which is derived from ANonce and BNonce by applying a key derivation function (i.e., SK = kdf(ANonce, BNonce)).
Once the session key is installed, Alex sends a message with the string "ACK" to Blake (A -> B: "ACK") and switches to a 'DONE' state to indicate that the protocol has been executed successfully on Alex's side.
When Blake receives the "ACK" message, Blake also computes the session key SK = kdf(ANonce, BNonce), installs it and switches to a 'DONE' state.'''
    text = '''In this protocol, each party x has a long-term private key lkx and a corresponding public key pkx = 'g'^lkx, 
where 'g' is a generator of the Diffie-Hellman group. Because 'g' can be public, we model it as a public constant. 
Two different hash functions h1 and h2 are used.

To start a session, the initiator I first creates a fresh nonce eskI, also known as I's ephemeral (private) key.
He then concatenates eskI with I's long-term private key lkI, hashes the result using the hash function h1, and sends 'g'^h1(eskI ,lkI) to the responder.
The responder R stores the received value in a variable X, computes a similar value based on his own nonce eskR and long-term private key lkR, and sends the result to the initiator, who stores the received value in the variable Y.
Finally, both parties compute a session key (kI and kR, respectively) whose computation includes their own long-term private keys, such that only the intended partner can compute the same key.'''

    text = '''A sends a message containing A's identity, B's identity, and a nonce value Na to S.
S sends a message to A containing several components encrypted with Kas. These components include Na, B's identity, a shared secret key Kab, and an encrypted message containing Kab and A's identity encrypted with Kbs.
A sends a message to B containing Kab and A's identity, encrypted with Kbs.
B sends a message to A containing a nonce value Nb, encrypted with Kab.
A sends a message to B containing the decryption of Nb using Kab, encrypted with Kab.'''
    text = '''A initiates communication with B by sending a message containing M (a nonce), A's identity, B's identity, and an encrypted portion {Na, M, A, B}Kas using A's secret key Kas.
B receives the message from A and responds by sending back a message that includes M, A's identity, B's identity, the previously received encrypted portion {Na, M, A, B}Kas, and another encrypted portion {Nb, M, A, B}Kbs using B's secret key Kbs.
The server S receives the message from B and forwards it to B. S decrypts the first encrypted portion {Na, M, A, B}Kas and {Nb, M, A, B}Kbs, obtaining Na and Nb. 
Then S re-encrypts Na with the shared secret key Kab to create {Na, Kab}Kas and re-encrypts Nb with the same shared key Kab to create {Nb, Kab}Kbs. 
S sends this modified message to B.
B receives the modified message from S, decrypts the encrypted portions {Na, Kab}Kas and {Nb, Kab}Kbs, obtaining Na and Kab.
B then re-encrypts Na with the shared secret key Kab to create {Na, Kab}Kas and sends this to A.'''

    text = '''The following steps are used to exchange a key.  In this, C is the
client; S is the server; p is a large safe prime; g is a generator
for a subgroup of GF(p); q is the order of the subgroup; V_S is S's
identification string; V_C is C's identification string; K_S is S's
public host key; I_C is C's SSH_MSG_KEXINIT message and I_S is S's
SSH_MSG_KEXINIT message that have been exchanged before this part
begins.

C generates a random number x (1 < x < q) and computes
e = g^x mod p.  C sends e to S.

S generates a random number y (0 < y < q) and computes
f = g^y mod p.  S receives e.  It computes K = e^y mod p,
H = hash(V_C || V_S || I_C || I_S || K_S || e || f || K)
(these elements are encoded according to their types; see below),
and signature s on H with its private host key.  S sends
(K_S || f || s) to C.  The signing operation may involve a
second hashing operation.

C verifies that K_S really is the host key for S (e.g., using
certificates or a local database).  C is also allowed to accept
the key without verification; however, doing so will render the
protocol insecure against active attacks (but may be desirable for
practical reasons in the short term in many environments).  C then
computes K = f^x mod p, H = hash(V_C || V_S || I_C || I_S || K_S
|| e || f || K), and verifies the signature s on H.
'''
    text = '''R: knows(k, k0)
T: knows(k)
R: fresh(r0)
1. R -> T: r0
T: fresh(r1)
2. T -> R: r1, h(r0 xor r1 xor k)
R: updates k0 := k
R: updates k := h(k)
3. R -> T: h(h(r0 xor r1 xor k) xor k xor r0)
T: updates k := h(k)'''
    text = '''// Protocol: IKEv2's post-quantum extension
// Source: ACSAC'21-A formal analysis of IKEv2's post-quantum extension
// Authors: Stefan-Lukas Gazdag

IKE_SA_INIT:
    Initiator The Initiator chooses a private ephemeral key eI for the DH-exchange, calculates the public ephemeral
    key epI and sends it to the Responder together with a Nonce_i in an IKE_SA_INIT message. The message also contains 
    a list of proposed key exchange methods.
IKE_SA_INIT:
    Responder The Responder also chooses a private ephemeral key eR and uses it to calculate the shared DHkey. Together 
    with Nonce_r and Nonce_i, this shared key is used to derive the session key (called keymat), typically by hashing. 
    The public ephemeral key, Nonce_r, and a selection of key exchange methods from the Initiator's proposal comprise 
    the Responder's IKE_SA_INIT message, which he sends to the Initiator.
IKE_AUTH:
    Initiator Upon receiving the IKE_SA_INIT response, the Initiator also calculates the shared DH key and derives the 
    session key keymat. For authentication, the Initiator signs his own IKE_SA_INIT message with his private static key, 
    thereby also proving that the IKE_SA_INIT message was sent by him. The IKE_AUTH message itself is also signed and 
    sent to the Responder.
IKE_AUTH Responder:
    The Responder verifies the signature with the Initiator's public static key, and proves his own identity by signing 
    the IKE_AUTH response as well as his own IKE_SA_INIT message with his own private static key and sending them to the 
    Initiator.
IKE_AUTH Done:
    As a last step, the Initiator verifies the signature of the Responder; this completes the exchange. Both peers now 
    share a common SA, which can be used for communication with IKEv2 or deriving new so-called Child-SAs for other 
    IPsec protocol.'''
    print(loadText(text))