-- | Common URI schemes, taken from the list of permanent URI schemes
-- | [assigned by IANA](https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml).
module URI.Scheme.Common where

import URI.Scheme (Scheme, unsafeFromString)

-- | Diameter Protocol ([RFC6733](https://tools.ietf.org/html/rfc6733))
aaa :: Scheme
aaa = unsafeFromString "aaa"

-- | Diameter Protocol with Secure ([RFC6733](https://tools.ietf.org/html/rfc6733))
aaas :: Scheme
aaas = unsafeFromString "aaas"

-- | about ([RFC6694](https://tools.ietf.org/html/rfc6694))
about :: Scheme
about = unsafeFromString "about"

-- | application configuration access ([RFC2244](https://tools.ietf.org/html/rfc2244))
acap :: Scheme
acap = unsafeFromString "acap"

-- | acct ([RFC7565](https://tools.ietf.org/html/rfc7565))
acct :: Scheme
acct = unsafeFromString "acct"

-- | Calendar Access Protocol ([RFC4324](https://tools.ietf.org/html/rfc4324))
cap :: Scheme
cap = unsafeFromString "cap"

-- | content identifier ([RFC2392](https://tools.ietf.org/html/rfc2392))
cid :: Scheme
cid = unsafeFromString "cid"

-- | coap ([RFC7252](https://tools.ietf.org/html/rfc7252))
coap :: Scheme
coap = unsafeFromString "coap"

-- | coap+tcp ([RFC8323](https://tools.ietf.org/html/rfc8323))
coaptcp :: Scheme
coaptcp = unsafeFromString "coap+tcp"

-- | coap+ws ([RFC8323](https://tools.ietf.org/html/rfc8323))
coapws :: Scheme
coapws = unsafeFromString "coap+ws"

-- | coaps ([RFC7252](https://tools.ietf.org/html/rfc7252))
coaps :: Scheme
coaps = unsafeFromString "coaps"

-- | coaps+tcp ([RFC8323](https://tools.ietf.org/html/rfc8323))
coapstcp :: Scheme
coapstcp = unsafeFromString "coaps+tcp"

-- | coaps+ws ([RFC8323](https://tools.ietf.org/html/rfc8323))
coapsws :: Scheme
coapsws = unsafeFromString "coaps+ws"

-- | TV-Anytime Content Reference ([RFC4078](https://tools.ietf.org/html/rfc4078))
crid :: Scheme
crid = unsafeFromString "crid"

-- | data ([RFC2397](https://tools.ietf.org/html/rfc2397))
data_ :: Scheme
data_ = unsafeFromString "data"

-- | dav ([RFC4918](https://tools.ietf.org/html/rfc4918))
dav :: Scheme
dav = unsafeFromString "dav"

-- | dictionary service protocol ([RFC2229](https://tools.ietf.org/html/rfc2229))
dict :: Scheme
dict = unsafeFromString "dict"

-- | Domain Name System ([RFC4501](https://tools.ietf.org/html/rfc4501))
dns :: Scheme
dns = unsafeFromString "dns"

-- | example ([RFC7595](https://tools.ietf.org/html/rfc7595))
example :: Scheme
example = unsafeFromString "example"

-- | Host-specific file names ([RFC8089](https://tools.ietf.org/html/rfc8089))
file :: Scheme
file = unsafeFromString "file"

-- | File Transfer Protocol ([RFC1738](https://tools.ietf.org/html/rfc1738))
ftp :: Scheme
ftp = unsafeFromString "ftp"

-- | Geographic Locations ([RFC5870](https://tools.ietf.org/html/rfc5870))
geo :: Scheme
geo = unsafeFromString "geo"

-- | go ([RFC3368](https://tools.ietf.org/html/rfc3368))
go :: Scheme
go = unsafeFromString "go"

-- | The Gopher Protocol ([RFC4266](https://tools.ietf.org/html/rfc4266))
gopher :: Scheme
gopher = unsafeFromString "gopher"

-- | H.323 ([RFC3508](https://tools.ietf.org/html/rfc3508))
h323 :: Scheme
h323 = unsafeFromString "h323"

-- | Hypertext Transfer Protocol ([RFC7230, Section 2.7.1](https://tools.ietf.org/html/rfc7230#section-2.7.1))
http :: Scheme
http = unsafeFromString "http"

-- | Hypertext Transfer Protocol Secure ([RFC7230, Section 2.7.2](https://tools.ietf.org/html/rfc7230#section-2.7.2))
https :: Scheme
https = unsafeFromString "https"

-- | Inter-Asterisk eXchange Version 2 ([RFC5456](https://tools.ietf.org/html/rfc5456))
iax :: Scheme
iax = unsafeFromString "iax"

-- | Internet Content Adaptation Protocol ([RFC3507](https://tools.ietf.org/html/rfc3507))
icap :: Scheme
icap = unsafeFromString "icap"

-- | Instant Messaging ([RFC3860](https://tools.ietf.org/html/rfc3860))
im :: Scheme
im = unsafeFromString "im"

-- | internet message access protocol ([RFC5092](https://tools.ietf.org/html/rfc5092))
imap :: Scheme
imap = unsafeFromString "imap"

-- | registry of public namespaces, which ([RFC4452](https://tools.ietf.org/html/rfc4452))
info :: Scheme
info = unsafeFromString "info"

-- | Internet Printing Protocol ([RFC3510](https://tools.ietf.org/html/rfc3510))
ipp :: Scheme
ipp = unsafeFromString "ipp"

-- | Internet Printing Protocol over ([RFC7472](https://tools.ietf.org/html/rfc7472))
ipps :: Scheme
ipps = unsafeFromString "ipps"

-- | Internet Registry Information ([RFC3981](https://tools.ietf.org/html/rfc3981))
iris :: Scheme
iris = unsafeFromString "iris"

-- | iris.beep ([RFC3983](https://tools.ietf.org/html/rfc3983))
irisbeep :: Scheme
irisbeep = unsafeFromString "iris.beep"

-- | iris.lwz ([RFC4993](https://tools.ietf.org/html/rfc4993))
irislwz :: Scheme
irislwz = unsafeFromString "iris.lwz"

-- | iris.xpc ([RFC4992](https://tools.ietf.org/html/rfc4992))
irisxpc :: Scheme
irisxpc = unsafeFromString "iris.xpc"

-- | iris.xpcs ([RFC4992](https://tools.ietf.org/html/rfc4992))
irisxpcs :: Scheme
irisxpcs = unsafeFromString "iris.xpcs"

-- | Lightweight Directory Access ([RFC4516](https://tools.ietf.org/html/rfc4516))
ldap :: Scheme
ldap = unsafeFromString "ldap"

-- | Electronic mail address ([RFC6068](https://tools.ietf.org/html/rfc6068))
mailto :: Scheme
mailto = unsafeFromString "mailto"

-- | message identifier ([RFC2392](https://tools.ietf.org/html/rfc2392))
mid :: Scheme
mid = unsafeFromString "mid"

-- | Message Session Relay Protocol ([RFC4975](https://tools.ietf.org/html/rfc4975))
msrp :: Scheme
msrp = unsafeFromString "msrp"

-- | Message Session Relay Protocol ([RFC4975](https://tools.ietf.org/html/rfc4975))
msrps :: Scheme
msrps = unsafeFromString "msrps"

-- | Message Tracking Query Protocol ([RFC3887](https://tools.ietf.org/html/rfc3887))
mtqp :: Scheme
mtqp = unsafeFromString "mtqp"

-- | Mailbox Update (MUPDATE) Protocol ([RFC3656](https://tools.ietf.org/html/rfc3656))
mupdate :: Scheme
mupdate = unsafeFromString "mupdate"

-- | USENET news ([RFC5538](https://tools.ietf.org/html/rfc5538))
news :: Scheme
news = unsafeFromString "news"

-- | network file system protocol ([RFC2224](https://tools.ietf.org/html/rfc2224))
nfs :: Scheme
nfs = unsafeFromString "nfs"

-- | ni ([RFC6920](https://tools.ietf.org/html/rfc6920))
ni :: Scheme
ni = unsafeFromString "ni"

-- | nih ([RFC6920](https://tools.ietf.org/html/rfc6920))
nih :: Scheme
nih = unsafeFromString "nih"

-- | USENET news using NNTP access ([RFC5538](https://tools.ietf.org/html/rfc5538))
nntp :: Scheme
nntp = unsafeFromString "nntp"

-- | opaquelocktokent ([RFC4918](https://tools.ietf.org/html/rfc4918))
opaquelocktoken :: Scheme
opaquelocktoken = unsafeFromString "opaquelocktoken"

-- | PKCS#11 ([RFC7512](https://tools.ietf.org/html/rfc7512))
pkcs11 :: Scheme
pkcs11 = unsafeFromString "pkcs11"

-- | Post Office Protocol v3 ([RFC2384](https://tools.ietf.org/html/rfc2384))
pop :: Scheme
pop = unsafeFromString "pop"

-- | Presence ([RFC3859](https://tools.ietf.org/html/rfc3859))
pres :: Scheme
pres = unsafeFromString "pres"

-- | reload ([RFC6940](https://tools.ietf.org/html/rfc6940))
reload :: Scheme
reload = unsafeFromString "reload"

-- | Real-Time Streaming Protocol (RTSP) ([RFC2326](https://tools.ietf.org/html/rfc2326), [RFC7826](https://tools.ietf.org/html/rfc7826))
rtsp :: Scheme
rtsp = unsafeFromString "rtsp"

-- | Real-Time Streaming Protocol (RTSP) ([RFC2326](https://tools.ietf.org/html/rfc2326), [RFC7826](https://tools.ietf.org/html/rfc7826))
rtsps :: Scheme
rtsps = unsafeFromString "rtsps"

-- | Real-Time Streaming Protocol (RTSP) ([RFC2326](https://tools.ietf.org/html/rfc2326))
rtspu :: Scheme
rtspu = unsafeFromString "rtspu"

-- | service location ([RFC2609](https://tools.ietf.org/html/rfc2609))
service :: Scheme
service = unsafeFromString "service"

-- | session ([RFC6787](https://tools.ietf.org/html/rfc6787))
session :: Scheme
session = unsafeFromString "session"

-- | Secure Hypertext Transfer Protocol ([RFC2660](https://tools.ietf.org/html/rfc2660))
shttp :: Scheme
shttp = unsafeFromString "shttp"

-- | ManageSieve Protocol ([RFC5804](https://tools.ietf.org/html/rfc5804))
sieve :: Scheme
sieve = unsafeFromString "sieve"

-- | session initiation protocol ([RFC3261](https://tools.ietf.org/html/rfc3261))
sip :: Scheme
sip = unsafeFromString "sip"

-- | secure session initiation protocol ([RFC3261](https://tools.ietf.org/html/rfc3261))
sips :: Scheme
sips = unsafeFromString "sips"

-- | Short Message Service ([RFC5724](https://tools.ietf.org/html/rfc5724))
sms :: Scheme
sms = unsafeFromString "sms"

-- | Simple Network Management Protocol ([RFC4088](https://tools.ietf.org/html/rfc4088))
snmp :: Scheme
snmp = unsafeFromString "snmp"

-- | soap.beep ([RFC4227](https://tools.ietf.org/html/rfc4227))
soapbeep :: Scheme
soapbeep = unsafeFromString "soap.beep"

-- | soap.beeps ([RFC4227](https://tools.ietf.org/html/rfc4227))
soapbeeps :: Scheme
soapbeeps = unsafeFromString "soap.beeps"

-- | stun ([RFC7064](https://tools.ietf.org/html/rfc7064))
stun :: Scheme
stun = unsafeFromString "stun"

-- | stuns ([RFC7064](https://tools.ietf.org/html/rfc7064))
stuns :: Scheme
stuns = unsafeFromString "stuns"

-- | tag ([RFC4151](https://tools.ietf.org/html/rfc4151))
tag :: Scheme
tag = unsafeFromString "tag"

-- | telephone ([RFC3966](https://tools.ietf.org/html/rfc3966))
tel :: Scheme
tel = unsafeFromString "tel"

-- | Reference to interactive sessions ([RFC4248](https://tools.ietf.org/html/rfc4248))
telnet :: Scheme
telnet = unsafeFromString "telnet"

-- | Trivial File Transfer Protocol ([RFC3617](https://tools.ietf.org/html/rfc3617))
tftp :: Scheme
tftp = unsafeFromString "tftp"

-- | multipart/related relative reference ([RFC2557](https://tools.ietf.org/html/rfc2557))
thismessage :: Scheme
thismessage = unsafeFromString "thismessage"

-- | Transaction Internet Protocol ([RFC2371](https://tools.ietf.org/html/rfc2371))
tip :: Scheme
tip = unsafeFromString "tip"

-- | Interactive 3270 emulation sessions ([RFC6270](https://tools.ietf.org/html/rfc6270))
tn3270 :: Scheme
tn3270 = unsafeFromString "tn3270"

-- | turn ([RFC7065](https://tools.ietf.org/html/rfc7065))
turn :: Scheme
turn = unsafeFromString "turn"

-- | turns ([RFC7065](https://tools.ietf.org/html/rfc7065))
turns :: Scheme
turns = unsafeFromString "turns"

-- | TV Broadcasts ([RFC2838](https://tools.ietf.org/html/rfc2838))
tv :: Scheme
tv = unsafeFromString "tv"

-- | Uniform Resource Names ([RFC8141](https://tools.ietf.org/html/rfc8141))
urn :: Scheme
urn = unsafeFromString "urn"

-- | versatile multimedia interface ([RFC2122](https://tools.ietf.org/html/rfc2122))
vemmi :: Scheme
vemmi = unsafeFromString "vemmi"

-- | Remote Framebuffer Protocol ([RFC7869](https://tools.ietf.org/html/rfc7869))
vnc :: Scheme
vnc = unsafeFromString "vnc"

-- | WebSocket connections ([RFC6455](https://tools.ietf.org/html/rfc6455))
ws :: Scheme
ws = unsafeFromString "ws"

-- | Encrypted WebSocket connections ([RFC6455](https://tools.ietf.org/html/rfc6455))
wss :: Scheme
wss = unsafeFromString "wss"

-- | xcon ([RFC6501](https://tools.ietf.org/html/rfc6501))
xcon :: Scheme
xcon = unsafeFromString "xcon"

-- | xcon-userid ([RFC6501](https://tools.ietf.org/html/rfc6501))
xconuserid :: Scheme
xconuserid = unsafeFromString "xcon-userid"

-- | xmlrpc.beep ([RFC3529](https://tools.ietf.org/html/rfc3529))
xmlrpcbeep :: Scheme
xmlrpcbeep = unsafeFromString "xmlrpc.beep"

-- | xmlrpc.beeps ([RFC3529](https://tools.ietf.org/html/rfc3529))
xmlrpcbeeps :: Scheme
xmlrpcbeeps = unsafeFromString "xmlrpc.beeps"

-- | Extensible Messaging and Presence ([RFC5122](https://tools.ietf.org/html/rfc5122))
xmpp :: Scheme
xmpp = unsafeFromString "xmpp"

-- | Z39.50 Retrieval ([RFC2056](https://tools.ietf.org/html/rfc2056))
z3950r :: Scheme
z3950r = unsafeFromString "z39.50r"

-- | Z39.50 Session ([RFC2056](https://tools.ietf.org/html/rfc2056))
z3950s :: Scheme
z3950s = unsafeFromString "z39.50s"
